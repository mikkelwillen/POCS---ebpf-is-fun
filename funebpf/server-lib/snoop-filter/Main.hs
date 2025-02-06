{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Network.Run.UDP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL (fromStrict)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map.Strict as Map
import Data.Binary.Get (runGet, getInt64le, getWord32le)
import Data.Word (Word32)
import Data.Int (Int64, Int32)
import Control.Concurrent (forkIO)
import Data.Foldable (for_)
import Control.Monad (when)
import System.Posix.Types (Fd(..), CPid(..))
import System.Process (getCurrentPid)
import Foreign.Storable
import Text.Printf

import Ebpf.Asm
import Ebpf.Quote
import Ebpf.Encode (encodeProgram)
import Ebpf.LinuxAPI

import ServerLib

type Key = Word32
type Count = Int64

vUDPhlen = 8

main :: IO ()
main = do
  -- setup map for counting calls to recvfrom
  bpfMap <- newMap 16 BPFMapTypeHash
  defaultMain (pre bpfMap) (post bpfMap)
  where
    post bpfMap _ = printStats bpfMap

pre :: BPFMap Int32 Int64 -> Socket -> IO ()
pre bpfMap sock = do
  setupFilter sock
  setupSnooper bpfMap sock

setupFilter :: Socket -> IO ()
setupFilter sock = do
  let get = Imm 0x474554       -- GET
      put = Imm 0x505554       -- PUT
      del1 = Imm 0x5445     -- TE
      del2 = Imm 0x44454c45 -- DELE
      stop = Imm 0x53544f50    -- STOP
      offset = vUDPhlen
      offset2 = vUDPhlen + 4
      insns = [ebpf| mov r6, r1
                     ldabsw $offset
                     mov r3, r0
                     jeq r3, $del2, +4
                     jeq r3, $stop, +9
                     rsh r3, 8
                     jeq r3, $get, +7
                     jeq r3, $put, +6
                     ldabsw $offset2
                     mov r5, r0
                     rsh r5, 16  ;; NOTE: when does the message end?
                     jeq r5, $del1, +2
                     mov32 r0, 0
                     exit
                     mov32 r0, -1
                     exit
                   |]
      fprog = BPFProgLoad { progType = c_BPF_PROG_TYPE_SOCKET_FILTER
                         , insns = encodeProgram insns
                         , license = s_Dual_MIT_GPL
                         , logLevel = Just 2
                         , kernVersion = Nothing
                         }
  -- Right fd@(Fd progFd) <- bpfProgLoad fprog
  fres <- bpfProgLoad fprog
  case fres of
    Right (Fd progFd) ->
      putStrLn "load filter success" >>
      setSocketOption sock (SockOpt c_SOL_SOCKET c_SO_ATTACH_BPF) ( fromIntegral progFd ) >>
      putStrLn "attach filter success"
    Left e ->
      putStrLn e

setupSnooper :: BPFMap Int32 Int64 -> Socket -> IO ()
setupSnooper bpfMap _sock = do
  CPid cpid <- getCurrentPid
  putStrLn $ "Pid: " ++ show cpid
  _ret <- updateElem bpfMap cpid 0 Any

  -- setup snooper
  let map_lookup_elem = fromIntegral c_BPF_FUNC_map_lookup_elem
      map_update_elem = fromIntegral c_BPF_FUNC_map_update_elem
      get_pid = fromIntegral c_BPF_FUNC_get_current_pid_tgid
      flag = Imm $ fromIntegral c_BPF_ANY
      map_fd = mapToRawFd bpfMap
      pid = Imm $ fromIntegral cpid
      -- offset = Imm 8
      snoop = [ebpf| ;; look only at specific pid
                     call $get_pid
                     mov32 r3, r0
                     mov r2, $pid
                     jeq r3, r2, +2
                     mov r0, 0
                     exit

                     ;; get info from map
                     mov r2, r10
                     add r2, -4
                     stxw [r2], r3
                     lmfd r1, $map_fd  ;; remember this is 2 instructions
                     call $map_lookup_elem

                     ;; update map (counter)
                     ;; get elem value and increment (if NULL then element is set to 1)
                     mov r8, r0
                     jeq r8, 0, +4
                     ldxdw r6, [r8]
                     add r6, 1
                     mov r8, r6
                     jmp +1
                     mov r8, 1 ;; pid has triggered function once

                     ;; pid is key
                     call $get_pid
                     mov32 r3, r0
                     mov r2, r10
                     add r2, -4
                     stxw [r2], r3

                     ;; counter is value
                     mov r3, r10
                     add r3, -16
                     mov r4, r8
                     stxdw [r3], r4

                     ;; set flag, load bpf map, and call elem_update
                     mov r4, $flag
                     lmfd r1, $map_fd
                     call $map_update_elem

                     ;; done
                     mov32 r0, 0
                     exit
                   |]
      sprog = BPFProgLoad { progType = c_BPF_PROG_TYPE_TRACEPOINT
                          , insns = encodeProgram snoop
                          , license = s_Dual_MIT_GPL
                          , logLevel = Just 2
                          , kernVersion = Nothing
                          }
  sres <- bpfProgLoad sprog
  case sres of
    Right progFd ->
      putStrLn "load snooper success" >>
      do res <- bpfProgAttach "syscalls/sys_enter_recvfrom" progFd
         case res of
           Left err -> putStrLn err
           Right _ -> putStrLn "attach snooper success"
    Left e ->
      putStrLn e


printStats :: BPFMap Int32 Int64 -> IO ()
printStats bpfMap = do
  putStrLn "\nPrinting map keys and values:\n"
  bpfMap_mapM_ (\k v -> putStrLn $ "Key: " <> show k <> "\nElem: " <> show v) bpfMap
