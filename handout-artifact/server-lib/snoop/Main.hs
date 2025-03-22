{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Network.Run.UDP
import qualified Data.Map.Strict as Map
import Data.Int (Int64, Int32)
import Data.Foldable (for_)
import System.Posix.Types (Fd(..), CPid(..))
import System.Process (getCurrentPid)
import Foreign.Storable

import Ebpf.Asm
import Ebpf.Quote
import Ebpf.Encode (encodeProgram)
import Ebpf.LinuxAPI

import ServerLib

verbose = True
type SnoopMap = BPFMap Int32 Int64

main :: IO ()
main = do
  bpfMap <- newMap 16 BPFMapTypeHash
  defaultMain (pre bpfMap) (post bpfMap)
  where
    pre bpfMap _ = countRecvFrom bpfMap
    post bpfMap _ = printStats bpfMap

countRecvFrom :: SnoopMap -> IO ()
countRecvFrom bpfMap = do
  CPid cpid <- getCurrentPid
  putStrLn $ "Pid: " ++ show cpid
  _ret <- updateElem bpfMap cpid 0 Any
  let map_lookup_elem = fromIntegral c_BPF_FUNC_map_lookup_elem
      map_update_elem = fromIntegral c_BPF_FUNC_map_update_elem
      get_pid = fromIntegral c_BPF_FUNC_get_current_pid_tgid
      flag = Imm $ fromIntegral c_BPF_ANY
      map_fd = mapToRawFd bpfMap
      pid = Imm $ fromIntegral cpid
      -- offset = Imm 8
      insns = [ebpf| ;; look only at specific pid
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
      prog = BPFProgLoad { progType = c_BPF_PROG_TYPE_TRACEPOINT
                         , insns = encodeProgram insns
                         , license = s_Dual_MIT_GPL
                         , logLevel = Just 2
                         , kernVersion = Nothing
                         }
  res <- bpfProgLoad prog
  case res of
    Right progFd ->
      putStrLn "load success" >>
      do res1 <- bpfProgAttach "syscalls/sys_enter_recvfrom" progFd
         case res1 of
           Left err -> putStrLn err
           Right _ -> putStrLn "attach success"
    Left e ->
      putStrLn e


printStats :: SnoopMap -> IO ()
printStats bpfMap = do
  putStrLn "\nPrinting map keys and values:\n"
  bpfMap_mapM_ (\k v -> putStrLn $ "Key: " <> show k <> "\nElem: " <> show v) bpfMap
