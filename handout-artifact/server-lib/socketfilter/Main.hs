{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Network.Socket
import Network.Run.UDP
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Data.Int (Int64)
import System.Posix.Types (Fd(..))

import Ebpf.Asm
import Ebpf.Quote
import Ebpf.Encode (encodeProgram)
import Ebpf.LinuxAPI

import ServerLib

vUDPhlen = 8

verbose = True

main :: IO ()
main = defaultMain pre post
  where
    pre sock = createFilter >>= attach sock
    post _ = pure ()

createFilter :: IO Program
createFilter = do
  -- setup filter
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
                     rsh r5, 16  ;; problem with shifting is we dont know if the message actually ends here
                     jeq r5, $del1, +2
                     mov32 r0, 0
                     exit
                     mov32 r0, -1
                     exit
                   |]
  return insns

attach :: Socket -> Program -> IO ()
attach sock insns = do
  -- putStrLn $ "Program: " <> show insns
  let prog = BPFProgLoad { progType = c_BPF_PROG_TYPE_SOCKET_FILTER
                         , insns = encodeProgram insns
                         , license = s_Dual_MIT_GPL
                         , logLevel = Just 2
                         , kernVersion = Nothing
                         }
  -- Right fd@(Fd progFd) <- bpfProgLoad prog
  fres <- bpfProgLoad prog
  case fres of
    Right (Fd progFd) ->
      -- putStrLn "load filter success" >>
      setSocketOption sock (SockOpt c_SOL_SOCKET c_SO_ATTACH_BPF) ( fromIntegral progFd ) -- >>
      -- putStrLn "attach filter success"
    Left e ->
      putStrLn e
