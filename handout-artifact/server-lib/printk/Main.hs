{-# LANGUAGE QuasiQuotes #-}

import System.Posix.Types (Fd(..), CPid(..))
import System.Process (getCurrentPid)
import Numeric (readHex, showHex)
import Data.List.Split (chunksOf)
-- import qualified Data.ByteString as BS

import Ebpf.Asm
import Ebpf.Quote
import Ebpf.Encode (encodeProgram)
import Ebpf.LinuxAPI

import ServerLib

main :: IO()
main = defaultMain pre post
  where
    pre _ = printKEbpfFun >>= attach
    post _ = pure ()

stringToHex :: String -> String
stringToHex cs = foldr (showHex . fromEnum) "" cs

toHex :: String -> Int
toHex str = case readHex $ stringToHex str of
  [(msg, _)] -> msg
  _ -> 0

-- stringOnStack : takes a string and writes the appropriate code for placing it on the stack
stringOnStack :: String -> Program
stringOnStack str = let
  chunks = map (fromIntegral . toHex) $ chunksOf 8 $ reverse str
  in foldl (\acc c -> acc <> (insns c)) [] chunks
  where
    insns :: Imm -> Program
    insns c = [ebpf| lddw r2, $c
                     add r3, -8
                     stxdw [r3], r2
                   |]


printKEbpfFun = do
  CPid cpid <- getCurrentPid
  putStrLn $ "Pid: " ++ show cpid
  let get_pid = fromIntegral c_BPF_FUNC_get_current_pid_tgid
      trace_printk = fromIntegral c_BPF_FUNC_trace_printk
      str = "Got a package with protocol %d\n\0"
      fmt_sz = Imm $ fromIntegral $ length str
      val = Imm 0x2a
      pid = Imm $ fromIntegral cpid
      start = [ebpf| ;; look only at specific pid
                     call $get_pid
                     mov32 r3, r0
                     mov r2, $pid
                     jeq r3, r2, +2
                     mov r0, 0
                     exit
                     mov r3, r10 |]
      insns = [ebpf|
                     mov r1, r3
                     mov r2, $fmt_sz
                     mov r3, $val
                     call $trace_printk

                     mov r0, 0
                     exit
                   |]
  return $ start <> stringOnStack str <> insns

attach :: Program -> IO()
attach p = do
  let prog = BPFProgLoad { progType = c_BPF_PROG_TYPE_TRACEPOINT
                         , insns = encodeProgram p
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
