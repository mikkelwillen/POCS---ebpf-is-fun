{-# LANGUAGE QuasiQuotes #-}

import Test.Tasty
import Test.Tasty.HUnit
import System.Posix.Types (Fd (..))
import System.Posix.IO (closeFd)

import Data.Int (Int32, Int64)
import Data.Either (isRight)
import qualified Data.ByteString as B

import Ebpf.LinuxAPI
import Ebpf.Quote
import Ebpf.Encode (encodeProgram)

main = defaultMain tests

tests :: TestTree
tests =
  testGroup "eBPF Map Tests"
  [ testCase "Create a map and close it" $ do
      let attr = BPFMapCreateAttr BPFMapTypeArray 4 8 16
      fd <- bpfMapCreate attr
      closeFd fd

  , testCaseSteps "Create a map, update en elem and read it back" $ \step -> do
      let attr = BPFMapCreateAttr BPFMapTypeArray 4 8 16
      step "Make map"
      fd <- bpfMapCreate attr

      step "Insert a new element"
      ret <- bpfMapUpdateElem fd (2 :: Int32) (424242424242424 :: Int64) Any
      Success @=? ret

      step "Lookup the newly inserted element"
      Just v <- bpfMapLookupElem fd (2 :: Int32)
      (424242424242424 :: Int64) @=? v

      step "Close map"
      closeFd fd

  , testCaseSteps "Load program 'exit 0', and close it" $ \step -> do
      let prog = BPFProgLoad { progType = c_BPF_PROG_TYPE_SOCKET_FILTER
                             , insns = encodeProgram [ebpf| mov r0, 0
                                                            exit |]
                             , license = s_Dual_MIT_GPL
                             , logLevel = Just 2
                             , kernVersion = Nothing
                             }
      step "Load program"
      res <- bpfProgLoad prog
      fd <- case res of
              Right fd -> pure fd
              Left e -> assertFailure e

      step "Close program"
      closeFd fd

  , testCaseSteps "Load program using ldabs, and close it" $ \step -> do
      let prog = BPFProgLoad { progType = c_BPF_PROG_TYPE_SOCKET_FILTER
                             , insns = encodeProgram [ebpf| mov r6, r1
                                                            ldabsw 0
                                                            exit |]
                             , license = s_Dual_MIT_GPL
                             , logLevel = Just 2
                             , kernVersion = Nothing
                             }
      step "Load program"
      res <- bpfProgLoad prog
      fd <- case res of
              Right fd -> pure fd
              Left e -> assertFailure e

      step "Close program"
      closeFd fd

  , testCaseSteps "Load program using ldind, and close it" $ \step -> do
      let prog = BPFProgLoad { progType = c_BPF_PROG_TYPE_SOCKET_FILTER
                             , insns = encodeProgram [ebpf| mov r6, r1
                                                            mov r2, 0x2a
                                                            ldindw r2, 2
                                                            exit |]
                             , license = s_Dual_MIT_GPL
                             , logLevel = Just 2
                             , kernVersion = Nothing
                             }
      step "Load program"
      res <- bpfProgLoad prog
      fd <- case res of
              Right fd -> pure fd
              Left e -> assertFailure e

      step "Close program"
      closeFd fd

  , testCaseSteps "Create map and load program" $ \step -> do
      let attr = BPFMapCreateAttr BPFMapTypeArray 4 8 16
      step "Make map"
      fd <- bpfMapCreate attr

      step "Load program"
      let map_fd = case fd of
            Fd i -> fromIntegral i
          prog = BPFProgLoad { progType = c_BPF_PROG_TYPE_SOCKET_FILTER
                             , insns = encodeProgram [ebpf| lmfd r1, $map_fd
                                                            mov r0, 0
                                                            exit |]
                             , license = s_Dual_MIT_GPL
                             , logLevel = Just 2
                             , kernVersion = Nothing
                             }
      res <- bpfProgLoad prog
      fd <- case res of
              Right fd -> pure fd
              Left e -> assertFailure e

      step "Close program"
      closeFd fd

  , testCaseSteps "Load program calling bpf-helper" $ \step -> do
      let attr = BPFMapCreateAttr BPFMapTypeArray 4 8 16
      step "Make map"
      fd <- bpfMapCreate attr

      step "Insert a new element"
      ret <- bpfMapUpdateElem fd (2 :: Int32) (424242424242424 :: Int64) Any
      Success @=? ret

      step "Load program"
      let bpf_cmd = fromIntegral c_BPF_FUNC_map_lookup_elem
          map_fd = case fd of
            Fd i -> fromIntegral i
          prog = BPFProgLoad { progType = c_BPF_PROG_TYPE_SOCKET_FILTER
                             , insns = encodeProgram [ebpf| lmfd r1, $map_fd
                                                            mov r2, r10
                                                            add r2, -4
                                                            stw [r2], 2
                                                            call $bpf_cmd
                                                            mov r0, 0
                                                            exit |]
                             , license = s_Dual_MIT_GPL
                             , logLevel = Just 2
                             , kernVersion = Nothing
                             }
      res <- bpfProgLoad prog
      fd <- case res of
              Right fd -> pure fd
              Left e -> assertFailure e

      -- trigger program???

      step "Close program"
      closeFd fd

  ]
