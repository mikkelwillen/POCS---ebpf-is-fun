{-# LANGUAGE CApiFFI, ExplicitForAll,
             GeneralisedNewtypeDeriving, LambdaCase,
             NamedFieldPuns, DuplicateRecordFields, OverloadedRecordDot,
             ScopedTypeVariables #-}
module Ebpf.LinuxAPI
  ( BpfProgFd
  , BPFMapType(..)
  , BPFMapCreateAttr(..)
  , bpfMapCreate
  , BPFUpdateFlag(..)
  , BPFUpdateStatus(..)
  , bpfMapLookupElem
  , bpfMapUpdateElem
  , bpfMapGetNextKey
  , GetNextKeyAttr(..)
  , s_GPL, s_Dual_MIT_GPL, s_Dual_BSD_GPL, s_Dual_MPL_GPL, s_Proprietary
  , c_BPF_PROG_TYPE_SOCKET_FILTER, c_BPF_PROG_TYPE_TRACEPOINT, c_BPF_PROG_TYPE_KPROBE
  , c_SOL_SOCKET, c_SO_ATTACH_BPF
  , c_BPF_ANY
  , c_BPF_FUNC_map_lookup_elem, c_BPF_FUNC_map_update_elem, c_BPF_FUNC_map_delete_elem
  , c_BPF_FUNC_get_current_pid_tgid, c_BPF_FUNC_trace_printk
  , BPFProgLoad(..)
  , bpfProgLoad
  , bpfProgAttach
  , BPFMap
  , mapToFd
  , mapToRawFd
  , newMap
  , lookupElem
  , updateElem
  , deleteElem
  , getNextKey
  , bpfMap_foldM
  , bpfMap_mapM_
  )
where

import System.Posix.Types (Fd(..))
-- import Network.Socket.IOCtl
-- import System.Posix.IOCtl (ioctl_)
import Foreign.Marshal.Alloc (alloca, mallocBytes)
import Foreign.C.Types
import Foreign.C.String (CString, CStringLen, withCString, peekCAString, newCString)
import Foreign.C.Error (getErrno, Errno(..), throwErrno)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Data.Word (Word32)
import Data.Int (Int64, Int32)


type C_u32 = CUInt
type C_u64 = CULong
type C_aligned_u64 = CULLong
-- type C_uchar = CUChar

type BpfProgFd = Fd

{-
Note: We use `capi` for imports of system libraries and kernel API as
      it is not under our control.

      See <https://www.haskell.org/ghc/blog/20210709-capi-usage.html>
-}

-- Import the syscall function
foreign import capi unsafe "unistd.h syscall" c_syscall :: CLong -> CInt -> Ptr () -> CInt -> IO CLong
foreign import capi unsafe "unistd.h syscall" c_syscall2 :: CLong -> Ptr () -> CInt -> CInt -> CInt -> CLong -> IO CLong
foreign import capi unsafe "unistd.h syscall" c_syscall3 :: CLong -> CInt -> CULong -> CInt -> IO CLong

-- Use the system defined value for __NR_bpf (different between x86_64 and arm64)
foreign import capi "sys/syscall.h value __NR_bpf" c__NR_bpf :: CLong
foreign import capi "sys/syscall.h value __NR_ioctl" c__NR_ioctl :: CLong
foreign import capi "sys/syscall.h value __NR_perf_event_open" c__NR_perf_event_open :: CLong

foreign import capi "linux/perf_event.h value PERF_EVENT_IOC_ENABLE" c__PERF_EVENT_IOC_ENABLE :: CULong
foreign import capi "linux/perf_event.h value PERF_EVENT_IOC_SET_BPF" c__PERF_EVENT_IOC_SET_BPF :: CULong
foreign import capi "linux/perf_event.h value PERF_TYPE_TRACEPOINT" c__PERF_TYPE_TRACEPOINT :: CUInt
foreign import capi "linux/perf_event.h value PERF_SAMPLE_RAW" c__PERF_SAMPLE_RAW :: CULong

bpf_ :: CInt -> Ptr () -> CInt -> IO CLong
bpf_ cmd attrs sz = c_syscall c__NR_bpf cmd attrs sz

ioctl_ :: CInt -> CULong -> CInt -> IO CLong
ioctl_ fd request arg = c_syscall3 c__NR_ioctl fd request arg

--NOTE: also implement this for perf_event_open
-- (struct perf_event_attr *attr, pid_t pid, int cpu, int group_fd, unsigned long flags)
perf_event_open_ :: Ptr () -> CInt -> CInt -> CInt -> CLong -> IO CLong
perf_event_open_ attr pid cpu group_fd flags =
  c_syscall2 c__NR_perf_event_open attr pid cpu group_fd flags

foreign import capi "linux/bpf.h value BPF_MAP_CREATE" c_BPF_MAP_CREATE :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_LOOKUP_ELEM" c_BPF_MAP_LOOKUP_ELEM :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_UPDATE_ELEM" c_BPF_MAP_UPDATE_ELEM :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_DELETE_ELEM" c_BPF_MAP_DELETE_ELEM :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_GET_NEXT_KEY" c_BPF_MAP_GET_NEXT_KEY :: CInt
foreign import capi "linux/bpf.h value BPF_PROG_LOAD" c_BPF_PROG_LOAD :: CInt
foreign import capi "linux/bpf.h value BPF_OBJ_PIN" c_BPF_OBJ_PIN :: CInt
foreign import capi "linux/bpf.h value BPF_OBJ_GET" c_BPF_OBJ_GET :: CInt
foreign import capi "linux/bpf.h value BPF_PROG_ATTACH" c_BPF_PROG_ATTACH :: CInt
foreign import capi "linux/bpf.h value BPF_PROG_DETACH" c_BPF_PROG_DETACH :: CInt
foreign import capi "linux/bpf.h value BPF_PROG_TEST_RUN" c_BPF_PROG_TEST_RUN :: CInt
foreign import capi "linux/bpf.h value BPF_PROG_GET_NEXT_ID" c_BPF_PROG_GET_NEXT_ID :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_GET_NEXT_ID" c_BPF_MAP_GET_NEXT_ID :: CInt
foreign import capi "linux/bpf.h value BPF_PROG_GET_FD_BY_ID" c_BPF_PROG_GET_FD_BY_ID :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_GET_FD_BY_ID" c_BPF_MAP_GET_FD_BY_ID :: CInt
foreign import capi "linux/bpf.h value BPF_OBJ_GET_INFO_BY_FD" c_BPF_OBJ_GET_INFO_BY_FD :: CInt
foreign import capi "linux/bpf.h value BPF_PROG_QUERY" c_BPF_PROG_QUERY :: CInt
foreign import capi "linux/bpf.h value BPF_RAW_TRACEPOINT_OPEN" c_BPF_RAW_TRACEPOINT_OPEN :: CInt
foreign import capi "linux/bpf.h value BPF_BTF_LOAD" c_BPF_BTF_LOAD :: CInt
foreign import capi "linux/bpf.h value BPF_BTF_GET_FD_BY_ID" c_BPF_BTF_GET_FD_BY_ID :: CInt
foreign import capi "linux/bpf.h value BPF_TASK_FD_QUERY" c_BPF_TASK_FD_QUERY :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_LOOKUP_AND_DELETE_ELEM" c_BPF_MAP_LOOKUP_AND_DELETE_ELEM :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_FREEZE" c_BPF_MAP_FREEZE :: CInt
foreign import capi "linux/bpf.h value BPF_BTF_GET_NEXT_ID" c_BPF_BTF_GET_NEXT_ID :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_LOOKUP_BATCH" c_BPF_MAP_LOOKUP_BATCH :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_LOOKUP_AND_DELETE_BATCH" c_BPF_MAP_LOOKUP_AND_DELETE_BATCH :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_UPDATE_BATCH" c_BPF_MAP_UPDATE_BATCH :: CInt
foreign import capi "linux/bpf.h value BPF_MAP_DELETE_BATCH" c_BPF_MAP_DELETE_BATCH :: CInt
foreign import capi "linux/bpf.h value BPF_LINK_CREATE" c_BPF_LINK_CREATE :: CInt
foreign import capi "linux/bpf.h value BPF_LINK_UPDATE" c_BPF_LINK_UPDATE :: CInt
foreign import capi "linux/bpf.h value BPF_LINK_GET_FD_BY_ID" c_BPF_LINK_GET_FD_BY_ID :: CInt
foreign import capi "linux/bpf.h value BPF_LINK_GET_NEXT_ID" c_BPF_LINK_GET_NEXT_ID :: CInt
foreign import capi "linux/bpf.h value BPF_ENABLE_STATS" c_BPF_ENABLE_STATS :: CInt
foreign import capi "linux/bpf.h value BPF_ITER_CREATE" c_BPF_ITER_CREATE :: CInt
foreign import capi "linux/bpf.h value BPF_LINK_DETACH" c_BPF_LINK_DETACH :: CInt
foreign import capi "linux/bpf.h value BPF_PROG_BIND_MAP" c_BPF_PROG_BIND_MAP :: CInt
c_BPF_PROG_RUN = c_BPF_PROG_TEST_RUN

foreign import capi "linux/bpf.h value BPF_ANY" c_BPF_ANY :: CInt
foreign import capi "linux/bpf.h value BPF_NOEXIST" c_BPF_NOEXIST :: CInt
foreign import capi "linux/bpf.h value BPF_EXIST" c_BPF_EXIST :: CInt
foreign import capi "errno.h value EINVAL" c_EINVAL :: CInt
foreign import capi "errno.h value EPERM" c_EPERM :: CInt
foreign import capi "errno.h value ENOMEM" c_ENOMEM :: CInt
foreign import capi "errno.h value E2BIG" c_E2BIG :: CInt
foreign import capi "errno.h value EEXIST" c_EEXIST :: CInt
foreign import capi "errno.h value ENOENT" c_ENOENT :: CInt

foreign import capi "linux/bpf.h value BPF_PROG_TYPE_UNSPEC" c_BPF_PROG_TYPE_UNSPEC :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_SOCKET_FILTER" c_BPF_PROG_TYPE_SOCKET_FILTER :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_KPROBE" c_BPF_PROG_TYPE_KPROBE :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_SCHED_CLS" c_BPF_PROG_TYPE_SCHED_CLS :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_SCHED_ACT" c_BPF_PROG_TYPE_SCHED_ACT :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_TRACEPOINT" c_BPF_PROG_TYPE_TRACEPOINT :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_XDP" c_BPF_PROG_TYPE_XDP :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_PERF_EVENT" c_BPF_PROG_TYPE_PERF_EVENT :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_CGROUP_SKB" c_BPF_PROG_TYPE_CGROUP_SKB :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_CGROUP_SOCK" c_BPF_PROG_TYPE_CGROUP_SOCK :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_LWT_IN" c_BPF_PROG_TYPE_LWT_IN :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_LWT_OUT" c_BPF_PROG_TYPE_LWT_OUT :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_LWT_XMIT" c_BPF_PROG_TYPE_LWT_XMIT :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_SOCK_OPS" c_BPF_PROG_TYPE_SOCK_OPS :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_SK_SKB" c_BPF_PROG_TYPE_SK_SKB :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_CGROUP_DEVICE" c_BPF_PROG_TYPE_CGROUP_DEVICE :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_SK_MSG" c_BPF_PROG_TYPE_SK_MSG :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_RAW_TRACEPOINT" c_BPF_PROG_TYPE_RAW_TRACEPOINT :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_CGROUP_SOCK_ADDR" c_BPF_PROG_TYPE_CGROUP_SOCK_ADDR :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_LWT_SEG6LOCAL" c_BPF_PROG_TYPE_LWT_SEG6LOCAL :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_LIRC_MODE2" c_BPF_PROG_TYPE_LIRC_MODE2 :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_SK_REUSEPORT" c_BPF_PROG_TYPE_SK_REUSEPORT :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_FLOW_DISSECTOR" c_BPF_PROG_TYPE_FLOW_DISSECTOR :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_CGROUP_SYSCTL" c_BPF_PROG_TYPE_CGROUP_SYSCTL :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_RAW_TRACEPOINT_WRITABLE" c_BPF_PROG_TYPE_RAW_TRACEPOINT_WRITABLE :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_CGROUP_SOCKOPT" c_BPF_PROG_TYPE_CGROUP_SOCKOPT :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_TRACING" c_BPF_PROG_TYPE_TRACING :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_STRUCT_OPS" c_BPF_PROG_TYPE_STRUCT_OPS :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_EXT" c_BPF_PROG_TYPE_EXT :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_LSM" c_BPF_PROG_TYPE_LSM :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_SK_LOOKUP" c_BPF_PROG_TYPE_SK_LOOKUP :: CUInt
foreign import capi "linux/bpf.h value BPF_PROG_TYPE_SYSCALL" c_BPF_PROG_TYPE_SYSCALL :: CUInt
-- foreign import capi "linux/bpf.h value BPF_PROG_TYPE_NETFILTER" c_BPF_PROG_TYPE_NETFILTER :: CUInt

foreign import capi "sys/socket.h value SOL_SOCKET" c_SOL_SOCKET :: CInt
foreign import capi "sys/socket.h value SO_ATTACH_BPF" c_SO_ATTACH_BPF :: CInt

foreign import capi "linux/bpf.h value BPF_FUNC_map_lookup_elem" c_BPF_FUNC_map_lookup_elem :: CInt
foreign import capi "linux/bpf.h value BPF_FUNC_map_update_elem" c_BPF_FUNC_map_update_elem :: CInt
foreign import capi "linux/bpf.h value BPF_FUNC_map_delete_elem" c_BPF_FUNC_map_delete_elem :: CInt
foreign import capi "linux/bpf.h value BPF_FUNC_get_current_pid_tgid" c_BPF_FUNC_get_current_pid_tgid :: CInt
foreign import capi "linux/bpf.h value BPF_FUNC_trace_printk" c_BPF_FUNC_trace_printk :: CInt



data BPFMapType = BPFMapTypeUnspec
                | BPFMapTypeHash
                | BPFMapTypeArray
                | BPFMapTypeProgArray
                | BPFMapTypePerfEventArray
                | BPFMapTypeRaw CUInt
  deriving (Show, Eq)

mapTypeToC :: BPFMapType -> CUInt
mapTypeToC = \case
    BPFMapTypeUnspec -> 0
    BPFMapTypeHash -> 1
    BPFMapTypeArray -> 2
    BPFMapTypeProgArray -> 3
    BPFMapTypePerfEventArray -> 4
    BPFMapTypeRaw n -> n

mapTypeFromC :: CUInt -> BPFMapType
mapTypeFromC = \case
    0 -> BPFMapTypeUnspec
    1 -> BPFMapTypeHash
    2 -> BPFMapTypeArray
    3 -> BPFMapTypeProgArray
    4 -> BPFMapTypePerfEventArray
    n -> BPFMapTypeRaw n


-- Haskell representation of the first struct variant in the union bpf_attr
data BPFMapCreateAttr = BPFMapCreateAttr
  { mapType :: BPFMapType
  , keySize :: C_u32
  , valueSize :: C_u32
  , maxEntries :: C_u32
  }

instance Storable BPFMapCreateAttr where
  sizeOf _ = 16 -- Sum of four C_u32 fields
  alignment _ = 8

  peek ptr = do
    mapType <- mapTypeFromC <$> peekByteOff ptr 0
    keySize <- peekByteOff ptr 4
    valueSize <- peekByteOff ptr 8
    maxEntries <- peekByteOff ptr 12
    pure BPFMapCreateAttr{ mapType, keySize, valueSize, maxEntries }

  poke ptr (BPFMapCreateAttr{ mapType, keySize, valueSize, maxEntries }) = do
    pokeByteOff ptr 0 $ mapTypeToC mapType
    pokeByteOff ptr 4 keySize
    pokeByteOff ptr 8 valueSize
    pokeByteOff ptr 12 maxEntries


bpfMapCreate :: BPFMapCreateAttr -> IO Fd
bpfMapCreate attr = alloca $ \attrPtr -> do
  poke attrPtr attr
  fd <- bpf_ c_BPF_MAP_CREATE (castPtr attrPtr) (fromIntegral $ sizeOf attr)
  if fd < 0
    then error "bpfMapCreate failed"
    else return $ Fd $ fromIntegral fd

newtype BPFMap key value = BPFMap Fd
  deriving (Show)

-- Proxy types for dummy keys and values
newMap :: forall k v . (Storable k, Storable v) => Word32 -> BPFMapType -> IO (BPFMap k v)
newMap maxEntries mapType = BPFMap <$> bpfMapCreate attr
  where
    attr = BPFMapCreateAttr { mapType
                            , keySize = fromIntegral $ sizeOf (undefined :: k)
                            , valueSize = fromIntegral $ sizeOf (undefined :: v)
                            , maxEntries = fromIntegral maxEntries
                            }

mapToFd :: BPFMap key value -> Fd
mapToFd (BPFMap fd) = fd

mapToRawFd :: BPFMap key value -> Int64  -- Fixme: Int64 is wrong type, but used to match ebpf-tools
mapToRawFd (BPFMap (Fd fd)) = fromIntegral fd

-- Haskell representation of the second struct variant in the union bpf_attr
data BPFMapElem = BPFMapElem
  { mapFd :: Fd
  , key   :: Ptr()  -- 'key' and 'value' are actually pointers
  , value :: Ptr()  -- 'value' and 'next_key' share the same space in the union.
  , flags :: C_u64
  } deriving (Eq, Show)

instance Storable BPFMapElem where
  sizeOf _ = 32  -- As calculated based on the C struct layout
  alignment _ = 8

  peek ptr = do
    mapFd <- Fd <$> peekByteOff ptr 0
    _pad  <- peekByteOff ptr 4 :: IO CUInt  -- Padding to make `key` aligned
    key   <- peekByteOff ptr 8
    value <- peekByteOff ptr 16
    flags <- peekByteOff ptr 24
    return BPFMapElem{ mapFd, key, value, flags }

    -- Implement poke
  poke ptr BPFMapElem{ mapFd = Fd fd, key, value, flags } = do
    pokeByteOff ptr 0 fd
    pokeByteOff ptr 4 (0 :: CUInt)  -- Write padding explicitly
    pokeByteOff ptr 8 key
    pokeByteOff ptr 16 value
    pokeByteOff ptr 24 flags


bpfMapLookupElem :: (Storable key, Storable value) => Fd -> key -> IO (Maybe value)
bpfMapLookupElem mapFd key =
  alloca $ \retPtr -> alloca $ \keyPtr -> alloca $ \attrPtr -> do
  poke keyPtr key
  let attr = BPFMapElem { mapFd, key = castPtr keyPtr, value = castPtr retPtr, flags = 0 }
  poke attrPtr attr
  res <- bpf_ c_BPF_MAP_LOOKUP_ELEM (castPtr attrPtr) $ fromIntegral $ sizeOf attr
  if res == 0
    then Just <$> peek retPtr
    else pure Nothing

lookupElem :: (Storable key, Storable value) => BPFMap key value -> key -> IO (Maybe value)
lookupElem bpfMap key = bpfMapLookupElem (mapToFd bpfMap) key

data BPFUpdateFlag = Any | NoExist | Exist deriving (Eq, Show)
data BPFUpdateStatus = Success |
                       Errno_EINVAL |
                       Errno_EPERM |
                       Errno_ENOMEM |
                       Errno_E2BIG |
                       Errno_EEXIST |
                       Errno_ENOENT
  deriving (Eq, Show)

bpfMapUpdateElem :: (Storable key, Storable value) => Fd -> key -> value -> BPFUpdateFlag ->
                    IO BPFUpdateStatus
bpfMapUpdateElem mapFd key value flag =
  alloca $ \valPtr -> alloca $ \keyPtr -> alloca $ \attrPtr -> do
  poke keyPtr key
  poke valPtr value
  let flags = fromIntegral $ case flag of Any -> c_BPF_ANY; NoExist -> c_BPF_NOEXIST; Exist -> c_BPF_EXIST
      attr = BPFMapElem { mapFd, key = castPtr keyPtr, value = castPtr valPtr, flags }
  poke attrPtr attr
  res <- bpf_ c_BPF_MAP_UPDATE_ELEM (castPtr attrPtr) $ fromIntegral $ sizeOf attr
  if res == 0
    then pure Success
    else do Errno err <- getErrno
            case err of
              _ | err == c_EINVAL -> pure Errno_EINVAL
              _ | err == c_EPERM  -> pure Errno_EPERM
              _ | err == c_ENOMEM -> pure Errno_ENOMEM
              _ | err == c_E2BIG  -> pure Errno_E2BIG
              _ | err == c_EEXIST -> pure Errno_EEXIST
              _ | err == c_ENOENT -> pure Errno_ENOENT
              _ -> throwErrno $ "bpfMapUpdateElem failed with an unexpected error"

updateElem :: (Storable key, Storable value) => BPFMap key value -> key -> value -> BPFUpdateFlag ->
                    IO BPFUpdateStatus
updateElem bpfMap = bpfMapUpdateElem $ mapToFd bpfMap

bpfMapDeleteElem :: (Storable key) => Fd -> key -> IO BPFUpdateStatus
bpfMapDeleteElem mapFd key =
  alloca $ \keyPtr -> alloca $ \attrPtr -> do
  poke keyPtr key
  let attr = BPFMapElem { mapFd, key = castPtr keyPtr, value = nullPtr, flags = 0}
  poke attrPtr attr
  res <- bpf_ c_BPF_MAP_DELETE_ELEM (castPtr attrPtr) $ fromIntegral $ sizeOf attr
  if res == 0
    then pure Success
    else do Errno err <- getErrno
            case err of
              _ | err == c_EINVAL -> pure Errno_EINVAL
              _ | err == c_EPERM  -> pure Errno_EPERM
              _ | err == c_ENOMEM -> pure Errno_ENOMEM
              _ | err == c_E2BIG  -> pure Errno_E2BIG
              _ | err == c_EEXIST -> pure Errno_EEXIST
              _ | err == c_ENOENT -> pure Errno_ENOENT
              _ -> throwErrno $ "bpfMapDeleteElem failed with an unexpected error"

deleteElem :: (Storable key, Storable value) => BPFMap key value -> key -> IO BPFUpdateStatus
deleteElem bpfMap = bpfMapDeleteElem $ mapToFd bpfMap

-- Haskell representation of the third struct variant in the union bpf_attr
data BPFProgLoad_ = BPFProgLoad_
  { progType    :: CUInt
  , insnCnt     :: CUInt
  , insns       :: Ptr CChar  -- Representing as pointer to instructions
  , license     :: CString
  , logLevel    :: CUInt
  , logSize     :: CUInt
  , logBuf      :: Ptr CChar
  , kernVersion :: CUInt
  } deriving (Eq, Show)

instance Storable BPFProgLoad_ where
  sizeOf _ = 48
  alignment _ = 8

  peek ptr = do
    progType     <- peekByteOff ptr 0
    insnCnt      <- peekByteOff ptr 4
    insns        <- peekByteOff ptr 8
    license      <- peekByteOff ptr 16
    logLevel     <- peekByteOff ptr 24
    logSize      <- peekByteOff ptr 28
    logBuf       <- peekByteOff ptr 32
    kernVersion  <- peekByteOff ptr 40
    return BPFProgLoad_{ progType, insnCnt, insns, license, logLevel, logSize, logBuf, kernVersion }

  poke ptr BPFProgLoad_{progType,insnCnt,insns,license,logLevel,logSize,logBuf,kernVersion} = do
    pokeByteOff ptr 0  progType
    pokeByteOff ptr 4  insnCnt
    pokeByteOff ptr 8  insns
    pokeByteOff ptr 16 license
    pokeByteOff ptr 24 logLevel
    pokeByteOff ptr 28 logSize
    pokeByteOff ptr 32 logBuf
    pokeByteOff ptr 40 kernVersion


-- | Valid license strings, see <https://docs.kernel.org/process/license-rules.html>
s_GPL, s_Dual_MIT_GPL, s_Dual_BSD_GPL, s_Dual_MPL_GPL, s_Proprietary :: CString

s_GPL = unsafePerformIO $ newCString "GPL"
s_Dual_MIT_GPL = unsafePerformIO $ newCString "Dual MIT/GPL"
s_Dual_BSD_GPL = unsafePerformIO $ newCString "Dual BSD/GPL"
s_Dual_MPL_GPL = unsafePerformIO $ newCString "Dual MPL/GPL"
s_Proprietary = unsafePerformIO $ newCString "Proprietary"

data BPFProgLoad = BPFProgLoad
  { progType    :: CUInt
  , insns       :: B.ByteString
  , license     :: CString
  , logLevel    :: Maybe Int  -- maybe make a pair someday
  , kernVersion :: Maybe CUInt
  } deriving (Eq, Show)

bpfProgLoad :: BPFProgLoad -> IO (Either String BpfProgFd)
bpfProgLoad pl = BU.unsafeUseAsCStringLen pl.insns $ \(insPtr, insSize) -> alloca $ \attrPtr -> do
  (logLevel, logSize, logBuf) <- case pl.logLevel of
                                   Just n | n > 0 -> do let sz = 50000 * n
                                                        buf <- mallocBytes sz
                                                        pure (fromIntegral n, fromIntegral sz, buf)
                                   _ -> pure (0, 0, nullPtr)
  let attr = BPFProgLoad_ { progType = pl.progType
                          , insnCnt = fromIntegral $ insSize `div` 8
                          , insns = insPtr
                          , license = pl.license
                          , logLevel
                          , logSize
                          , logBuf
                          , kernVersion = maybe 0 id $ pl.kernVersion
                          }
  poke attrPtr attr
  res <- bpf_ c_BPF_PROG_LOAD (castPtr attrPtr) $ fromIntegral $ sizeOf attr
  if res > 0
    then pure $ Right $ Fd $ fromIntegral res
    else if logSize > 0
         then do whatWentWrong <- peekCAString logBuf
                 pure $ Left whatWentWrong
         else pure $ Left ""


-- bpfProgAttach, maybe a function for each type of prog?
-- function for finding the id of a probe/event
-- we need:
--  1. getId
--  2. sys_perf_event_open (opening the event, in a context where bpf prog can be attached)
--  3. ioctl (attaching a prog in fd to an event in efd, with a BPF option)

-- does this need to be the full size?
data PerfEventAttr = PerfEventAttr
  { eventType :: C_u32
  , config :: C_u64
  , samplePeriod :: C_u64
  , sampleType :: C_u64
  , wakeupEvents :: C_u32
  } deriving (Eq, Show)

instance Storable PerfEventAttr where
  sizeOf _ = 128 -- 16 * 8
  alignment _ = 8

  peek ptr = do
    eventType     <- peekByteOff ptr 0
    config        <- peekByteOff ptr 8
    samplePeriod  <- peekByteOff ptr 16  -- samplePeriodOrFreq
    sampleType    <- peekByteOff ptr 24
    wakeupEvents  <- peekByteOff ptr 48
    return PerfEventAttr { eventType, config, samplePeriod, sampleType, wakeupEvents }

  poke ptr PerfEventAttr { eventType, config, samplePeriod, sampleType, wakeupEvents } = do
    pokeByteOff ptr 0  eventType
    pokeByteOff ptr 8  config
    pokeByteOff ptr 16 samplePeriod
    pokeByteOff ptr 24 sampleType
    pokeByteOff ptr 48 wakeupEvents

type Error = String
type Event = String

data EventType = TracePoint | KProbe
  deriving (Eq, Show)

bpfProgAttach :: Event -> BpfProgFd -> IO (Either Error ())
bpfProgAttach event (Fd pfd) = do
  eid <- getId event
  case eid of
    Right id -> do
      res <- makePerfEvent id
      case res of
        Right (Fd efd) -> enableAndAttach efd pfd
        Left err -> pure $ Left err
    Left err -> pure $ Left err

getId :: FilePath -> IO (Either Error Int)
getId event = do
  let str = "/sys/kernel/debug/tracing/" ++ "events/" ++ event ++ "/id"
  res <- readFile str
  if length res <= 0
    then pure $ Left "error: could not read file"
    else pure $ Right $ read res

makePerfEvent :: Int -> IO (Either String Fd)
makePerfEvent id = alloca $ \ attrPtr -> do
  let attr = PerfEventAttr { eventType = c__PERF_TYPE_TRACEPOINT
                           , config = fromIntegral id
                           , samplePeriod = 1
                           , sampleType = c__PERF_SAMPLE_RAW
                           , wakeupEvents = 1
                           }
  poke attrPtr attr
  res <- perf_event_open_ (castPtr attrPtr) (-1) 0 (-1) 0  -- attr pid cpu group_fd ?
  if res > 0
    then pure $ Right $ Fd $ fromIntegral res
    else pure $ Left "could not make perf event"

enableAndAttach :: CInt -> CInt -> IO (Either Error ())
enableAndAttach efd pfd = do
  r1 <- ioctl_ efd c__PERF_EVENT_IOC_ENABLE 0
  r2 <- ioctl_ efd c__PERF_EVENT_IOC_SET_BPF pfd
  case (r1 < 0, r2 < 0) of
    (True, _) -> pure $ Left "Could not enable perf event"
    (_, True) -> pure $ Left "Could not set bpf perf event"
    _ -> pure $ Right ()


data GetNextKeyAttr = GetNextKeyAttr
  { mapFd   :: Fd
  , key     :: Ptr ()
  , nextKey :: Ptr ()
  , flags   :: C_u64
  } deriving (Eq, Show)

instance Storable GetNextKeyAttr where
  sizeOf _ = 32 -- 1*4 + 3*8
  alignment _ = 8

  peek ptr = do
    mapFd         <- Fd <$> peekByteOff ptr 0
    _pad          <- peekByteOff ptr 4 :: IO CUInt  -- Padding to make `key` aligned
    key           <- peekByteOff ptr 8
    nextKey       <- peekByteOff ptr 16  -- samplePeriodOrFreq
    flags         <- peekByteOff ptr 24
    return GetNextKeyAttr { mapFd, key, nextKey, flags }

  poke ptr GetNextKeyAttr {mapFd = Fd fd, key, nextKey, flags } = do
    pokeByteOff ptr 0  fd
    pokeByteOff ptr 4 (0 :: CUInt)  -- Write padding explicitly
    pokeByteOff ptr 8  key
    pokeByteOff ptr 16 nextKey
    pokeByteOff ptr 24 flags

bpfMapGetNextHelper :: (Storable key) => Fd -> Ptr key -> IO (Maybe key)
bpfMapGetNextHelper mapFd keyPtr =
  alloca $ \retPtr -> alloca $ \attrPtr -> do
  let attr = GetNextKeyAttr { mapFd, key = castPtr keyPtr, nextKey = castPtr retPtr, flags = 0 }
  poke attrPtr attr
  res <- bpf_ c_BPF_MAP_GET_NEXT_KEY (castPtr attrPtr) $ fromIntegral $ sizeOf attr
  if res == 0
    then Just <$> peek retPtr
    else pure Nothing

bpfMapGetFirstKey :: (Storable key) => Fd -> IO (Maybe key)
bpfMapGetFirstKey mapFd = bpfMapGetNextHelper mapFd nullPtr

bpfMapGetNextKey :: (Storable key) => Fd -> key -> IO (Maybe key)
bpfMapGetNextKey mapFd key = alloca $ \keyPtr -> do
  poke keyPtr key
  bpfMapGetNextHelper mapFd keyPtr

getFirstKey :: (Storable key, Storable value) => BPFMap key value -> IO (Maybe key)
getFirstKey (BPFMap fd) = bpfMapGetFirstKey fd

getNextKey :: (Storable key, Storable value) => BPFMap key value -> key -> IO (Maybe key)
getNextKey (BPFMap fd) = bpfMapGetNextKey fd

bpfMap_foldM :: (Storable key, Storable value) => (a -> key -> value -> IO a) ->
                 a -> BPFMap key value -> IO a
bpfMap_foldM f acc bpfMap = do
  fk <- getFirstKey bpfMap
  loop acc fk
  where
    -- loop :: (Storable key, Storable value) => a -> BPFMap key value -> key -> IO a
    loop a mk =
      case mk of
        Nothing -> pure a
        Just k -> do
          mv <- lookupElem bpfMap k
          case mv of
            Nothing -> pure a  -- shouldn't happen
            Just v -> do a' <- f a k v
                         mk' <- getNextKey bpfMap k
                         loop a' mk'

bpfMap_mapM_ :: (Storable key, Storable value) => (key -> value -> IO ()) ->
                 BPFMap key value -> IO ()
bpfMap_mapM_ f bpfMap = bpfMap_foldM (\_ -> f) () bpfMap
