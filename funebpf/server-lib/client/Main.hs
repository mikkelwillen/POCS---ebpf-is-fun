{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}

import Network.Socket
import Network.Socket.ByteString (sendAllTo, recvFrom)
import Network.Run.UDP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL (toStrict, replicate)
import qualified Data.ByteString.Builder as Builder
import Data.Word (Word32)
import Data.Int (Int64)
import Data.Foldable (forM_)
import System.Process (getCurrentPid)
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)
import Options.Applicative

type Command = BS.ByteString

verbosity = switch
  ( long "verbose"
 <> short 'v'
 <> help "Enable verbose mode" )

behaviour :: Parser String
behaviour = strOption
  ( long "behaviour"
  <> short 'b'
  <> metavar "BEHAVIOUR"
  <> help "Behaviour to run client with")

percent :: Parser Int
percent = option auto
  ( long "percent"
  <> short 'p'
  <> metavar "PERCENT"
  <> value 0
  <> help "Percent of messages that should be bad for frey client")

numberOfPackets :: Parser Int
numberOfPackets = option auto
  ( long "packets"
  <> short 'n'
  <> metavar "PACKETS"
  <> value 100_000
  <> help "Number of packets to send for frey2 client")

data Config = Config
  { optVerbose :: Bool
  , optBehaviour :: String
  , optPercent :: Int
  , optNumberOfPackets :: Int
  }

opts :: Parser Config
opts = Config <$> verbosity <*> behaviour <*> percent <*> numberOfPackets

parseArgs :: ParserInfo Config
parseArgs = info ( opts <**> helper ) fullDesc

logging :: String -> Bool -> IO()
logging msg verbose =
  if verbose then putStrLn msg
             else pure ()

main :: IO ()
main = do
  runUDPClient "127.0.0.1" "12345" $ \ sock addr -> do
    config <- execParser parseArgs
    let verbose = optVerbose config

    threadDelay 1000000 -- sleep for a little while

    case (optBehaviour config, optPercent config) of
      ("thor", _) -> thor sock addr verbose
      ("odin", _) -> odin sock addr verbose
      ("loki", _) -> loki sock addr verbose -- Shouldn't it be "loki"?
      ("njord", _)  -> njord sock addr verbose
      ("sylvie", _) -> sylvie sock addr verbose
      ("sif", _) -> sif sock addr verbose
      ("frey", p) -> frey sock addr verbose p
      ("frey2", p) -> frey2 sock addr verbose p (optNumberOfPackets config)
      ("frigg", p) -> frigg sock addr verbose p (optNumberOfPackets config)
      ("stop", _) -> stop sock addr verbose
      ("getstop", _) -> getstop sock addr verbose
      _ -> do logging "Not a valid behaviour" verbose

-- simple client putting a lot of values into the bag server
thor :: Socket -> SockAddr -> Bool -> IO()
thor sock addr verbose = do
  sendPutCmds sock addr verbose
  sendPutCmds sock addr verbose
  sendStopCmd sock addr verbose

-- client putting a lot of values into the bag server, then getting
odin :: Socket -> SockAddr -> Bool -> IO()
odin sock addr verbose = do
  sendPutCmds sock addr verbose
  sendGetCmd sock addr verbose
  sendPutCmds sock addr verbose
  sendGetCmd sock addr verbose
  sendStopCmd sock addr verbose

njord :: Socket -> SockAddr -> Bool -> IO()
njord sock addr verbose = do
  sendPutCmds sock addr verbose
  sendGetCmd sock addr verbose
  sendDelCmd sock addr verbose
  sendDelCmd sock addr verbose
  sendDelCmd sock addr verbose
  sendDelCmd sock addr verbose
  sendDelCmd sock addr verbose
  sendPutCmds sock addr verbose
  sendGetCmd sock addr verbose
  sendDelCmd sock addr verbose
  badDelCmd sock addr verbose
  sendStopCmd sock addr verbose

loki :: Socket -> SockAddr -> Bool -> IO()
loki sock addr verbose = do
  sendPutCmds sock addr verbose
  sendGetCmd sock addr verbose
  sendBadCmds sock addr verbose
  sendPutCmds sock addr verbose
  sendGetCmd sock addr verbose
  sendStopCmd sock addr verbose

sylvie :: Socket -> SockAddr -> Bool -> IO()
sylvie sock addr verbose = do
  logging "Just PUT'ing to the same key, what's wrong with that?" verbose
  forM_ (take 10_000_000 $ repeat $ putMsg 10 1) $
    sendCommand sock addr
  sendStopCmd sock addr verbose

sif :: Socket -> SockAddr -> Bool -> IO()
sif sock addr verbose = do
  sendGetCmd sock addr verbose
  sendPut9Cmd sock addr verbose
  sendGetCmd sock addr verbose
  sendGetCmd' sock addr verbose 18
  sendGetCmd' sock addr verbose 20
  sendDelCmd sock addr verbose
  sendGetCmd sock addr verbose
  sendStopCmd sock addr verbose

frey :: Socket -> SockAddr -> Bool -> Int -> IO()
frey sock addr verbose p = do
  let total = 100000
      bad = (total * p) `div` 100
      good = total - bad
  logging ("Sending " <> show good <> " PUT commands") verbose
  forM_ [1..good] $ \ m -> do
    sendCommand sock addr $ putMsg 10 1
  logging "PUT commands sent" verbose
  logging ("Sending " <> show bad <> " BAD commands") verbose
  -- sendBadCmds sock addr verbose
  forM_ [1..bad] $ \_ -> do
    sendCommand sock addr $ badMsg 256 -- 1024 -- 512 --
    -- NOTE: seems that when we use 1024 something hangs for too long
  logging "BAD commands sent" verbose
  -- sendGetCmdNoTO sock addr verbose 10
  sendGetCmd sock addr verbose
  sendStopCmd sock addr verbose

frey2 :: Socket -> SockAddr -> Bool -> Int -> Int -> IO()
frey2 sock addr verbose p n = do
  let total = n
      bad = (total * p) `div` 100
      good = total - bad
  logging ("Sending " <> show good <> " PUT commands") verbose
  forM_ [1..good] $ \ m -> do
    sendCommand sock addr $ putMsg 10 1
  logging "PUT commands sent" verbose
  logging ("Sending " <> show bad <> " BAD commands") verbose
  forM_ [1..bad] $ \_ -> do
    sendCommand sock addr $ badMsg 256
  logging "BAD commands sent" verbose
  sendGetCmd sock addr verbose
  sendStopCmd sock addr verbose

-- sends a mix of badand good commands
frigg :: Socket -> SockAddr -> Bool -> Int -> Int -> IO()
frigg sock addr verbose p n = do
  let total = n
      bad = (total * p) `div` 100
      good = total - bad
      iterTotal = total `div` 100
      iterGood = good `div` iterTotal
      iterBad = bad `div` iterTotal
  forM_ [1..iterTotal] $ \ _ -> do
    logging ("Sending " <> show iterGood <> " PUT commands") verbose
    forM_ [1..iterGood] $ \ m -> do
      sendCommand sock addr $ putMsg 10 1

    logging ("Sending " <> show iterBad <> " BAD commands") verbose
    forM_ [1..iterBad] $ \_ -> do
      sendCommand sock addr $ badMsg 256

  sendGetCmd sock addr verbose

-- sends stop message
stop :: Socket -> SockAddr -> Bool -> IO()
stop sock addr verbose = do
  sendStopCmd sock addr verbose

-- sends get and stop
getstop :: Socket -> SockAddr -> Bool -> IO()
getstop sock addr verbose = do
  sendGetCmd sock addr verbose
  sendStopCmd sock addr verbose

sendPut9Cmd :: Socket -> SockAddr -> Bool -> IO()
sendPut9Cmd sock addr verbose = do
    logging "Sending PUT commands" verbose
    forM_ ([4660] <> [2,4..100]) $ \ m -> do    -- 4660 = 0x1234
      sendCommand sock addr $ putMsg m 10
    logging "PUT commands sent" verbose

sendPutCmds :: Socket -> SockAddr -> Bool -> IO()
sendPutCmds sock addr verbose = do
    logging "Sending PUT commands" verbose
    forM_ [2,4,6,-5] $ \ m -> do
      sendCommand sock addr $ putMsg 10 m
    logging "PUT commands sent" verbose

sendGetCmd :: Socket -> SockAddr -> Bool -> IO()
sendGetCmd sock addr verbose = sendGetCmd' sock addr verbose 10

sendGetCmdNoTO :: Socket -> SockAddr -> Bool -> Word32 -> IO()
sendGetCmdNoTO sock addr verbose id = do
    logging "Sending GET command aiaiai" verbose
    sendCommand sock addr $ getMsg id
    logging "Sent GET command" verbose

    -- Receiving server response
    logging "Waiting for GET command response" verbose
    (resp, _server) <- recvFrom sock 1024
    logging ("Server response: " ++ show resp) verbose

sendGetCmd' :: Socket -> SockAddr -> Bool -> Word32 -> IO()
sendGetCmd' sock addr verbose id = do
    logging "Sending GET command" verbose
    sendCommand sock addr $ getMsg id
    logging "Sent GET command" verbose

    -- Receiving server response
    logging "Waiting for GET command response" verbose
    res <- timeout 10000 $ recvFrom sock 1024
    case res of
      Just (resp, _server) -> logging ("Server response: " ++ show resp) verbose
      _ -> logging "No response from server" verbose

sendBadCmds :: Socket -> SockAddr -> Bool -> IO()
sendBadCmds sock addr verbose = do
    logging "Sending many BAD commands" verbose
    forM_ [0..1000000] $ \ m -> do
      sendCommand sock addr $ badMsg 1024
    logging "BAD commands sent" verbose

sendDelCmd :: Socket -> SockAddr -> Bool -> IO()
sendDelCmd sock addr verbose = do
    logging "Sending DELETE command" verbose
    sendCommand sock addr $ delMsg 10
    logging "DELETE command sent" verbose

badDelCmd :: Socket -> SockAddr -> Bool -> IO()
badDelCmd sock addr verbose = do
    logging "Sending DELELULU command" verbose
    sendCommand sock addr $ deluluMsg 10
    logging "DELELULU command sent" verbose

sendStopCmd :: Socket -> SockAddr -> Bool -> IO()
sendStopCmd sock addr verbose = do
    logging "Sending STOP command" verbose
    sendCommand sock addr $ stopMsg
    logging "STOP command sent" verbose

putMsg :: Word32 -> Int64 -> Command
putMsg value multiplicity = command' "PUT" $ Builder.word32LE value <> Builder.int64LE multiplicity

getMsg :: Word32 -> Command
getMsg value = command' "GET" $ Builder.word32LE value

badMsg :: Int64 -> Command
badMsg times = command' "BAD" $ Builder.lazyByteString $ BL.replicate times 0x42

delMsg :: Word32 -> Command
delMsg value = command' "DELETE" $ Builder.word32LE value

deluluMsg :: Word32 -> Command
deluluMsg value = command' "DELELULU" $ Builder.word32LE value

stopMsg :: Command
stopMsg = command' "STOP" $ Builder.byteString ""

command' :: BS.ByteString -> Builder.Builder -> Command
command' oper args = BL.toStrict $ Builder.toLazyByteString msg
  where msg = Builder.byteString oper <> args

sendCommand :: Socket -> SockAddr -> Command -> IO ()
sendCommand sock addr cmd = sendAllTo sock cmd addr
