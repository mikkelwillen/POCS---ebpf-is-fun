{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module ServerLib
  (defaultMain
  , serve
  )
where

import Network.Socket
import Network.Run.UDP
import Network.Socket.ByteString (recvFrom, sendTo)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL (fromStrict)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map.Strict as Map
import Data.Binary.Get (runGet, getInt64le, getWord32le)
import Data.Word (Word32)
import Data.Word8 (isLetter)
import Data.Int (Int64)
import Data.Foldable (for_)
import Control.Monad (when)
import System.Environment (getArgs)
import Options.Applicative

type Key = Word32
type Count = Int64

type Bag = Map.Map Key Count

type ServerState = Bag
type Request = BS.ByteString
type Response = Maybe BS.ByteString

data Message = Get Key | Put Key Int64 | Delete Key | Stop
  deriving (Eq, Show)

type PFun = Socket -> IO()

verbosity = switch
  ( long "verbose"
 <> short 'v'
 <> help "Enable verbose mode" )

capacity = option auto
  ( long "capacity"
  <> short 'c'
  <> help "Give max key capacity of the server"
  <> showDefault
  <> value 100000
  <> metavar "INT")

data Config = Config
  { optVerbose :: Bool
  , optCap :: Int
  }

opts :: Parser Config
opts = Config <$> verbosity <*> capacity

parseArgs :: ParserInfo Config
parseArgs = info ( opts <**> helper ) fullDesc

defaultMain :: PFun -> PFun -> IO()
defaultMain pre post = runUDPServer Nothing "12345" $ \sock -> do
  -- configuration from commandline
  config <- execParser parseArgs
  let verbose = optVerbose config
      cap = optCap config

  logging verbose "Starting server"
  pre sock
  serve sock Map.empty verbose cap
  post sock

serve :: Socket -> ServerState -> Bool -> Int -> IO ()
serve sock state verbose cap = do
  (req, addr) <- recvFrom sock 1024
  case parseMessage req of
    Nothing ->
      serve sock state verbose cap
    Just Stop -> do
      logging verbose $ "Stopping server"
      -- putStrLn $ show $ Map.keys state -- for debugging :/
      return ()
    Just msg -> do
      logging verbose $ "Got " ++ show msg
      let (resp, newState) = processMessage state verbose cap msg
      for_ resp $ \reply -> do
        logging verbose "Sending reply"
        sendTo sock reply addr
      serve sock newState verbose cap

processMessage :: ServerState -> Bool -> Int -> Message -> (Response, ServerState)
processMessage bag verbose cap = \case
  Put value multiplicity -> (Nothing, newBag)
    where
      newBag = if Map.size bag < cap || Map.member value bag
        then Map.insertWith (+) value multiplicity bag
        else bag
  Get value -> (case Map.lookup value bag of
                  Just multiplicity -> Just . BC8.pack . show $ multiplicity
                  _ -> Nothing
               , bag)
  Delete value -> (Nothing, Map.delete value bag)


parseMessage :: Request -> Maybe Message
parseMessage req =
  case cmd of
    "PUT" -> pure $ runGet (Put <$> getWord32le <*> getInt64le) rest
    "GET" -> pure $ Get $ runGet getWord32le rest
    "DELETE" -> pure $ Delete $ runGet getWord32le rest
    "STOP" -> pure Stop
    _ -> Nothing -- Silently ignore errors
  where
    (cmd, restStrict) = BS.span (isLetter) req
    rest = BL.fromStrict restStrict

logging verbose msg =
  when verbose $ putStr $ "+ "<> msg <> "\n"



-- WIP : for composing filters
-- paramParser :: (Key -> Int -> Maybe Message) -> (Key -> Maybe Message) ->
--                (Key -> Maybe Message) -> (Maybe Message) -> (Maybe Message) ->
--                (Request -> Maybe Message)
-- paramParser put get delete stop err =
--   choice [ string "PUT" *> put <$> getWord32le <*> getInt64le
--          , string "GET" *> get <$> getWord32le
--          , string "DELETE" *> delete <$> getWord32le
--          , string "STOP" *> stop
--          , err
--          ]

-- parseMessage' :: Request -> Maybe Message
-- parseMessage' = runParser $ paramParser Put Get Delete Stop Nothing

-- sockFilt = giveAST $ paramParser put get del stop err req
--   where
--     put _ _ = const True
--     get _ = const True
--     del _ = const True
--     stop = const True
--     err = const False

-- dynServ :: BPFMap Key Int -> Request -> Maybe Message
-- dynServ kMap = runParser $ paramParser put get del stop err
--   where
--     put k m = do
--       res <- updateElem kMap k 1 Any
--       case res of
--         Success -> Put k m
--         _ -> Nothing
--     get k = do
--       res <- lookupElem kMap k
--       case res of
--         Just _ -> Get k
--         _ -> Nothing
--     del k = do
--       res <- deleteElem kMap k Exist
--       case res of
--         Success -> Delete k
--         _ -> Nothing
--     stop = Stop
--     err = Nothing
