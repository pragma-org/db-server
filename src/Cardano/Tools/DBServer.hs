{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Tools.DBServer (run, DBServerLog (..), withLog, withDB, app) where

import Cardano.Crypto (unsafeHashFromBytes)
import Cardano.Ledger.Api (StandardCrypto)
import Cardano.Protocol.TPraos.BHeader (HashHeader (HashHeader))
import Cardano.Tools.DBAnalyser.Block.Cardano (Args (CardanoBlockArgs))
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Control.Monad (forever)
import Control.Tracer (Tracer (..), contramap, traceWith)
import Data.Aeson (FromJSON, ToJSON, Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base16.Lazy as LHex
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word64)
import qualified GHC.Clock
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404, statusCode, statusMessage)
import Network.Socket (PortNumber)
import Network.Wai (Application, Middleware, pathInfo, requestMethod, responseLBS, responseStatus)
import qualified Network.Wai.Handler.Warp as Warp
import Ouroboros.Consensus.Block (ConvertRawHash (fromRawHash), Proxy (..))
import Ouroboros.Consensus.Block.RealPoint
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Config (TopLevelConfig, configStorage)
import Ouroboros.Consensus.Fragment.InFuture (dontCheck)
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Storage.ChainDB (BlockComponent (GetRawHeader), ChainDB, TraceEvent, defaultArgs, getBlockComponent)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args (completeChainDbArgs, updateTracer)
import Ouroboros.Consensus.Util.IOLike (MonadSTM (writeTQueue), atomically, newTQueueIO, readTQueue, withAsync)
import Ouroboros.Consensus.Util.ResourceRegistry (withRegistry)
import System.IO (Handle, hFlush, stdout)
import Text.Read (readMaybe)

data DBServerLog = HttpServerLog HttpServerLog | DBLog (TraceEvent StandardBlock)
  deriving stock (Eq, Show, Generic)

instance ToJSON DBServerLog where
  toJSON = \case
    HttpServerLog l -> object ["tag" .= ("HttpServer" :: Text), "log" .= toJSON l]
    DBLog l -> object ["tag" .= ("ChainDB" :: Text), "log" .= show l]

run :: Tracer IO DBServerLog -> PortNumber -> String -> FilePath -> FilePath -> IO ()
run tracer (fromIntegral -> port) host configurationFile databaseDirectory = do
  withDB configurationFile databaseDirectory (contramap DBLog tracer) $ \db ->
    Warp.runSettings settings $
      tracerMiddleware tr $
        app db
 where
  tr = contramap HttpServerLog tracer
  settings =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost (fromString host)
      & Warp.setServerName "db-server"
      & Warp.setTimeout 120
      & Warp.setMaximumBodyFlush Nothing
      & Warp.setBeforeMainLoop (traceWith tr HttpServerListening{host, port})

type StandardBlock = CardanoBlock StandardCrypto

withDB ::
  FilePath ->
  FilePath ->
  Tracer IO (TraceEvent StandardBlock) ->
  (ChainDB IO StandardBlock -> IO a) ->
  IO a
withDB configurationFile databaseDir tracer k = do
  let args = CardanoBlockArgs configurationFile Nothing
  protocolInfo <- mkProtocolInfo args
  withRegistry $ \registry -> do
    let ProtocolInfo{pInfoInitLedger = genesisLedger, pInfoConfig = cfg} = protocolInfo
    let chunkInfo = Node.nodeImmutableDbChunkInfo (configStorage cfg)
        chainDbArgs =
          updateTracer tracer $
            completeChainDbArgs
              registry
              dontCheck
              cfg
              genesisLedger
              chunkInfo
              (const True)
              (Node.stdMkChainDbHasFS databaseDir)
              (Node.stdMkChainDbHasFS databaseDir)
              defaultArgs
    ChainDB.withDB chainDbArgs $ \chainDB -> k chainDB

app :: ChainDB IO StandardBlock -> Application
app db req send =
  case pathInfo req of
    [slot, hash, "header"] -> handleGetHeader slot hash
    _ -> send responseNotFound
 where
  responseNotFound = responseLBS status404 [] ""

  responseBadRequest = responseLBS status400 [] ""

  handleGetHeader slot hash = do
    case makePoint slot hash of
      Nothing -> send responseBadRequest
      Just point ->
        getBlockComponent db GetRawHeader point >>= \case
          Just header -> send $ responseLBS status200 [("content-type", "application/text")] (LHex.encode header)
          Nothing -> send responseNotFound

makePoint :: Text -> Text -> Maybe (RealPoint StandardBlock)
makePoint slotTxt hashTxt =
  readMaybe (Text.unpack slotTxt)
    >>= \(fromInteger -> slot) ->
      case Hex.decode (encodeUtf8 hashTxt) of
        Right bytes -> Just $ RealPoint slot (fromRawHash (Proxy @StandardBlock) bytes)
        Left _ -> Nothing

-- * Tracing
withLog :: Handle -> (Tracer IO DBServerLog -> IO a) -> IO a
withLog hdl k = do
  logQueue <- newTQueueIO
  let tracer =
        Tracer $ \msg -> do
          time <- getCurrentTime
          let logged =
                case toJSON msg of
                  Object o ->
                    Object $ o <> KeyMap.fromList [("timestamp", toJSON time)]
                  other -> object ["timestamp" .= time, "log" .= other]
          atomically $ writeTQueue logQueue $ LBS.toStrict $ encode logged <> "\n"

      runLogThread = forever $ do
        msg <- atomically $ readTQueue logQueue
        BS.hPutStr hdl msg
        hFlush hdl

  withAsync runLogThread $
    \_ -> k tracer

tracerMiddleware :: Tracer IO HttpServerLog -> Middleware
tracerMiddleware tr runApp req send = do
  start <- GHC.Clock.getMonotonicTimeNSec
  traceWith tr HttpRequest{path, method}
  runApp req $ \res -> do
    result <- send res
    end <- GHC.Clock.getMonotonicTimeNSec
    let time = mkRequestTime start end
    traceWith tr HttpResponse{status = mkStatus (responseStatus res), time}
    pure result
 where
  method = decodeUtf8 (requestMethod req)
  path = pathInfo req

mkStatus :: Status -> HttpStatus
mkStatus status =
  HttpStatus{code, message}
 where
  code = statusCode status
  message = decodeUtf8 (statusMessage status)

newtype RequestTime = RequestTime {milliseconds :: Word64}
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

mkRequestTime :: Word64 -> Word64 -> RequestTime
mkRequestTime start end =
  RequestTime $ (end - start) `div` 1_000_000

data HttpStatus = HttpStatus {code :: Int, message :: Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data HttpServerLog
  = HttpServerListening {host :: String, port :: Int}
  | HttpRequest {path :: [Text], method :: Text}
  | HttpResponse {status :: HttpStatus, time :: RequestTime}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
