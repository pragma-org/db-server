{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Cardano.Tools.DBServer (
  run,
  DBServerLog (..),
  StandardPoint,
  asInteger,
  pattern StandardPoint,
  withLog,
  withDB,
  webApp,
  tracerMiddleware,
) where

import Cardano.Crypto.Hash (hashToBytes, hashToBytesShort)
import Cardano.Ledger.Api (StandardCrypto)
import Cardano.Ledger.Binary (serialize, serialize')
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Cardano.Tools.DBAnalyser.Block.Cardano (Args (CardanoBlockArgs))
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Control.Monad (forever)
import Control.Tracer (Tracer (..), contramap, traceWith)
import Data.Aeson (FromJSON (..), ToJSON, Value (..), encode, object, toJSON, (.:), (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base16.Lazy as LHex
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.SOP (hmap)
import Data.SOP.NS (index_NS)
import Data.SOP.Telescope (tip)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word64)
import Debug.Trace (trace)
import qualified GHC.Clock
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404, statusCode, statusMessage)
import Network.Socket (PortNumber)
import Network.Wai (Application, Middleware, pathInfo, requestMethod, responseLBS, responseStatus)
import qualified Network.Wai.Handler.Warp as Warp
import Ouroboros.Consensus.Block (ChainHash (..), ConvertRawHash (fromRawHash), Proxy (..), headerPrevHash, toRawHash, toShortRawHash)
import Ouroboros.Consensus.Block.Abstract (HeaderHash)
import Ouroboros.Consensus.Block.RealPoint
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block (LedgerState (..))
import Ouroboros.Consensus.Config (configStorage)
import Ouroboros.Consensus.Fragment.InFuture (dontCheck)
import Ouroboros.Consensus.HardFork.Combinator (LedgerState (hardForkLedgerStatePerEra), OneEraHash (..), getHardForkState)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (ledgerState))
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Shelley.Ledger (LedgerState (shelleyLedgerState), ShelleyTip (..), shelleyLedgerTip)
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ShelleyHash (..))
import Ouroboros.Consensus.Storage.ChainDB (BlockComponent (..), ChainDB, TraceEvent, defaultArgs, getBlockComponent)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args (completeChainDbArgs, updateTracer)
import Ouroboros.Consensus.Storage.LedgerDB (Checkpoint (unCheckpoint), LedgerDB (..))
import Ouroboros.Consensus.Util.IOLike (MonadSTM (writeTQueue), atomically, newTQueueIO, readTQueue, withAsync)
import Ouroboros.Consensus.Util.ResourceRegistry (withRegistry)
import Ouroboros.Network.AnchoredSeq (lookupByMeasure)
import qualified Ouroboros.Network.AnchoredSeq as Seq
import System.IO (Handle, hFlush)
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
        webApp db
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

asInteger :: SlotNo -> Integer
asInteger (SlotNo slot) = fromIntegral slot

type StandardPoint = RealPoint StandardBlock

pattern StandardPoint :: SlotNo -> HeaderHash StandardBlock -> StandardPoint
pattern StandardPoint slot hash = RealPoint slot hash

instance ToJSON StandardPoint where
  toJSON (RealPoint slot hash) =
    object ["slot" .= slot, "hash" .= decodeUtf8 (Hex.encode $ toRawHash (Proxy @StandardBlock) hash)]

instance FromJSON StandardPoint where
  parseJSON = \case
    Object o -> do
      slot <- o .: "slot"
      hash <- o .: "hash"
      case Hex.decode (encodeUtf8 hash) of
        Right bytes -> pure $ RealPoint slot (fromRawHash (Proxy @StandardBlock) bytes)
        Left _ -> fail "cannot decode hash"
    _ -> fail "expected object"

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

webApp :: ChainDB IO StandardBlock -> Application
webApp db req send =
  case pathInfo req of
    ["snapshots"] -> handleGetSnapshots
    ["snapshots", slot] -> handleGetSnapshot slot
    [slot, hash] -> handleGetBlock slot hash
    [slot, hash, "header"] -> handleGetHeader slot hash
    [slot, hash, "header", "parent"] -> handleGetParent slot hash
    _ -> send responseNotFound
 where
  responseNotFound = responseLBS status404 [] ""

  responseBadRequest = responseLBS status400 [] ""

  handleGetSnapshots = do
    LedgerDB{ledgerDbCheckpoints} <- atomically $ ChainDB.getLedgerDB db
    let snapshotsList :: [LedgerState StandardBlock] = ledgerState . unCheckpoint <$> Seq.toOldestFirst ledgerDbCheckpoints
        snapshotsPoints :: [StandardPoint] = pointOfState <$> snapshotsList
    send $
      responseLBS
        status200
        [("content-type", "application/json")]
        (encode snapshotsPoints)

  handleGetHeader slot hash = do
    case makePoint slot hash of
      Nothing -> send responseBadRequest
      Just point ->
        getBlockComponent db GetRawHeader point >>= \case
          Just header -> send $ responseLBS status200 [("content-type", "application/text")] (LHex.encode header)
          Nothing -> send responseNotFound

  handleGetSnapshot slotTxt =
    case makeSlot slotTxt of
      Nothing -> send responseBadRequest
      Just slot -> do
        LedgerDB{ledgerDbCheckpoints} <- atomically $ ChainDB.getLedgerDB db
        case lookupByMeasure (At slot) ledgerDbCheckpoints of
          [snapshot] ->
            case ledgerState $ unCheckpoint snapshot of
              LedgerStateBabbage state ->
                send $
                  responseLBS
                    status200
                    [("content-type", "application/json")]
                    (Base64.encode $ serialize (toEnum 10) $ shelleyLedgerState state)
              _other -> send responseNotFound
          _other -> send responseNotFound

  handleGetParent slot hash = do
    case makePoint slot hash of
      Nothing -> send responseBadRequest
      Just point ->
        getBlockComponent db GetHeader point >>= \case
          Just header ->
            case headerPrevHash header of
              BlockHash headerHash ->
                handleGetHeader slot (decodeUtf8 $ Hex.encode $ toRawHash (Proxy @StandardBlock) headerHash)
              GenesisHash -> send responseNotFound
          Nothing -> send responseNotFound

  handleGetBlock slot hash = do
    case makePoint slot hash of
      Nothing -> send responseBadRequest
      Just point ->
        getBlockComponent db GetRawBlock point >>= \case
          Just header -> send $ responseLBS status200 [("content-type", "application/text")] (LHex.encode header)
          Nothing -> send responseNotFound

pointOfState :: LedgerState StandardBlock -> StandardPoint
pointOfState = \case
  LedgerStateBabbage state ->
    case shelleyLedgerTip state of
      At shelleyTip ->
        RealPoint
          (shelleyTipSlotNo shelleyTip)
          ( fromRawHash (Proxy @StandardBlock)
              . hashToBytes
              . unShelleyHash @StandardCrypto
              $ shelleyTipHash shelleyTip
          )
      Origin -> error "ledger state is at origin"
  LedgerStateByron _ -> error "byron snapshots are not supported"

makeSlot :: Text -> Maybe SlotNo
makeSlot slotTxt = fromInteger <$> readMaybe (Text.unpack slotTxt)

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
