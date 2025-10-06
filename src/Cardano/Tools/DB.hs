{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Cardano.Tools.DB
  ( StandardBlock,
    StandardPoint,
    pattern StandardPoint,
    Result (..),
    DBError (..),
    DB,
    DBTrace,
    LedgerDbBackend(..),
    LedgerDbConfig(..),
    SlotNo,
    Hash,
    asInteger,
    withDB,
    getHeader,
    getParent,
    getBlock,
    getSnapshot,
    listSnapshots,
    parsePoint,
    makePoint,
    makeSlot,
    mkPoint,
    listBlocks,
    parseLedgerDbConfig,
    toBytestring,
  )
where

import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Binary (serialize)
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Cardano.Tools.DBAnalyser.Block.Cardano (Args (CardanoBlockArgs))
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Control.Tracer (Tracer (..))
import Data.Aeson (FromJSON (..), ToJSON, object, toJSON, withObject, (.:), (.=), eitherDecodeFileStrict', Value (Object))
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block (ChainHash (..), ConvertRawHash (fromRawHash), Proxy (..), headerPrevHash, toRawHash)
import Ouroboros.Consensus.Block.Abstract (HeaderHash)
import Ouroboros.Consensus.Block.RealPoint
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block (LedgerState (..))
import Ouroboros.Consensus.Config (configStorage)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (ledgerState))
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Shelley.Ledger (LedgerState (shelleyLedgerState), ShelleyTip (..), shelleyLedgerTip)
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ShelleyHash (..))
import Ouroboros.Consensus.Storage.ChainDB (BlockComponent (..), ChainDB, IteratorResult (..), TraceEvent, defaultArgs, getBlockComponent, streamAll)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args (completeChainDbArgs, updateTracer)
import Ouroboros.Consensus.Storage.LedgerDB (LedgerDB (..))
import Ouroboros.Consensus.Util.IOLike (atomically)
import Control.ResourceRegistry (withRegistry)
import Ouroboros.Network.AnchoredSeq (lookupByMeasure)
import qualified Ouroboros.Network.AnchoredSeq as Seq
import Text.Read (readMaybe)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (fromJSON)

type StandardBlock = CardanoBlock StandardCrypto

asInteger :: SlotNo -> Integer
asInteger (SlotNo slot) = fromIntegral slot

type StandardPoint = RealPoint StandardBlock

type Hash = HeaderHash StandardBlock

pattern StandardPoint :: SlotNo -> HeaderHash StandardBlock -> StandardPoint
pattern StandardPoint slot hash = RealPoint slot hash

mkPoint :: Word64 -> Text -> StandardPoint
mkPoint slot hash =
  StandardPoint
    (SlotNo slot)
    ( fromRawHash (Proxy @StandardBlock) $
        either error id $
          Hex.decode (encodeUtf8 hash)
    )

instance ToJSON StandardPoint where
  toJSON (RealPoint slot hash) =
    object ["slot" .= slot, "hash" .= decodeUtf8 (Hex.encode $ toRawHash (Proxy @StandardBlock) hash)]

instance FromJSON StandardPoint where
  parseJSON = withObject "StandardPoint" $ \o -> do
    slot <- o .: "slot"
    hash <- o .: "hash"
    case Hex.decode
      ( encodeUtf8
          hash
      ) of
      Right bytes -> pure $ RealPoint slot (fromRawHash (Proxy @StandardBlock) bytes)
      Left _ -> fail "cannot decode hash"

type DBTrace = TraceEvent StandardBlock

type DB = ChainDB IO StandardBlock

withDB ::
  FilePath ->
  FilePath ->
  Tracer IO DBTrace ->
  (ChainDB IO StandardBlock -> IO a) ->
  IO a
withDB configurationFile databaseDir tracer k = do
  let args = CardanoBlockArgs configurationFile Nothing
  protocolInfo <- mkProtocolInfo args
  withRegistry $ \registry -> do
    let ProtocolInfo {pInfoInitLedger = genesisLedger, pInfoConfig = cfg} = protocolInfo
    let chunkInfo = Node.nodeImmutableDbChunkInfo (configStorage cfg)
        chainDbArgs =
          updateTracer tracer $
            completeChainDbArgs
              registry
              cfg
              genesisLedger
              chunkInfo
              (const True)
              (Node.stdMkChainDbHasFS databaseDir)
              (Node.stdMkChainDbHasFS databaseDir)
              _ldbArgs
              defaultArgs
    ChainDB.withDB chainDbArgs $ \chainDB -> k chainDB

parsePoint :: Text -> Maybe StandardPoint
parsePoint txt =
  case Text.splitOn "." txt of
    [slotTxt, hashTxt] -> makePoint slotTxt hashTxt
    _other -> Nothing

makePoint :: Text -> Text -> Maybe (RealPoint StandardBlock)
makePoint slotTxt hashTxt =
  readMaybe (Text.unpack slotTxt)
    >>= \(fromInteger -> slot) ->
      case Hex.decode (encodeUtf8 hashTxt) of
        Right bytes -> Just $ RealPoint slot (fromRawHash (Proxy @StandardBlock) bytes)
        Left _ -> Nothing

data DBError
  = NotFound
  | Malformed
  | MalformedQuery Text
  | InitialHeader
  | UnknownStateType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

toBytestring :: (IsString s) => DBError -> s
toBytestring = fromString . show

data Result a = Err DBError | Found a
  deriving stock (Eq, Show)

getHeader :: ChainDB IO StandardBlock -> StandardPoint -> IO (Result LBS.ByteString)
getHeader db point =
  maybe (Err NotFound) Found <$> getBlockComponent db GetRawHeader point

getParent :: ChainDB IO StandardBlock -> StandardPoint -> IO (Result LBS.ByteString)
getParent db point =
  getBlockComponent db GetHeader point >>= \case
    Nothing -> pure (Err NotFound)
    Just header ->
      case headerPrevHash header of
        BlockHash headerHash ->
          getHeader db (RealPoint (SlotNo 0) headerHash) -- FIXME
        GenesisHash -> pure (Err InitialHeader)

getBlock :: ChainDB IO StandardBlock -> StandardPoint -> IO (Result LBS.ByteString)
getBlock db point =
  maybe (Err NotFound) Found <$> getBlockComponent db GetRawBlock point

--pointOfState :: LedgerState StandardBlock -> StandardPoint
--pointOfState = \case
--  LedgerStateConway state ->
--    makePointOfState state
--  LedgerStateBabbage state ->
--    makePointOfState state
--  _ -> error "snapshots older than conway are not supported"
--  where
--    makePointOfState state =
--      case shelleyLedgerTip state of
--        At shelleyTip ->
--          RealPoint
--            (shelleyTipSlotNo shelleyTip)
--            ( fromRawHash (Proxy @StandardBlock)
--                . hashToBytes
--                . unShelleyHash @StandardCrypto
--                $ shelleyTipHash shelleyTip
--            )
--        Origin -> error "ledger state is at origin"

makeSlot :: Text -> Maybe SlotNo
makeSlot slotTxt = fromInteger <$> readMaybe (Text.unpack slotTxt)

listSnapshots :: ChainDB IO StandardBlock -> IO [StandardPoint]
listSnapshots db = do
  --LedgerDB {getPastLedgerState} <- atomically $ ChainDB.getCurrentLedger db
  --let snapshotsList :: [LedgerState StandardBlock] = ledgerState . unCheckpoint <$> Seq.toOldestFirst getPastLedgerState
  --pure $ pointOfState <$> snapshotsList
  pure []

listBlocks :: ChainDB IO StandardBlock -> IO [StandardPoint]
listBlocks db = do
  withRegistry $ \registry ->
    streamAll db registry GetHeader >>= go []
  where
    go acc iter = do
      ChainDB.iteratorNext iter >>= \case
        IteratorResult hdr -> go (headerRealPoint hdr : acc) iter
        IteratorExhausted -> pure acc
        IteratorBlockGCed _ ->
          error "block on the current chain was garbage-collected"

getSnapshot :: ChainDB IO StandardBlock -> StandardPoint -> IO (Result LBS.ByteString)
getSnapshot db point = do
  maybeLedgerState <- atomically $ do
    LedgerDB {getPastLedgerState} <- ChainDB.getCurrentLedger db
    getPastLedgerState point

  case maybeLedgerState of
    Just (LedgerStateConway state) ->
      pure $ Found $ serialize (toEnum 10) $ shelleyLedgerState state
    _other -> pure (Err NotFound)


-- # LedgerDb Configuration
-- We need to read the node's config file to get its LedgerDB options
-- There is no way to parse the node config without pulling in cardano-node code and dependencies,
-- which defeats the purpose of this code.

data LedgerDbConfig = LedgerDbConfig
  { backend :: LedgerDbBackend,
    numOfDiskSnapshots :: Int,
    queryBatchSize :: Int,
    snapshotInterval :: Int
  }
  deriving (Eq, Show)

data LedgerDbBackend = V2InMemory
  deriving (Eq, Show)

newtype LedgerDbConfigError = LedgerDbConfigError String
  deriving (Eq, Show)

instance FromJSON LedgerDbConfig where
  parseJSON = withObject "LedgerDbConfig" $ \v ->
    LedgerDbConfig
      <$> v .: "Backend"
      <*> v .: "NumOfDiskSnapshots"
      <*> v .: "QueryBatchSize"
      <*> v .: "SnapshotInterval"

instance FromJSON LedgerDbBackend where
  parseJSON "V2InMemory" = pure V2InMemory
  parseJSON _ = fail "Unsupported LedgerDbBackend"

parseLedgerDbConfig :: FilePath -> IO (Either LedgerDbConfigError LedgerDbConfig)
parseLedgerDbConfig nodeConfig = do
  eitherDecodeFileStrict' nodeConfig >>= \case
    Left _ ->
      undefined
    Right (Object config) ->
      case KeyMap.lookup "LedgerDB" config of
        Nothing ->
          undefined
        Just value ->
          case fromJSON value of
            Aeson.Error err ->
              fail err
            Aeson.Success v ->
              pure (Right v)
    Right _ ->
      undefined