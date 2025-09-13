{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Tools.DB where

import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Ledger.Api (StandardCrypto)
import Cardano.Ledger.Binary (serialize)
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Cardano.Tools.DBAnalyser.Block.Cardano (Args (CardanoBlockArgs))
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Control.Tracer (Tracer (..))
import Data.Aeson (FromJSON (..), ToJSON, object, toJSON, withObject, (.:), (.=))
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Ouroboros.Consensus.Block (ChainHash (..), ConvertRawHash (fromRawHash), Proxy (..), headerPrevHash, toRawHash)
import Ouroboros.Consensus.Block.Abstract (HeaderHash)
import Ouroboros.Consensus.Block.RealPoint
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block (LedgerState (..))
import Ouroboros.Consensus.Config (configStorage)
import Ouroboros.Consensus.Fragment.InFuture (dontCheck)
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
import Ouroboros.Consensus.Util.IOLike (atomically)
import Ouroboros.Consensus.Util.ResourceRegistry (withRegistry)
import Ouroboros.Network.AnchoredSeq (lookupByMeasure)
import qualified Ouroboros.Network.AnchoredSeq as Seq
import Text.Read (readMaybe)

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
  parseJSON = withObject "StandardPoint" $ \o -> do
    slot <- o .: "slot"
    hash <- o .: "hash"
    case Hex.decode (encodeUtf8 hash) of
      Right bytes -> pure $ RealPoint slot (fromRawHash (Proxy @StandardBlock) bytes)
      Left _ -> fail "cannot decode hash"

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
    let ProtocolInfo {pInfoInitLedger = genesisLedger, pInfoConfig = cfg} = protocolInfo
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

makePoint :: Text -> Text -> Maybe (RealPoint StandardBlock)
makePoint slotTxt hashTxt =
  readMaybe (Text.unpack slotTxt)
    >>= \(fromInteger -> slot) ->
      case Hex.decode (encodeUtf8 hashTxt) of
        Right bytes -> Just $ RealPoint slot (fromRawHash (Proxy @StandardBlock) bytes)
        Left _ -> Nothing

data Result a = Malformed | NotFound | Found a
  deriving stock (Eq, Show)

getHeader :: ChainDB IO StandardBlock -> Text -> Text -> IO (Result LBS.ByteString)
getHeader db slot hash = do
  case makePoint slot hash of
    Nothing ->
      return Malformed
    Just point ->
      maybe NotFound Found <$> getBlockComponent db GetRawHeader point

getParent :: ChainDB IO StandardBlock -> Text -> Text -> IO (Result LBS.ByteString)
getParent db slot hash =
  case makePoint slot hash of
    Nothing -> pure Malformed
    Just point ->
      getBlockComponent db GetHeader point >>= \case
        Nothing -> pure NotFound
        Just header ->
          case headerPrevHash header of
            BlockHash headerHash ->
              getHeader db slot (decodeUtf8 $ Hex.encode $ toRawHash (Proxy @StandardBlock) headerHash)
            GenesisHash -> pure Malformed

getBlock :: ChainDB IO StandardBlock -> Text -> Text -> IO (Result LBS.ByteString)
getBlock db slot hash =
  case makePoint slot hash of
    Nothing ->
      pure Malformed
    Just point ->
      maybe NotFound Found <$> getBlockComponent db GetRawBlock point

pointOfState :: LedgerState StandardBlock -> StandardPoint
pointOfState = \case
  LedgerStateConway state ->
    makePointOfState state
  LedgerStateBabbage state ->
    makePointOfState state
  _ -> error "snapshots older than conway are not supported"
  where
    makePointOfState state =
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

makeSlot :: Text -> Maybe SlotNo
makeSlot slotTxt = fromInteger <$> readMaybe (Text.unpack slotTxt)

getSnapshots :: ChainDB IO StandardBlock -> IO [StandardPoint]
getSnapshots db = do
  LedgerDB {ledgerDbCheckpoints} <- atomically $ ChainDB.getLedgerDB db
  let snapshotsList :: [LedgerState StandardBlock] = ledgerState . unCheckpoint <$> Seq.toOldestFirst ledgerDbCheckpoints
  pure $ pointOfState <$> snapshotsList

getSnapshot :: ChainDB IO StandardBlock -> Text -> IO (Maybe LBS.ByteString)
getSnapshot db slotTxt = case makeSlot slotTxt of
  Nothing -> pure Nothing
  Just slot -> do
    LedgerDB {ledgerDbCheckpoints} <- atomically $ ChainDB.getLedgerDB db
    case lookupByMeasure (At slot) ledgerDbCheckpoints of
      [snapshot] ->
        case ledgerState $ unCheckpoint snapshot of
          LedgerStateBabbage state ->
            pure $ Just $ serialize (toEnum 10) $ shelleyLedgerState state
          _other -> pure Nothing
      _other -> pure Nothing
