{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Tools.DBQuery
  ( DBQueryLog (..),
    Query (..),
    Error (..),
    runQuery,
    runDBQuery,
    parseQuery,
  )
where

import Cardano.Tools.DB
  ( DB,
    DBError (..),
    Result (..),
    SlotNo,
    StandardBlock,
    StandardPoint,
    getBlock,
    getHeader,
    getParent,
    getSnapshot,
    listBlocks,
    listSnapshots,
    makeSlot,
    parsePoint,
    withDB,
  )
import Control.Tracer (Tracer, contramap)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Ouroboros.Consensus.Storage.ChainDB (TraceEvent)

data DBQueryLog = DBLog (TraceEvent StandardBlock)
  deriving stock (Eq, Show, Generic)

instance ToJSON DBQueryLog where
  toJSON = \case
    DBLog l -> object ["tag" .= ("ChainDB" :: Text), "log" .= show l]

data Error
  = ParseError Text
  | QueryError Text
  deriving (Eq, Show, Generic)

instance ToJSON Error where
  toJSON = \case
    ParseError msg -> object ["tag" .= ("ParseError" :: Text), "message" .= msg]
    QueryError msg -> object ["tag" .= ("QueryError" :: Text), "message" .= msg]

data Query
  = GetBlock StandardPoint
  | GetHeader StandardPoint
  | GetParent StandardPoint
  | GetSnapshot SlotNo
  | ListSnapshots
  | ListBlocks
  deriving (Eq, Show)

runQuery :: Tracer IO DBQueryLog -> FilePath -> FilePath -> Text -> IO ()
runQuery tracer configurationFile databaseDirectory query =
  withDB configurationFile databaseDirectory (contramap DBLog tracer) $ \db ->
    runDBQuery db query >>= \case
      Err err -> LBS.putStr $ Aeson.encode err
      Found result -> LBS.putStr result

runDBQuery :: DB -> Text -> IO (Result LBS.ByteString)
runDBQuery db query = do
  case parseQuery query of
    Left err -> pure $ Err (MalformedQuery query)
    Right q ->
      case q of
        GetBlock point -> getBlock db point
        GetHeader point -> getHeader db point
        GetParent point -> getParent db point
        GetSnapshot slot -> getSnapshot db slot
        ListSnapshots -> Found . Aeson.encode <$> listSnapshots db
        ListBlocks -> Found . Aeson.encode <$> listBlocks db

parseQuery :: Text -> Either Error Query
parseQuery str =
  case Text.words str of
    ["get-block", point] ->
      withPoint GetBlock point
    ["get-header", point] -> withPoint GetHeader point
    ["get-parent", point] -> withPoint GetParent point
    ["get-snapshot", slot] -> withSlot GetSnapshot slot
    ["list-snapshots"] -> Right ListSnapshots
    ["list-blocks"] -> Right ListBlocks
    _ -> Left $ ParseError "Invalid query"
  where
    withPoint q point =
      maybe (Left $ ParseError $ "error parsing point: " <> point) (Right . q) $ parsePoint point
    withSlot q slot =
      maybe (Left $ ParseError $ "error parsing slot: " <> slot) (Right . q) $ makeSlot slot
