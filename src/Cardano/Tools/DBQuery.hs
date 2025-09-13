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
    parseQuery,
  )
where

import Cardano.Tools.DB
  ( DB,
    Result (..),
    SlotNo,
    StandardBlock,
    StandardPoint,
    getBlock,
    getHeader,
    getParent,
    getSnapshot,
    getSnapshots,
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
  deriving (Eq, Show)

data Query
  = GetBlock StandardPoint
  | GetHeader StandardPoint
  | GetParent StandardPoint
  | GetSnapshot SlotNo
  | ListSnapshots
  deriving (Eq, Show)

runQuery :: Tracer IO DBQueryLog -> FilePath -> FilePath -> Text -> IO (Either Error LBS.ByteString)
runQuery tracer configurationFile databaseDirectory query =
  withDB configurationFile databaseDirectory (contramap DBLog tracer) $ \db -> do
    case parseQuery query of
      Left err -> pure $ Left err
      Right q ->
        runDBQuery db q >>= \case
          NotFound -> return $ Left $ QueryError "Not found"
          Malformed -> return $ Left $ QueryError "Malformed"
          Found result -> return $ Right result

runDBQuery :: DB -> Query -> IO (Result LBS.ByteString)
runDBQuery db = \case
  GetBlock point -> getBlock db point
  GetHeader point -> getHeader db point
  GetParent point -> getParent db point
  GetSnapshot slot -> getSnapshot db slot
  ListSnapshots -> Found . Aeson.encode <$> getSnapshots db

parseQuery :: Text -> Either Error Query
parseQuery str =
  case Text.words str of
    ["get-block", point] ->
      withPoint GetBlock point
    ["get-header", point] -> withPoint GetHeader point
    ["get-parent", point] -> withPoint GetParent point
    ["get-snapshot", slot] -> withSlot GetSnapshot slot
    ["list-snapshots"] -> Right ListSnapshots
    _ -> Left $ ParseError "Invalid query"
  where
    withPoint q point =
      maybe (Left $ ParseError $ "error parsing point: " <> point) (Right . q) $ parsePoint point
    withSlot q slot =
      maybe (Left $ ParseError $ "error parsing point: " <> slot) (Right . q) $ makeSlot slot
