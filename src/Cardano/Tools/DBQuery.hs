{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Cardano.Tools.DBQuery (runQuery) where

import Ouroboros.Consensus.Storage.ChainDB (TraceEvent)
import Cardano.Tools.DBServer (StandardBlock)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), (.=), object)
import Data.Text (Text)
import Control.Tracer (Tracer)

data DBQueryLog = DBLog (TraceEvent StandardBlock)
  deriving stock (Eq, Show, Generic)

instance ToJSON DBQueryLog where
  toJSON = \case
    DBLog l -> object ["tag" .= ("ChainDB" :: Text), "log" .= show l]

runQuery :: Tracer IO DBQueryLog -> FilePath -> FilePath -> String -> IO ()
runQuery tracer configurationFile databaseDirectory query =
  undefined
