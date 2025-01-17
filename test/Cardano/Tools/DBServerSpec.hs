{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tools.DBServerSpec where

import Cardano.Tools.DBServer (DBServerLog (..), app, withDB, withLog)
import Data.Functor.Contravariant (contramap)
import Network.Wai (Application)
import System.IO (Handle)
import System.Posix.Temp (mkstemp)
import Test.Hspec (Spec, it)
import Test.Hspec.Wai (get, shouldRespondWith, with)

spec :: Spec
spec =
  with testApp $
    it "GET /:slot/:hash/header returns block header in hex-encoded CBOR given it exists" $ do
      get "/295/eeff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7/header" `shouldRespondWith` 200

testConfigFile :: FilePath
testConfigFile = "test-data/config/config.json"

testDatabaseDir :: FilePath
testDatabaseDir = "test-data/test-db"

testApp :: IO Application
testApp = do
  (_, hdl) <- createLog
  withLog hdl $ \tr ->
    withDB testConfigFile testDatabaseDir (contramap DBLog tr) $ \db -> do
      pure $ app db

createLog :: IO (FilePath, Handle)
createLog = mkstemp "test-db-server"
