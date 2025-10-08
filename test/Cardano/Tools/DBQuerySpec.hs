{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tools.DBQuerySpec where

import Cardano.Crypto.Hash (Blake2b_256, hashToBytesAsHex, hashWith)
import Cardano.Tools.DB (DB, Result (..), StandardPoint, mkPoint)
import Cardano.Tools.DBQuery (DBQueryLog (..), Query (..), parseQuery, runDBQuery)
import Cardano.Tools.DBServer (withLog)
import Cardano.Tools.TestHelper (testBlockHex, testHeaderHex, testParentHash, testSnapshot, withLogFile, withTempDir, withTestDB)
import Control.Tracer (contramap)
import Data.Aeson (decode)
import qualified Data.ByteString.Base16.Lazy as LHex
import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>))
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldReturn, pendingWith)

spec :: Spec
spec = do
  describe "Query Parser" $ do
    it "can parse get-header query" $
      parseQuery "get-header 295.eeff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7"
        `shouldBe` Right (GetHeader $ mkPoint 295 "eeff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7")

  -- NOTE: Opening the ChainDB takes some time, so we do it only once for all tests
  -- This is fine as long as we don't write to the database
  aroundAll mkDB $ do
    it "allow querying header by point" $ \db ->
      runDBQuery db "get-header 45.42caa8f98ec335f71cfbf3f39b878c1c1055d1923280c33bf96dd73af8b621d6"
        `shouldReturn` Found (either error id $ LHex.decode testHeaderHex)

    it "allow listing all points from the DB" $ \db -> do
      Found json <- runDBQuery db "list-blocks"
      length <$> decode @[StandardPoint] json `shouldBe` Just 3000

    it "allow querying header's parent by point" $ \db -> do
      Found bytes <- runDBQuery db "get-parent 45.42caa8f98ec335f71cfbf3f39b878c1c1055d1923280c33bf96dd73af8b621d6"
      let parentHash = hashToBytesAsHex $ hashWith @Blake2b_256 id $ LBS.toStrict bytes
      parentHash `shouldBe` testParentHash

    it "allow querying block by point" $ \db ->
      runDBQuery db "get-block 45.42caa8f98ec335f71cfbf3f39b878c1c1055d1923280c33bf96dd73af8b621d6"
        `shouldReturn` Found (either error id $ LHex.decode testBlockHex)

    it "allow querying list of snapshots" $ \db -> do
      pendingWith "Snapshot logic may have changed in 10.5.1, under investigation"
      Found json <- runDBQuery db "list-snapshots"
      length <$> decode @[StandardPoint] json `shouldBe` Just 2160

    it "allow querying one snapshot from available list" $ \db -> do
      pendingWith "Snapshot logic may have changed in 10.5.1, under investigation"
      runDBQuery db "get-snapshot 59563"
        `shouldReturn` Found (either error id $ LHex.decode testSnapshot)

mkDB :: (DB -> IO r) -> IO r
mkDB k =
  withTempDir "test-db-server" $ \dir -> do
    withLogFile (dir </> "db-server.log") $ \hdl ->
      withLog hdl $ \tr ->
        withTestDB (contramap DBLog tr) $ \db -> k db
