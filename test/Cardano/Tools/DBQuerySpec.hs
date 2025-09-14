{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tools.DBQuerySpec where

import Cardano.Tools.DB ( DB, mkPoint, Result(..) )
import Cardano.Tools.DBQuery (DBQueryLog (..), Query (..), parseQuery, runDBQuery)
import Cardano.Tools.DBServer (withLog)
import Cardano.Tools.TestHelper (withLogFile, withTempDir, withTestDB, testHeaderHex)
import Control.Tracer (contramap)
import System.FilePath ((</>))
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString.Base16.Lazy as LHex

spec :: Spec
spec = do
  describe "Query Parser" $ do
    it "can parse get-header query" $
      parseQuery "get-header 295.eeff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7"
        `shouldBe` Right (GetHeader $ mkPoint 295 "eeff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7")

  -- NOTE: Opening the ChainDB takes some time, so we do it only once for all tests
  -- This is fine as long as we don't write to the database
  around mkDB $ do
    it "allow querying header by point" $ \db ->
      runDBQuery db "get-header 295.eeff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7"
        `shouldReturn` Found (either error id $ LHex.decode $ LT.encodeUtf8 $ LT.pack testHeaderHex)

mkDB :: (DB -> IO r) -> IO r
mkDB k =
  withTempDir "test-db-server" $ \dir -> do
    withLogFile (dir </> "db-server.log") $ \hdl ->
      withLog hdl $ \tr ->
        withTestDB (contramap DBLog tr) $ \db -> k db
