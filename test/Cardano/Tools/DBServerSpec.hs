{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Tools.DBServerSpec where

import Cardano.Tools.DB (StandardPoint, asInteger, pattern StandardPoint)
import Cardano.Tools.DBServer (DBServerLog (..), tracerMiddleware, webApp, withLog)
import Cardano.Tools.TestHelper (testBlockHex, testHeaderHex, testParentHex, withLogFile, withTempDir, withTestDB)
import Data.Aeson (decode)
import Data.Functor.Contravariant (contramap)
import Data.String (fromString)
import qualified Data.Text as Text
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai (Application, Request (..), defaultRequest)
import Network.Wai.Test (SResponse, Session, request, runSession, simpleBody, simpleStatus)
import System.FilePath ((</>))
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldNotBe, pendingWith, pending)
import Test.QuickCheck (counterexample, elements, property)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, pick, run)

spec :: Spec
spec =
  -- NOTE: Opening the ChainDB takes some time, so we do it only once for all tests
  -- This is fine as long as we don't write to the database
  aroundAll mkApp $ do
    describe "GET /blocks/:slot/:hash/header" $ do
      it "returns block header in hex-encoded CBOR given it exists" $ \app -> do
        response <- runSession (getHeader "blocks/45/42caa8f98ec335f71cfbf3f39b878c1c1055d1923280c33bf96dd73af8b621d6/header") app

        simpleStatus response `shouldBe` status200
        simpleBody response `shouldBe` fromString testHeaderHex

      it "returns block header in hex-encoded CBOR given hash is correct but slot is not" $ \app -> do
        response <- runSession (getHeader "blocks/45/42caa8f98ec335f71cfbf3f39b878c1c1055d1923280c33bf96dd73af8b621d6/header") app

        simpleStatus response `shouldBe` status200
        simpleBody response `shouldBe` fromString testHeaderHex

      it "returns 404 given requested block hash does not exist" $ \app -> do
        response <- runSession (getHeader "blocks/295/beff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7/header") app

        simpleStatus response `shouldBe` status404

      it "returns 400 given requested block hash is malformed" $ \app -> do
        response <- runSession (getHeader "blocks/295/malformed/header") app

        simpleStatus response `shouldBe` status400

      it "returns 400 given requested slot is malformed" $ \app -> do
        response <- runSession (getHeader "blocks/not-a-number/eeff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7/header") app

        simpleStatus response `shouldBe` status400

    describe "GET /blocks/:slot/:hash" $ do
      it "returns full block in hex-encoded CBOR given it exists" $ \app -> do
        response <- runSession (getHeader "blocks/45/42caa8f98ec335f71cfbf3f39b878c1c1055d1923280c33bf96dd73af8b621d6") app

        simpleStatus response `shouldBe` status200
        simpleBody response `shouldBe` fromString testBlockHex

      it "returns 404 given requested block hash does not exist" $ \app -> do
        response <- runSession (getHeader "blocks/295/beff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7") app

        simpleStatus response `shouldBe` status404

      it "returns 400 given requested block hash is malformed" $ \app -> do
        response <- runSession (getHeader "blocks/295/malformed") app

        simpleStatus response `shouldBe` status400

      it "returns 400 given requested slot is malformed" $ \app -> do
        response <- runSession (getHeader "blocks/not-a-number/eeff5bd1eeea7fc2ccfc5e8e8b858e35b101eebc3cbe70b80c43502cb1c6e3c7") app

        simpleStatus response `shouldBe` status400

    describe "GET /blocks/:slot/:hash/parent" $ do
      it "returns block header's parent in hex-encoded CBOR given it exists" $ \app -> do
        response <- runSession (getHeader "blocks/45/42caa8f98ec335f71cfbf3f39b878c1c1055d1923280c33bf96dd73af8b621d6/parent") app

        simpleStatus response `shouldBe` status200
        simpleBody response `shouldBe` fromString testParentHex

    describe "GET /snapshots/:slot/:hash" $ do
      it "returns hex-encoded CBOR ledger snapshot at slot/hash given it exists" $ \app -> do
        pendingWith "Snapshot logic may have changed in 10.5.1, under investigation"
        response <- runSession (getHeader "snapshots/16426") app

        simpleStatus response `shouldBe` status200
        simpleBody response `shouldNotBe` ""

      it "returns 404 given no state exists at given slot" $ \app -> do
        response <- runSession (getHeader "snapshots/16421") app

        simpleStatus response `shouldBe` status404

    describe "GET /snapshots" $ do
      it "returns list of points for which snapshots are available" $ \app -> do
        pendingWith "Snapshot logic may have changed in 10.5.1, under investigation"
        response <- runSession (getHeader "snapshots") app

        simpleStatus response `shouldBe` status200
        length <$> decode @[StandardPoint] (simpleBody response) `shouldBe` Just 2160

      it "any element from the list can be retrieved" $ \app -> pendingWith "Snapshot logic may have changed in 10.5.1, under investigation"
      --   property $ monadicIO $ do
      --     response <- run $ runSession (getHeader "snapshots") app
      --     let Just snapshots = decode @[StandardPoint] (simpleBody response)
      --     StandardPoint slot _ <- pick $ elements snapshots

      --     monitor $ counterexample $ show slot
      --     snapshot <- run $ runSession (getHeader $ "snapshots/ " <> Text.pack (show $ asInteger slot)) app
      --     assert $ simpleStatus snapshot == status200

-- | Perform a GET request to the given path and return the response
-- `path` must be absolute, i.e. start with a slash character
getHeader :: Text.Text -> Session SResponse
getHeader path = do
  let req =
        defaultRequest
          { requestMethod = "GET",
            pathInfo = Text.splitOn "/" path
          }
  request req

-- FIXME: The logs are always deleted, but we would like to keep them in case
-- of failures.
-- see https://github.com/hspec/hspec/issues/907
mkApp :: (Application -> IO ()) -> IO ()
mkApp k = do
  withTempDir "test-db-server" $ \dir -> do
    withLogFile (dir </> "db-server.log") $ \hdl ->
      withLog hdl $ \tr ->
        withTestDB (contramap DBLog tr) $ \db -> do
          k $ tracerMiddleware (contramap HttpServerLog tr) $ webApp db
