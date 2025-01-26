module Cardano.Tools.DBServer.OptionsSpec where

import Cardano.Tools.DBServer.Options (Options (..), parseArgs)
import Data.String (IsString (..))
import Network.Wai.Handler.Warp (HostPreference)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "optsParser" $ do
    it "can parse --host options" $ do
      parseArgs ["--host", "0.0.0.0"] `shouldBe` Right (Options "db" 9003 "0.0.0.0" "config.json")

    it "parses host catch-all expressions" $ do
      -- NOTE: for some reason the constructors of HostPreference are not
      -- exported so we need to show them to be able to parse them 🤡
      show (fromString "!4" :: HostPreference) `shouldBe` "HostIPv4Only"
      show (fromString "*" :: HostPreference) `shouldBe` "HostAny"
      show (fromString "0.0.0.0" :: HostPreference) `shouldBe` "Host \"0.0.0.0\""
