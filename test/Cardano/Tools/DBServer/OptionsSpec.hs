module Cardano.Tools.DBServer.OptionsSpec where

import Cardano.Tools.DBServer.Options (Options (..), ServeOptions (..), parseArgs)
import Data.String (IsString (..))
import Network.Wai.Handler.Warp (HostPreference)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), elements, (===))

spec :: Spec
spec = do
  describe "optsParser" $ do
    prop "can parse 'serve --host ... --port ...' options" $ \(SomeHost someHost) somePort ->
      parseArgs ["serve", "--host", someHost, "--port", show somePort] === Right (Serve $ ServeOptions "db" somePort someHost "config.json")

    it "parses host catch-all expressions" $ do
      -- NOTE: for some reason the constructors of HostPreference are not
      -- exported so we need to show them to be able to parse them 🤡
      show (fromString "!4" :: HostPreference) `shouldBe` "HostIPv4Only"
      show (fromString "*" :: HostPreference) `shouldBe` "HostAny"
      show (fromString "0.0.0.0" :: HostPreference) `shouldBe` "Host \"0.0.0.0\""

newtype SomeHost = SomeHost String
  deriving (Show, Eq)

instance Arbitrary SomeHost where
  arbitrary = SomeHost <$> elements ["1.2.3.4", "localhost", "0.0.0.0"]
