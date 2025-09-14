{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.TestHelper where

import Cardano.Tools.DB (DB, DBTrace, withDB)
import Control.Exception (catch, onException, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Tracer (Tracer)
import GHC.IO.Exception (IOErrorType (UnsatisfiedConstraints), ioe_type)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hPutStrLn, hSetBuffering, stderr, withFile)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (os)
import System.Posix (mkstemp)
import Data.String (IsString)

-- | Create a unique directory in the caonical, system-specific temporary path,
-- e.g. in /tmp.
createTempDir :: (MonadIO m) => String -> m FilePath
createTempDir template = liftIO $ do
  tmpDir <- case os of
    "darwin" -> pure "/tmp"
    _ -> getCanonicalTemporaryDirectory
  createTempDirectory tmpDir template

-- | Create a temporary directory for the given 'action' to use. The directory
-- is removed if and only if the action completes successfuly.
withTempDir :: (MonadIO m) => String -> (FilePath -> m r) -> m r
withTempDir baseName action = do
  tmpDir <- createTempDir baseName
  res <- action tmpDir
  liftIO $ cleanup 0 tmpDir
  pure res
  where
    cleanup (maxAttempts :: Word) dir =
      removePathForcibly dir
        `catch` ( \e -> case ioe_type e of
                    UnsatisfiedConstraints ->
                      if maxAttempts < 3
                        then cleanup (succ maxAttempts) dir
                        else throwIO e
                    _ ->
                      throwIO e
                )

-- | Open given log file non-buffered in append mode and print a message with
-- filepath to @stderr@ on exceptions.
withLogFile :: FilePath -> (Handle -> IO a) -> IO a
withLogFile filepath io = do
  createDirectoryIfMissing True (takeDirectory filepath)
  withFile filepath AppendMode (\out -> hSetBuffering out NoBuffering >> io out)
    `onException` hPutStrLn stderr ("Logfile written to: " <> filepath)

createLog :: FilePath -> IO (FilePath, Handle)
createLog dir = mkstemp (dir </> "db-server.log")

withTestDB ::
  Tracer IO DBTrace ->
  (DB -> IO a) ->
  IO a
withTestDB = withDB testConfigFile testDatabaseDir

testConfigFile :: FilePath
testConfigFile = "test-data/config/config.json"

testDatabaseDir :: FilePath
testDatabaseDir = "test-data/test-db"

testHeaderHex :: IsString a => a
testHeaderHex = "828a0c1901275820e5b1b73c5e7ee253e87d02a420352b8f6250b76688acd06dc9a9b0f58532e1035820e41015edc7b39489226d27c51dbe84c636466b3e29758a95445297614a8050bf582006a90f0597762346dd9eee0017623aca4745105f75b6d0d44355b2639537293482584076b951f29f104902b0e3090cd36ae8ff73980e29e6ac7c770d4dfa309870b2510088f60943f9a5108b0eb438afc19ec4bed0a85a3dc612881cc3383e62f604255850f9b7d5afac819358cb68b1a4f941c8c77c43beb0fa697840ef3743b6059db749a7d8974693dafd67ce5ba4353bb0351d82a44cb77c875def59895709d59e3abd370dec6d85f14c4ab6a5e64d17e4fd0704582029571d16f081709b3c48651860077bebf9340abb3fc7133443c54f1f5a5edcf1845820a5ae7caf7a79b7f750d3d6da9a31d6523bdc0b99cc9dbfbdc11122e3ae07e8280000584071c1947b93fac5684a327a102f522d7b31daccfe8ef69ed0c36ed4618910245756bfe607b5a2bf7725045564b77ee18bfd7ed086b957d856a5491b51fbaedf06820a005901c01a462000cc7c061368fe46efb2974b74a390e9dc4ad5a243ff7f05d729b8bb6fd9177a2f9dd86f649bfed9eece9b83594253b6c705bacc1460fab79840907a05de8e5249591afc25d214c024eac3c1186c26136a8719ca647c3c554aff75301df40a7243f0cea69d0da41b0edd95c35cc6644a433e1a59898f70a88b9578635cb4ee2e7a940b2aa19d596f5b160abc0f83c66cd8c26d8f7226f4556d4a406e0b978df024d42a1a9236d58e8c64733aae1ee6e3258a27bfaf060b6c2913fc9bab1cb543b0b471a48af5e367b75d856a36f5899c8019f91d321c22d012ee466e509d49ca12ed800448ad43ee1575de56abad60d0cd1d2bb9b541573504040c3b4943c077e1127e25a0bb1fb8549c503b519f01f6092a3d3452341da2fb8687e07b340575532fe529cadd9701c300770930c4da09feed3a7f9b4d1253efe0fd1dc05e380763e756b7f6b04d45d45e61aba849736babb9224adbf27a8880f1ecc23dd0bbe61a5b73fa269cc100bf3f6cbd17163f31d38aa22db320d37cbb767821dee4500627980856833e796e4435768172cb98b8b33ed5970a92ab3f046050c9f5aeeafa151f9d11b93c425b68cace42f87c51dee5f0a38071b3a8da23743d699c"

testParentHash :: IsString a => a
testParentHash = "e5b1b73c5e7ee253e87d02a420352b8f6250b76688acd06dc9a9b0f58532e103"

testParentHex :: IsString a => a
testParentHex = "828a0b19010b5820c02836ece740e898371eec4b583dcbc62d1e6f6048fbab2b2538ed48fc7c798b5820e41015edc7b39489226d27c51dbe84c636466b3e29758a95445297614a8050bf582006a90f0597762346dd9eee0017623aca4745105f75b6d0d44355b26395372934825840ca4c710be92db163e35d48064ea0866d37772bec0026dfb68d5cbce7f8e738f36c8b033ca1e46a08607289fa0dd4cbca55cda6f696e4241c8b450a942c90ebc45850748daafccf0cad01f2245d858ff95998234fee511c6038f4edb1b0f894ec387e16eb6c36d8fd750c3caa71fd5657dae2c5a3a0c5d05ca6de78b0deb4d9235ad117b507f5867771add6546757114e1a0704582029571d16f081709b3c48651860077bebf9340abb3fc7133443c54f1f5a5edcf1845820a5ae7caf7a79b7f750d3d6da9a31d6523bdc0b99cc9dbfbdc11122e3ae07e8280000584071c1947b93fac5684a327a102f522d7b31daccfe8ef69ed0c36ed4618910245756bfe607b5a2bf7725045564b77ee18bfd7ed086b957d856a5491b51fbaedf06820a005901c0b84aeb1fc570ca478bb9aa51ca234be2b0ce19798d01f2e5e5e914ab4d80831f29687ebd441ce7a78a84904eb45ccbfa62143cf8e6905e89b9b289d0e9a2b509de8e5249591afc25d214c024eac3c1186c26136a8719ca647c3c554aff75301df40a7243f0cea69d0da41b0edd95c35cc6644a433e1a59898f70a88b9578635cb4ee2e7a940b2aa19d596f5b160abc0f83c66cd8c26d8f7226f4556d4a406e0b978df024d42a1a9236d58e8c64733aae1ee6e3258a27bfaf060b6c2913fc9bab1cb543b0b471a48af5e367b75d856a36f5899c8019f91d321c22d012ee466e509d49ca12ed800448ad43ee1575de56abad60d0cd1d2bb9b541573504040c3b4943c077e1127e25a0bb1fb8549c503b519f01f6092a3d3452341da2fb8687e07b340575532fe529cadd9701c300770930c4da09feed3a7f9b4d1253efe0fd1dc05e380763e756b7f6b04d45d45e61aba849736babb9224adbf27a8880f1ecc23dd0bbe61a5b73fa269cc100bf3f6cbd17163f31d38aa22db320d37cbb767821dee4500627980856833e796e4435768172cb98b8b33ed5970a92ab3f046050c9f5aeeafa151f9d11b93c425b68cace42f87c51dee5f0a38071b3a8da23743d699c"

testBlockHex :: IsString a => a
testBlockHex = "820685828a0c1901275820e5b1b73c5e7ee253e87d02a420352b8f6250b76688acd06dc9a9b0f58532e1035820e41015edc7b39489226d27c51dbe84c636466b3e29758a95445297614a8050bf582006a90f0597762346dd9eee0017623aca4745105f75b6d0d44355b2639537293482584076b951f29f104902b0e3090cd36ae8ff73980e29e6ac7c770d4dfa309870b2510088f60943f9a5108b0eb438afc19ec4bed0a85a3dc612881cc3383e62f604255850f9b7d5afac819358cb68b1a4f941c8c77c43beb0fa697840ef3743b6059db749a7d8974693dafd67ce5ba4353bb0351d82a44cb77c875def59895709d59e3abd370dec6d85f14c4ab6a5e64d17e4fd0704582029571d16f081709b3c48651860077bebf9340abb3fc7133443c54f1f5a5edcf1845820a5ae7caf7a79b7f750d3d6da9a31d6523bdc0b99cc9dbfbdc11122e3ae07e8280000584071c1947b93fac5684a327a102f522d7b31daccfe8ef69ed0c36ed4618910245756bfe607b5a2bf7725045564b77ee18bfd7ed086b957d856a5491b51fbaedf06820a005901c01a462000cc7c061368fe46efb2974b74a390e9dc4ad5a243ff7f05d729b8bb6fd9177a2f9dd86f649bfed9eece9b83594253b6c705bacc1460fab79840907a05de8e5249591afc25d214c024eac3c1186c26136a8719ca647c3c554aff75301df40a7243f0cea69d0da41b0edd95c35cc6644a433e1a59898f70a88b9578635cb4ee2e7a940b2aa19d596f5b160abc0f83c66cd8c26d8f7226f4556d4a406e0b978df024d42a1a9236d58e8c64733aae1ee6e3258a27bfaf060b6c2913fc9bab1cb543b0b471a48af5e367b75d856a36f5899c8019f91d321c22d012ee466e509d49ca12ed800448ad43ee1575de56abad60d0cd1d2bb9b541573504040c3b4943c077e1127e25a0bb1fb8549c503b519f01f6092a3d3452341da2fb8687e07b340575532fe529cadd9701c300770930c4da09feed3a7f9b4d1253efe0fd1dc05e380763e756b7f6b04d45d45e61aba849736babb9224adbf27a8880f1ecc23dd0bbe61a5b73fa269cc100bf3f6cbd17163f31d38aa22db320d37cbb767821dee4500627980856833e796e4435768172cb98b8b33ed5970a92ab3f046050c9f5aeeafa151f9d11b93c425b68cace42f87c51dee5f0a38071b3a8da23743d699c8080a080"
