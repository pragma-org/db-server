#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , directory
             , filepath
             , shake
             , unix
-}

import Control.Exception (throw)
import Control.Monad (forM_, unless)
import Data.Functor
import Data.Maybe
import Development.Shake
import System.Directory (XdgDirectory (..), getCurrentDirectory, getXdgDirectory)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.FilePath
import System.Info (os)
import System.Posix.User (getRealUserID)

-- | Id of current user
type UID = String

options =
  shakeOptions
    { -- Generate a report that can be opened in a browser to analyze build
      shakeReport = ["report.html"]
    , -- Use all CPUs provided by the platform
      shakeThreads = 0
    , -- Put Shake's database in directory '_build'
      shakeFiles = "_build"
    , shakeVerbosity = Verbose
    , shakeProgress = progressDisplay 5 putStrLn
    , shakeCommandOptions = [EchoStdout True]
    }

install :: IO ()
install = do
  pwd <- getCurrentDirectory
  uid <- show . toInteger <$> getRealUserID
  putStrLn $ "Building db-server in directory " <> pwd <> " as user " <> uid
  args <- getArgs
  withArgs args $ runShake pwd uid

runShake :: FilePath -> UID -> IO ()
runShake pwd uid = shakeArgs options $ do
  let defaultNodeVersion = "10.1.4"
      needHaskellSources = do
        needDirectoryFiles
          "."
          [ "src//*.hs"
          , "test//*.hs"
          , "data//*.*"
          , "app//*.hs"
          ]
        need ["db-server.cabal", "cabal.project"]

      needDependencies = do
        installDir <- liftIO $ getXdgDirectory XdgData ""
        need
          [ installDir </> "lib" </> "libsodium.a"
          , installDir </> "lib" </> "libsecp256k1.a"
          , installDir </> "lib" </> "libblst.a"
          ]

  want ["bin/db-server"]

  "//libsodium.a" %> \lib -> do
    nodeVersion <- getEnvWithDefault defaultNodeVersion "CARDANO_NODE_VERSION"
    installDir <- liftIO $ getXdgDirectory XdgData ""
    cmd_ "scripts/install-libsodium.sh" [installDir, nodeVersion]

  "//libsecp256k1.a" %> \lib -> do
    nodeVersion <- getEnvWithDefault defaultNodeVersion "CARDANO_NODE_VERSION"
    installDir <- liftIO $ getXdgDirectory XdgData ""
    cmd_ "scripts/install-libsecp256k1.sh" [installDir, nodeVersion]

  "//libblst.a" %> \lib -> do
    nodeVersion <- getEnvWithDefault defaultNodeVersion "CARDANO_NODE_VERSION"
    installDir <- liftIO $ getXdgDirectory XdgData ""
    cmd_ "scripts/install-libblst.sh" [installDir, nodeVersion]

  "bin/db-server" %> \bin -> do
    needDependencies
    needHaskellSources
    needDependencies
    installDir <- liftIO $ getXdgDirectory XdgData ""
    let libDirectory = installDir </> "lib"
    liftIO $ removeFiles installDir ["//*.so", "//*.dylib"]
    cmd_ "cabal" ["update"]
    command_
      [AddEnv "PKG_CONFIG_PATH" (libDirectory </> "pkgconfig")]
      "cabal"
      ["build", "all", "--ghc-options", "-L" <> libDirectory <> " -lsodium -lblst -lsecp256k1"]
    command_
      [ AddEnv "LD_LIBRARY_PATH" libDirectory
      , AddEnv "DYLD_FALLBACK_LIBRARY_PATH" libDirectory
      ]
      "cabal"
      ["test", "all"]
    Stdout exePath <- cmd "cabal" ["list-bin", "db-server"]
    dirExists <- doesDirectoryExist "bin"
    unless dirExists $ cmd_ "mkdir" ["bin"]
    cmd_ "cp" [head (lines exePath), bin]

needDirectoryFiles dir patterns =
  need =<< getDirectoryFiles "" ((dir </>) <$> patterns)

main = install
