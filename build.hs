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
  let dllExtension = case os of
        "darwin" -> "dylib"
        _ -> "so"
  withArgs args $ runShake dllExtension pwd uid

runShake :: String -> FilePath -> UID -> IO ()
runShake so pwd uid = shakeArgs options $ do
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
          [ installDir </> "lib" </> "libsodium" <.> so
          , installDir </> "lib" </> "libsecp256k1" <.> so
          , installDir </> "lib" </> "libblst.a"
          ]

  want ["bin/db-server"]

  ("//libsodium" <.> so) %> \lib -> do
    nodeVersion <- getEnvWithDefault defaultNodeVersion "CARDANO_NODE_VERSION"
    installDir <- liftIO $ getXdgDirectory XdgData ""
    cmd_ "scripts/install-libsodium.sh" [installDir, nodeVersion]

  ("//libsecp256k1" <.> so) %> \lib -> do
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
    cmd_ "cabal" ["update"]
    command_ [AddEnv "PKG_CONFIG_PATH" (installDir </> "lib" </> "pkgconfig")] "cabal" ["build", "all"]
    command_
      [ AddEnv "LD_LIBRARY_PATH" (installDir </> "lib")
      , AddEnv "DYLD_FALLBACK_LIBRARY_PATH" (installDir </> "lib")
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
