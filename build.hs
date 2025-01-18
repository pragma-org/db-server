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
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.FilePath
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
      --    , shakeVerbosity = Diagnostic
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
  let needHaskellSources = do
        needDirectoryFiles
          "."
          [ "src//*.hs"
          , "test//*.hs"
          , "data//*.*"
          , "app//*.hs"
          ]
        need ["db-server.cabal", "cabal.project"]

  want ["bin/db-server"]

  "bin/db-server" %> \bin -> do
    needHaskellSources
    cmd_ "cabal" ["update"]
    cmd_ "cabal" ["build", "all"]
    cmd_ "cabal" ["test", "all"]
    Stdout exePath <- cmd "cabal" ["list-bin", "db-server"]
    dirExists <- doesDirectoryExist "bin"
    unless dirExists $ cmd_ "mkdir" ["bin"]
    cmd_ "cp" [head (lines exePath), bin]

needDirectoryFiles dir patterns =
  need =<< getDirectoryFiles "" ((dir </>) <$> patterns)

main = install
