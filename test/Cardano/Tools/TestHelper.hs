{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.TestHelper where

import Control.Exception (catch, onException, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.IO.Exception (IOErrorType (UnsatisfiedConstraints), ioe_type)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (takeDirectory)
import System.IO (BufferMode (..), Handle, IOMode (..), hPutStrLn, hSetBuffering, stderr, withFile)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (os)

-- | Create a unique directory in the caonical, system-specific temporary path,
-- e.g. in /tmp.
createTempDir :: MonadIO m => String -> m FilePath
createTempDir template = liftIO $ do
  tmpDir <- case os of
    "darwin" -> pure "/tmp"
    _ -> getCanonicalTemporaryDirectory
  createTempDirectory tmpDir template

-- | Create a temporary directory for the given 'action' to use. The directory
-- is removed if and only if the action completes successfuly.
withTempDir :: MonadIO m => String -> (FilePath -> m r) -> m r
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
