{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import qualified Cardano.Tools.DBQuery as DBQuery
import qualified Cardano.Tools.DBServer as DBServer
import Cardano.Tools.DBServer.Options (Options (..), QueryOptions (..), ServeOptions (..), optsParser)
import Control.Tracer (nullTracer)
import Main.Utf8 (withStdTerminalHandles)
import Options.Applicative
import System.IO (stdout)

main :: IO ()
main = withStdTerminalHandles $
  DBServer.withLog stdout $ \tracer -> do
    cryptoInit
    execParser optsParser >>= \case
      Serve ServeOptions {configurationFile, host, port, databaseDirectory} ->
        DBServer.run tracer port host configurationFile databaseDirectory
      Query QueryOptions {configurationFile, databaseDirectory, query} ->
        DBQuery.runQuery nullTracer configurationFile databaseDirectory query
