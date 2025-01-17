{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import qualified Cardano.Tools.DBServer as DBServer
import DBServer.Options (Options (..), optsParser)
import Main.Utf8 (withStdTerminalHandles)
import Options.Applicative
import System.IO (stdout)

main :: IO ()
main = withStdTerminalHandles $
  DBServer.withLog stdout $ \tracer -> do
    cryptoInit
    Options{configurationFile, host, port, databaseDirectory} <- execParser optsParser
    DBServer.run tracer port host configurationFile databaseDirectory
