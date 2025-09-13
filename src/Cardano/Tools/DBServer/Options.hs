{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tools.DBServer.Options where

import qualified Network.Socket as Socket
import Options.Applicative
  ( Parser,
    ParserInfo,
    ParserResult (..),
    auto,
    command,
    defaultPrefs,
    execParserPure,
    fullDesc,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    option,
    progDesc,
    showDefault,
    strOption,
    value,
  )
import Test.QuickCheck (Arbitrary (..), elements)

-- * Options

data Options = Serve ServeOptions | Query QueryOptions
  deriving stock (Eq, Show)

data QueryOptions = QueryOptions
  { databaseDirectory :: FilePath,
    configurationFile :: FilePath,
    query :: Query
  }
  deriving stock (Eq, Show)

type Query = String

data ServeOptions = ServeOptions
  { databaseDirectory :: FilePath,
    port :: Socket.PortNumber,
    host :: String,
    configurationFile :: FilePath
  }
  deriving stock (Eq, Show)

optsParser :: ParserInfo Options
optsParser =
  info (helper <*> commandsParser) $ fullDesc <> progDesc desc
  where
    desc = "Serve a ChainDB directory over HTTP"

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command
        "serve"
        ( info
            (Serve <$> parseServeOptions)
            ( progDesc
                "Serve a ChainDB directory over HTTP."
            )
        )
        <> command
          "query"
          ( info
              (Query <$> parseQueryOptions)
              ( progDesc
                  "Query a ChainDB directory."
              )
          )
    )

parseServeOptions :: Parser ServeOptions
parseServeOptions = do
  databaseDirectory <-
    strOption $
      mconcat
        [ long "db",
          help "Path to the chain DB",
          value "db",
          metavar "PATH"
        ]
  port <-
    option auto $
      mconcat
        [ long "port",
          help "Port to listen on",
          value 9003,
          showDefault
        ]
  host <-
    strOption $
      mconcat
        [ long "host",
          help "host to listen on",
          value "127.0.0.1",
          showDefault
        ]
  configurationFile <-
    strOption $
      mconcat
        [ long "config",
          help "Path to cardano-node config file. Note this file should reference existing genesis files.",
          metavar "PATH",
          value "config.json",
          showDefault
        ]
  pure ServeOptions {databaseDirectory, port, host, configurationFile}

parseQueryOptions :: Parser QueryOptions
parseQueryOptions = do
  databaseDirectory <-
    strOption $
      mconcat
        [ long "db",
          help "Path to the chain DB",
          value "db",
          metavar "PATH"
        ]
  configurationFile <-
    strOption $
      mconcat
        [ long "config",
          help "Path to cardano-node config file. Note this file should reference existing genesis files.",
          metavar "PATH",
          value "config.json",
          showDefault
        ]
  query <-
    strOption $
      mconcat
        [ long "query",
          help "Query to execute",
          metavar "QUERY"
        ]
  pure QueryOptions {databaseDirectory, configurationFile, query}

parseArgs :: [String] -> Either String Options
parseArgs args =
  case execParserPure defaultPrefs optsParser args of
    Success opts -> Right opts
    Failure err -> Left (show err)
    CompletionInvoked _ -> Left "Unexpected completion invocation"

instance Arbitrary Socket.PortNumber where
  arbitrary = fromIntegral @Int <$> elements [1024 .. 65535]
