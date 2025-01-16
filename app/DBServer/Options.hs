{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module DBServer.Options (Options (..), optsParser) where

import qualified Network.Socket as Socket
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    showDefault,
    strOption,
    value,
 )

-- * Options
data Options = Options
    { databaseDirectory :: FilePath
    , port :: Socket.PortNumber
    , host :: String
    , configurationFile :: FilePath
    }

optsParser :: ParserInfo Options
optsParser =
    info (helper <*> parse) $ fullDesc <> progDesc desc
  where
    desc = "Serve a ChainDB directory over HTTP"

    parse :: Parser Options
    parse = do
        databaseDirectory <-
            strOption $
                mconcat
                    [ long "db"
                    , help "Path to the chain DB"
                    , value "db"
                    , metavar "PATH"
                    ]
        port <-
            option auto $
                mconcat
                    [ long "port"
                    , help "Port to listen on"
                    , value 9003
                    , showDefault
                    ]
        host <-
            option auto $
                mconcat
                    [ long "host"
                    , help "host to listen on"
                    , value "127.0.0.1"
                    , showDefault
                    ]
        configurationFile <-
            strOption $
                mconcat
                    [ long "config"
                    , help "Path to cardano-node config file. Note this file should reference existing genesis files."
                    , metavar "PATH"
                    , value "config.json"
                    , showDefault
                    ]
        pure Options{databaseDirectory, port, host, configurationFile}
