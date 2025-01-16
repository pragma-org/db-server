{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Tools.DBServer (run, DBServerLog (..), withLog) where

import Cardano.Ledger.Api (StandardCrypto)
import Cardano.Tools.DBAnalyser.Block.Cardano (Args (CardanoBlockArgs))
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Control.Monad (forever)
import Control.Tracer (Tracer (..), contramap, traceWith)
import Data.Aeson (FromJSON, ToJSON, Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word64)
import qualified GHC.Clock
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, statusCode, statusMessage)
import Network.Socket (PortNumber)
import Network.Wai (Application, Middleware, pathInfo, requestMethod, responseLBS, responseStatus)
import qualified Network.Wai.Handler.Warp as Warp
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Util.IOLike (MonadSTM (writeTQueue), atomically, newTQueueIO, readTQueue, withAsync)
import System.IO (hFlush, stdout)

data DBServerLog = HttpServerLog HttpServerLog
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

run :: Tracer IO DBServerLog -> PortNumber -> String -> FilePath -> FilePath -> IO ()
run tracer (fromIntegral -> port) host configurationFile _databaseDirectory = do
    let args = CardanoBlockArgs configurationFile Nothing
    ProtocolInfo{pInfoConfig} <- mkProtocolInfo args
    Warp.runSettings settings $
        tracerMiddleware tr $
            app pInfoConfig
  where
    tr = contramap HttpServerLog tracer
    settings =
        Warp.defaultSettings
            & Warp.setPort port
            & Warp.setHost (fromString host)
            & Warp.setServerName "db-server"
            & Warp.setTimeout 120
            & Warp.setMaximumBodyFlush Nothing
            & Warp.setBeforeMainLoop (traceWith tr HttpServerListening{host, port})

type StandardBlock = CardanoBlock StandardCrypto

app :: TopLevelConfig StandardBlock -> Application
app _config _req send = send response
  where
    response = responseLBS status200 [] "Hello, world!"

-- * Tracing
withLog :: (Tracer IO DBServerLog -> IO ()) -> IO ()
withLog k = do
    logQueue <- newTQueueIO
    let tracer =
            Tracer $ \msg -> do
                time <- getCurrentTime
                let logged =
                        case toJSON msg of
                            Object o ->
                                Object $ o <> KeyMap.fromList [("timestamp", toJSON time)]
                            other -> object ["timestamp" .= time, "log" .= other]
                atomically $ writeTQueue logQueue $ LBS.toStrict $ encode logged <> "\n"

        runLogThread = forever $ do
            msg <- atomically $ readTQueue logQueue
            BS.hPutStr stdout msg
            hFlush stdout

    withAsync runLogThread $
        \_ -> k tracer

tracerMiddleware :: Tracer IO HttpServerLog -> Middleware
tracerMiddleware tr runApp req send = do
    start <- GHC.Clock.getMonotonicTimeNSec
    traceWith tr HttpRequest{path, method}
    runApp req $ \res -> do
        result <- send res
        end <- GHC.Clock.getMonotonicTimeNSec
        let time = mkRequestTime start end
        traceWith tr HttpResponse{status = mkStatus (responseStatus res), time}
        pure result
  where
    method = decodeUtf8 (requestMethod req)
    path = pathInfo req

mkStatus :: Status -> HttpStatus
mkStatus status =
    HttpStatus{code, message}
  where
    code = statusCode status
    message = decodeUtf8 (statusMessage status)

newtype RequestTime = RequestTime {milliseconds :: Word64}
    deriving stock (Eq, Generic, Show)
    deriving newtype (ToJSON, FromJSON)

mkRequestTime :: Word64 -> Word64 -> RequestTime
mkRequestTime start end =
    RequestTime $ (end - start) `div` 1_000_000

data HttpStatus = HttpStatus {code :: Int, message :: Text}
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

data HttpServerLog
    = HttpServerListening {host :: String, port :: Int}
    | HttpRequest {path :: [Text], method :: Text}
    | HttpResponse {status :: HttpStatus, time :: RequestTime}
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
