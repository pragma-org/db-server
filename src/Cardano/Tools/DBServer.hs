{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tools.DBServer where

import Cardano.Tools.DB (DBError (..), Result (..), StandardBlock, getBlock, getHeader, getParent, getSnapshot, listSnapshots, makePoint, makeSlot, toBytestring, withDB)
import Control.Monad (forever)
import Control.Tracer (Tracer (..), contramap, traceWith)
import Data.Aeson (FromJSON (..), ToJSON, Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16.Lazy as LHex
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word64)
import qualified GHC.Clock
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status400, status404, statusCode, statusMessage)
import Network.Socket (PortNumber)
import Network.Wai (Application, Middleware, pathInfo, requestMethod, responseLBS, responseStatus)
import qualified Network.Wai.Handler.Warp as Warp
import Ouroboros.Consensus.Storage.ChainDB (ChainDB, TraceEvent)
import Ouroboros.Consensus.Util.IOLike (MonadSTM (writeTQueue), atomically, newTQueueIO, readTQueue, withAsync)
import System.IO (Handle, hFlush)

data DBServerLog = HttpServerLog HttpServerLog | DBLog (TraceEvent StandardBlock)
  deriving stock (Eq, Show, Generic)

instance ToJSON DBServerLog where
  toJSON = \case
    HttpServerLog l -> object ["tag" .= ("HttpServer" :: Text), "log" .= toJSON l]
    DBLog l -> object ["tag" .= ("ChainDB" :: Text), "log" .= show l]

run :: Tracer IO DBServerLog -> PortNumber -> String -> FilePath -> FilePath -> IO ()
run tracer (fromIntegral -> port) host configurationFile databaseDirectory = do
  withDB configurationFile databaseDirectory (contramap DBLog tracer) $ \db ->
    Warp.runSettings settings $
      tracerMiddleware tr $
        webApp db
  where
    tr = contramap HttpServerLog tracer
    settings =
      Warp.defaultSettings
        & Warp.setPort port
        & Warp.setHost (fromString host)
        & Warp.setServerName "db-server"
        & Warp.setTimeout 120
        & Warp.setMaximumBodyFlush Nothing
        & Warp.setBeforeMainLoop (traceWith tr (HttpListenLog (ListenEvent {host, port})))

webApp :: ChainDB IO StandardBlock -> Application
webApp db req send =
  case pathInfo req of
    ["snapshots"] -> handleGetSnapshots
    ["snapshots", slot] -> handleGetSnapshot slot
    ["blocks", slot, hash] -> handleGetBlock slot hash
    ["blocks", slot, hash, "header"] -> handleGetHeader slot hash
    ["blocks", slot, hash, "parent"] -> handleGetParent slot hash
    _ -> send responseNotFound
  where
    responseNotFound = responseLBS status404 [] ""
    responseBadRequest = responseLBS status400 []
    responseOk = responseLBS status200 [("content-type", "application/json")]

    handleGetSnapshots =
      listSnapshots db >>= \snapshotsPoints ->
        send $ responseOk (encode snapshotsPoints)

    handleWithPoint action slot hash =
      case makePoint slot hash of
        Nothing -> send $ responseBadRequest "Malformed hash or slot"
        Just point ->
          action db point >>= \case
            Err NotFound -> send responseNotFound
            Err err -> send $ responseBadRequest ("Bad query: " <> toBytestring err)
            Found result -> send $ responseOk (LHex.encode result)

    handleGetHeader = handleWithPoint getHeader

    handleGetSnapshot slot =
      case makeSlot slot of
        Nothing -> send $ responseBadRequest "Malformed slot"
        Just slot' ->
          getSnapshot db slot' >>= \case
            Err NotFound -> send responseNotFound
            Err err -> send $ responseBadRequest ("Bad query: " <> toBytestring err)
            Found snapshot -> send $ responseOk (LHex.encode snapshot)

    handleGetParent = handleWithPoint getParent

    handleGetBlock = handleWithPoint getBlock

-- * Tracing

withLog :: (ToJSON log) => Handle -> (Tracer IO log -> IO a) -> IO a
withLog hdl k = do
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
        BS.hPutStr hdl msg
        hFlush hdl

  withAsync runLogThread $
    \_ -> k tracer

tracerMiddleware :: Tracer IO HttpServerLog -> Middleware
tracerMiddleware tr runApp req send = do
  start <- GHC.Clock.getMonotonicTimeNSec
  traceWith tr (HttpRequestLog (RequestEvent {path, method}))
  runApp req $ \res -> do
    result <- send res
    end <- GHC.Clock.getMonotonicTimeNSec
    let time = mkRequestTime start end
    traceWith tr (HttpResponseLog (ResponseEvent {status = mkStatus (responseStatus res), time}))
    pure result
  where
    method = decodeUtf8 (requestMethod req)
    path = pathInfo req

mkStatus :: Status -> HttpStatus
mkStatus status =
  HttpStatus {code, message}
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

data HttpServerLog =
  HttpListenLog ListenEvent
  | HttpRequestLog RequestEvent
  | HttpResponseLog ResponseEvent
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ListenEvent = ListenEvent {host :: String, port :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
data RequestEvent = RequestEvent {path :: [Text], method :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
data ResponseEvent = ResponseEvent {status :: HttpStatus, time :: RequestTime}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

