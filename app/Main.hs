{-# LANGUAGE OverloadedStrings #-}

{-
    Copyright (c) 2018 Cisco and/or its affiliates.

    This software is licensed to you under the terms of the Cisco Sample
    Code License, Version 1.0 (the "License"). You may obtain a copy of the
    License at

               https://developer.cisco.com/docs/licenses

    All use of the material herein must be in accordance with the terms of
    the License. All rights not expressly granted by the License are
    reserved. Unless required by applicable law or agreed to separately in
    writing, software distributed under the License is distributed on an "AS
    IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
    or implied.
-}
module Main where

import           Conduit
import           Control.Applicative              ((<|>))
import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async         (race_)
import           Control.Concurrent.STM.TQueue    (TQueue, newTQueueIO,
                                                   readTQueue, writeTQueue)
import           Control.Monad                    (forever, unless)
import           Control.Monad.STM                (atomically)
import           Data.Attoparsec.ByteString       (word8)
import           Data.Attoparsec.ByteString.Char8 (Parser, decimal, option,
                                                   string)
import qualified Data.ByteString                  as B (null)
import           Data.ByteString.Char8            (unpack)
import           Data.ByteString.Lazy             (ByteString)
import           Data.ByteString.Lazy.Char8       (pack)
import           Data.Conduit                     (await, yield)
import           Data.Conduit.Attoparsec          (ParseError, PositionRange,
                                                   conduitParserEither)
import           Data.Default.Class
import           Data.Int                         (Int16, Int32)
import           Data.Word                        (Word16, Word32, Word8)
import           Text.Printf                      (printf)

import           Network.TcpServer                (TcpServerConfig (..),
                                                   Transport, newTcpServer,
                                                   recv, send)

import           Lib

{-
    HostLink command parser
-}
data DeviceType = DevDM deriving (Show)
newtype DeviceAddress = DeviceAddress Word16 deriving (Show)
data DeviceId = DeviceId DeviceType DeviceAddress deriving (Show)
data DataFormat = FormatWord16 | FormatInt16 | FormatWord32 | FormatInt32 | FormatHex16 deriving (Show)

data HostLinkCommand
    = ComRD
        { commRdDevice :: DeviceId
        , commRdFormat :: DataFormat
        }
    | ComRDS
        { commRdsDevice :: DeviceId
        , commRdsFormat :: DataFormat
        , commRdsCount  :: Int
        }
    deriving (Show)

whiteSpace :: Parser Word8
whiteSpace = word8 0x20     -- ' '

carriageReturn :: Parser Word8
carriageReturn = word8 0x0d -- '\r'

lineFeed :: Parser Word8
lineFeed = word8 0x0a       -- '\n'

deviceId :: Parser DeviceId
deviceId = do
    string "DM"
    addr <- decimal
    pure $ DeviceId DevDM $ DeviceAddress addr

dataFormat :: Parser DataFormat
dataFormat =
        (string ".U" *> pure FormatWord16)
    <|> (string ".S" *> pure FormatInt16)
    <|> (string ".D" *> pure FormatWord32)
    <|> (string ".L" *> pure FormatInt32)
    <|> (string ".H" *> pure FormatHex16)

readCount :: Parser Word16
readCount = do
    count <- decimal
    pure count

commRD :: Parser HostLinkCommand
commRD = do
    string "RD"
    whiteSpace
    devId <- deviceId
    format <- option FormatWord16 dataFormat
    carriageReturn
    option 0 lineFeed
    pure $ ComRD devId format

commRDS :: Parser HostLinkCommand
commRDS = do
    string "RDS"
    whiteSpace
    devId <- deviceId
    format <- option FormatWord16 dataFormat
    whiteSpace
    count <- decimal
    carriageReturn
    option 0 lineFeed
    pure $ ComRDS devId format count

hostLinkCommand :: Parser HostLinkCommand
hostLinkCommand = commRD <|> commRDS


{-
    Memory representation of dummy response
-}
type Response = [[String]]

responses :: String -> Response
responses = cycle . map (cycle . words) . lines

{-
    TCP server of the emulator
-}
serverConfig :: TcpServerConfig
serverConfig = def { tcpServerConfigPort = 8501 }

type ConduitParserResult = Either ParseError (PositionRange, HostLinkCommand)

recvLoop :: Transport t => t -> TQueue ConduitParserResult -> IO ()
recvLoop peer queue = runConduit $ src .| conduitParserEither hostLinkCommand .| sink
  where
    src = do
        msg <- liftIO (recv peer)
        unless (B.null msg) $ do
            yield msg
            src
    sink = do
        maybeParsed <- await
        liftIO $ print maybeParsed
        case maybeParsed of
            Just parsed     -> (liftIO . atomically . writeTQueue queue) parsed *> sink
            Nothing         -> liftIO $ threadDelay 1000000000

formatResponse :: DataFormat -> Int -> [String] -> ByteString
formatResponse format count = toBS . map (formatNum format) . take count
  where
    formatNum :: DataFormat -> String -> String
    formatNum FormatWord16 s = printf "%05d" (read s :: Word16)
    formatNum FormatWord32 s = printf "%010d" (read s :: Word32)
    formatNum FormatInt16 s  = printf "%+06d" (read s :: Int16)
    formatNum FormatInt32 s  = printf "%+011d" (read s :: Int32)
    formatNum FormatHex16 s  = printf "%04x" (read s :: Int16)

    toBS :: [String] -> ByteString
    toBS = pack . unwords

sendLoop :: Transport t => t -> TQueue ConduitParserResult -> Response -> IO ()
sendLoop peer queue responses = go responses
  where
    go (r:rs)   = do
        parserResult <- atomically $ readTQueue queue
        case parserResult of
            Right (_, command)  -> case command of
                ComRD _ format  -> do
                    send peer $ formatResponse format 1 r <> "\r\n"
                    go rs
                ComRDS _ format count   -> do
                    send peer $ formatResponse format count r <> "\r\n"
                    go rs
            Left _  -> send peer "Parse error\r\n"

    go [] = pure ()

keyenceHostLinkEmuHandler :: Transport t => Response -> t -> IO ()
keyenceHostLinkEmuHandler response peer = newTQueueIO
    >>= \q -> race_ (recvLoop peer q) (sendLoop peer q response)

startKeyenceHostLinkEmu :: Response -> IO ()
startKeyenceHostLinkEmu response = newTcpServer serverConfig $ keyenceHostLinkEmuHandler response

main :: IO ()
main = do
    getContents >>= startKeyenceHostLinkEmu . responses
    forever $ threadDelay 1000000000
