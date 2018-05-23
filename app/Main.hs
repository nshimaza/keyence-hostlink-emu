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

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Control.Monad.STM
import           Data.ByteString.Char8         (unpack)
import           Data.ByteString.Lazy.Char8    (pack)
import           Data.Foldable
import           Data.Semigroup

import           Network.TcpServer

import           Lib

debugResponse = dummyData "abc\ndef\nghi\njkl"

dummyData :: String -> [String]
dummyData = cycle . map (filter (/= '\r')) . lines

recvLoop :: Transport -> TQueue () -> IO ()
recvLoop peer queue = forever $ transportRecv peer >>= traverse kick . filter (== '\r') . unpack
  where
    kick _ = atomically $ writeTQueue queue ()

sendLoop :: Transport -> TQueue () -> [String] -> IO ()
sendLoop peer queue responses = forM_ responses $ \r -> do
    atomically $ readTQueue queue
    transportSend peer $ pack (r <> "\r\n")

keyenceHostLinkEmuHandler :: [String] -> ThreadMap -> Transport -> IO ()
keyenceHostLinkEmuHandler responses _ peer = newTQueueIO
    >>= \q -> race_ (recvLoop peer q) (sendLoop peer q responses)

startKeyenceHostLinkEmu :: [String] -> IO TcpServer
startKeyenceHostLinkEmu responses = newTcpServer 8501 $ keyenceHostLinkEmuHandler responses

main :: IO ()
main = do
    -- startKeyenceHostlinkEmu debugResponse
    getContents >>= startKeyenceHostLinkEmu . dummyData
    forever $ threadDelay 1000000000
