module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Char8  (unpack)
import Data.ByteString.Lazy.Char8   (pack)
import Data.Foldable
import Data.Semigroup
import Network.TcpServer


import Lib

debugResponse = dummyData "abc\ndef\nghi\njkl"

dummyData :: String -> [String]
dummyData = cycle . map (filter (/= '\r')) . lines

recvLoop :: Transport -> TQueue () -> IO ()
recvLoop peer queue = go
  where
    go = do
        msg <- transportRecv peer
        traverse kick . filter (== '\r') . unpack $ msg
        go

    kick _ = atomically $ writeTQueue queue ()

sendLoop :: Transport -> TQueue () -> [String] -> IO ()
sendLoop peer queue responses = go responses
  where
    go (r:rs) = do
        _ <- atomically $ readTQueue queue
        transportSend peer $ pack (r <> "\r\n")
        go rs

keyenceUplinkEmuHandler :: [String] -> ThreadMap -> Transport -> IO ()
keyenceUplinkEmuHandler responses _ peer = do
    queue <- newTQueueIO
    race_ (recvLoop peer queue) (sendLoop peer queue responses)

startKeyenceUplinkEmu :: [String] -> IO TcpServer
startKeyenceUplinkEmu responses = newTcpServer 8501 $ keyenceUplinkEmuHandler responses

main :: IO ()
main = do
    -- startKeyenceUplinkEmu debugResponse
    getContents >>= startKeyenceUplinkEmu . dummyData
    forever $ threadDelay 1000000000
