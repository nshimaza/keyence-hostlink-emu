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

keyenceUplinkEmuHandler :: [String] -> ThreadMap -> Transport -> IO ()
keyenceUplinkEmuHandler responses _ peer = newTQueueIO
    >>= \q -> race_ (recvLoop peer q) (sendLoop peer q responses)

startKeyenceUplinkEmu :: [String] -> IO TcpServer
startKeyenceUplinkEmu responses = newTcpServer 8501 $ keyenceUplinkEmuHandler responses

main :: IO ()
main = do
    -- startKeyenceUplinkEmu debugResponse
    getContents >>= startKeyenceUplinkEmu . dummyData
    forever $ threadDelay 1000000000
