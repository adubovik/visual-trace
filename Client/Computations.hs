{-# LANGUAGE OverloadedStrings  #-}

module Client.Computations(main) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8
import Text.Printf
import Control.Concurrent
import Control.Monad

import Protocol.ParallelComputation
import Graphics.Gloss.Data.ColorRead(fromColor)
import Graphics.Gloss.Data.Color

mkCommands :: [Command]
mkCommands = [Workunit "Node1" "Wu1" (fromColor red, Just "Start") "Hello world"]

sendTree :: IO ()
sendTree = do
  let sendWithDelay msg = do
        threadDelay 100000
        send msg
  mapM_ (sendWithDelay . show) mkCommands

send :: String -> IO ()
send msg = do
  putStrLn $ printf "Sending %s..." msg
  req <- parseUrl "http://localhost:8888"
  let req' = req { method = "POST"
                 , requestBody = RequestBodyLBS $ BS.pack $
                                 UTF8.encode msg
                 }
  void $ withManager $ httpLbs req'

main :: IO ()
main = sendTree

