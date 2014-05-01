{-# LANGUAGE OverloadedStrings  #-}

module Client.ProgressBar(main) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Concurrent
import Control.Monad

import Protocol.ProgressBar

main :: IO ()
main = do
  let send' msg = do
        threadDelay 1000000
        send msg
      go 0 = return ()
      go n = do
        send' (show $ Done (Just ("Done " ++ show n)) 1)
        go (n-1)
      loop n = do
        send' (show $ Init n)
        go n

  loop 10
  loop 15
  loop 20

send :: String -> IO ()
send msg = do
  req <- parseUrl "http://localhost:8888"
  let req' = req { method = "POST"
                 , requestBody = RequestBodyLBS $ BS.pack $
                                 UTF8.encode msg
                 }
  void $ withManager $ httpLbs req'
