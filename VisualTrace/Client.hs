{-# LANGUAGE
   OverloadedStrings
 #-}

module VisualTrace.Client
 ( send
 , sendWithDelay
 ) where

import qualified Network.HTTP.Conduit as HTTP
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8
import Text.Printf

import Control.Concurrent
import Control.Monad

sendWithDelay :: Show a => Float -> String -> Int -> a -> IO ()
sendWithDelay sec host port msg = do
  threadDelay (round $ sec * 10**6)
  send host port msg

send :: Show a => String -> Int -> a -> IO ()
send host port msg = do
  req <- HTTP.parseUrl $ printf "http://%s:%d" host port
  let req' = req { HTTP.method = "POST"
                 , HTTP.requestBody = HTTP.RequestBodyLBS $ BS.pack $
                                      UTF8.encode $ show msg
                 }
  void $ HTTP.withManager $ HTTP.httpLbs req'
