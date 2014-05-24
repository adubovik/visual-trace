{-# LANGUAGE
   OverloadedStrings
 , RecordWildCards
 #-}

module VisualTrace.Client
 ( send
 , sendWithConfig
 , delaySec
 ) where

import Control.Concurrent
import Control.Monad
import Text.Printf

import qualified Network.HTTP.Conduit as HTTP
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8

import VisualTrace.HTTPConfig

delaySec :: Float -> IO ()
delaySec sec = threadDelay (round $ sec * 10**6)

sendWithConfig :: Show a => HTTPConfig -> a -> IO ()
sendWithConfig HTTPConfig{..} = send httpHost httpPort

send :: Show a => String -> Int -> a -> IO ()
send host port msg = do
  req <- HTTP.parseUrl $ printf "http://%s:%d" host port
  let req' = req { HTTP.method = "POST"
                 , HTTP.requestBody = HTTP.RequestBodyLBS $ BS.pack $
                                      UTF8.encode $ show msg
                 }
  -- TODO: handle exceptions...
  void $ HTTP.withManager $ HTTP.httpLbs req'
