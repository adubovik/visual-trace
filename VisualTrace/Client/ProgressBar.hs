{-# LANGUAGE OverloadedStrings  #-}

module VisualTrace.Client.ProgressBar(main) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Concurrent
import Control.Monad

import VisualTrace.Protocol.ProgressBar

type ProgressState = Map.Map Int (Int,Int)

progressStep :: Int -> ProgressState -> IO (ProgressState, Bool)
progressStep curr bounds
  | curr == Map.size bounds
  = return (bounds, False)
  | val == limit = do
    let bounds' = Map.adjust (const (0,limit)) curr bounds
    sendDelay $ Init (show curr) Nothing
    progressStep (curr+1) bounds'
  | otherwise = do
    let bounds' = Map.adjust (const (val+1,limit)) curr bounds
    sendDelay $ Done (show curr) (Just $ "Done " ++ show val) 1
    return (bounds', True)
  where
    (val,limit) = bounds Map.! curr

runProgress :: [Int] -> IO ()
runProgress limits = do
  let progressIds = take (length limits) [0..]
      bounds = Map.fromList $ [ (i,(0,l))
                              | (i,l) <- zip progressIds limits
                              ]
      go bs = do
        (bs', continue) <- progressStep 0 bs
        when continue $ go bs'

  mapM_ (\idx -> sendDelay $ Init (show idx) Nothing) progressIds
  go bounds

sendDelay :: Show a => a -> IO ()
sendDelay msg = do
  threadDelay 100000
  send $ show msg

main :: IO ()
main = runProgress [4,4,4]

send :: String -> IO ()
send msg = do
  req <- parseUrl "http://localhost:8888"
  let req' = req { method = "POST"
                 , requestBody = RequestBodyLBS $ BS.pack $
                                 UTF8.encode msg
                 }
  void $ withManager $ httpLbs req'
