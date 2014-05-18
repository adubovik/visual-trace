{-# LANGUAGE OverloadedStrings  #-}

module VisualTrace.Client.Tree(main) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8
import Data.List
import Text.Printf
import Control.Concurrent
import Control.Monad
import System.Random

import VisualTrace.Protocol.Graph

mkTree :: [Command]
mkTree = go 4 1
  where
    go :: Int -> Int -> [Command]
    go 1 parent = [InsertNode parent (0,0)]
    go nForks parent =
      let forks = map (+ (parent*10)) [1..nForks]
          commands = map (InsertEdge parent) forks
          parentNode = InsertNode parent (0,0)
      in  [parentNode] ++ commands ++ concatMap (go (nForks-1)) forks

sendTree :: IO ()
sendTree = do
  let isInsertNode InsertNode{} = True
      isInsertNode _ = False

      (nodes,edges) = partition isInsertNode mkTree

      setRndPos (InsertNode node _) = do
        pos <- rndPoint (-200,200) (-200,200)
        return $ InsertNode node pos
      setRndPos command = return command

      sendWithDelay msg = do
        threadDelay 100000
        send msg

  nodes' <- mapM setRndPos nodes
  mapM_ (sendWithDelay . show) nodes'
  mapM_ (sendWithDelay . show) edges

rndPoint :: (Float,Float) -> (Float,Float) -> IO (Float, Float)
rndPoint xr yr = do
  x <- randomRIO xr
  y <- randomRIO yr
  return (x,y)

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
