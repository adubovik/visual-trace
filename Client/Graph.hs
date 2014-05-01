{-# LANGUAGE OverloadedStrings  #-}

module Client.Graph(main) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Set as Set
import Data.List
import Text.Printf
import Control.Concurrent
import Control.Monad
import System.Random

import Protocol.Graph

rndPoint :: (Float,Float) -> (Float,Float) -> IO (Float, Float)
rndPoint xr yr = do
  x <- randomRIO xr
  y <- randomRIO yr
  return (x,y)

rndElem :: [a] -> IO a
rndElem ls = do
  let len = length ls
  idx <- randomRIO (0,len-1)
  return $ ls !! idx

genRndCommand :: (Set.Set Int, Set.Set Int) ->
                 IO ((Set.Set Int, Set.Set Int), Command)
genRndCommand (restNodes, graphNodes) = do
  actType <- randomRIO (0.0, 1.0 :: Float)
  let nrest  = Set.size restNodes
      nnodes = Set.size graphNodes
  if (nnodes < 4) || (actType < 0.3 && nrest > 0)
  then do
    node <- rndElem (Set.toList restNodes)
    point <- rndPoint (-200,200) (-200,200)
    return $ ( ( Set.delete node restNodes
               , Set.insert node graphNodes
               )
             , InsertNode node point
             )
  else do
    let nodeList = Set.toList graphNodes
    node1 <- rndElem nodeList
    let nodeList' = nodeList \\ [node1]
    node2 <- rndElem nodeList'
    return $ ((restNodes, graphNodes), InsertEdge node1 node2)

main :: IO ()
main = do
  let n = 15
      m = 50
      initGr = (Set.fromList [0..n-1], Set.empty)

      go :: (Set.Set Int, Set.Set Int) -> Int -> IO ()
      go _ 0 = return ()
      go gr cnt = do
        threadDelay 1000000
        (gr', command) <- genRndCommand gr
        send $ show command
        go gr' (cnt-1)

  go initGr m

send :: String -> IO ()
send msg = do
  putStrLn $ printf "Sending %s..." msg
  req <- parseUrl "http://localhost:8888"
  let req' = req { method = "POST"
                 , requestBody = RequestBodyLBS $ BS.pack $
                                 UTF8.encode msg
                 }
  void $ withManager $ httpLbs req'
