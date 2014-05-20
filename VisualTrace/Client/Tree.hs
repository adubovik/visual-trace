{-# language
   RecordWildCards
 #-}

module VisualTrace.Client.Tree(main) where

import Data.List
import System.Random

import VisualTrace.Protocol.Graph
import qualified VisualTrace.Client as Client

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

  nodes' <- mapM setRndPos nodes
  mapM_ send nodes'
  mapM_ send edges

send :: Show a => a -> IO ()
send = Client.sendWithDelay 0.3 "localhost" 8888

rndPoint :: (Float,Float) -> (Float,Float) -> IO (Float, Float)
rndPoint xr yr = do
  x <- randomRIO xr
  y <- randomRIO yr
  return (x,y)

main :: IO ()
main = sendTree
