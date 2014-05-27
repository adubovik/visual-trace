{-# language
   RecordWildCards
 , DoAndIfThenElse
 #-}

module VisualTrace.Client.Graph(main) where

import Options.Applicative
import System.Random
import Data.List
import qualified Data.Set as Set

import VisualTrace.Protocol.Graph
import VisualTrace.Client hiding (send)

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

data Config = Config
  { cfgNodes :: Int
  , cfgEdges :: Int
  , cfgHttpConfig :: HTTPConfig
  }

options :: Parser Config
options = Config
  <$> option
      ( long "nodes"
     <> short 'n'
     <> metavar "INTEGER"
     <> help "Number of nodes in graph"
     <> value 15
     <> showDefault )
  <*> option
      ( long "edges"
     <> short 'e'
     <> metavar "INTEGER"
     <> help "Number of edges in graph"
     <> value 50
     <> showDefault )
  <*> httpOptions

opts :: ParserInfo Config
opts = info
         (helper <*> options)
         (fullDesc <> progDesc desc)
  where
    desc = "Generate random graph and send it node-by-node, \
           \edge-by-edge to a server."

main :: IO ()
main = do
  Config{..} <- execParser opts

  let send :: Show a => HTTPConfig -> a -> IO ()
      send config a = delaySec 0.3 >>
                      sendWithConfig config a

  let initGr = (Set.fromList [0..cfgNodes-1], Set.empty)

      go :: (Set.Set Int, Set.Set Int) -> Int -> IO ()
      go _ 0 = return ()
      go gr cnt = do
        (gr', cmd) <- genRndCommand gr
        send cfgHttpConfig cmd
        go gr' (cnt-1)

  go initGr cfgEdges

