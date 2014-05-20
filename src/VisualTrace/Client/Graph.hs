{-# language
   RecordWildCards
 , DoAndIfThenElse
 #-}

module VisualTrace.Client.Graph(main) where

import System.Console.GetOpt
import System.Random
import System.Environment

import qualified Data.Set as Set
import Data.List
import Text.Printf

import VisualTrace.Protocol.Graph
import qualified VisualTrace.Client as Client

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

send :: Show a => a -> IO ()
send = Client.sendWithDelay 0.5 "localhost" 8888

data Config = Config
  { cfgNodes :: Int
  , cfgEdges :: Int
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgNodes = 15
  , cfgEdges = 50
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option ['e'] ["edges"]
      (ReqArg (\i opts -> opts { cfgEdges = readSafe i }) "INTEGER")
      "Number of edges in graph (50 default)."
  , Option ['n'] ["nodes"]
      (ReqArg (\i opts -> opts { cfgNodes = readSafe i }) "INTEGER")
      "Number of nodes in graph (15 default)."
  ]
  where
    readSafe str = case reads str of
      [] -> error $ "Can't parse " ++ str
      (x,_):_ -> x

getConfig :: IO Config
getConfig = do
  argv <- getArgs
  pname <- getProgName
  let header = printf "Usage: %s [OPTION...]" pname
  case getOpt Permute options argv of
    (o,_n,[] ) -> return $ foldl (flip id) defaultConfig o
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

main :: IO ()
main = do
  Config{..} <- getConfig

  let initGr = (Set.fromList [0..cfgNodes-1], Set.empty)

      go :: (Set.Set Int, Set.Set Int) -> Int -> IO ()
      go _ 0 = return ()
      go gr cnt = do
        (gr', command) <- genRndCommand gr
        send command
        go gr' (cnt-1)

  go initGr cfgEdges

