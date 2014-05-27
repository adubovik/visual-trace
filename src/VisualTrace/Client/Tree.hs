{-# language
   RecordWildCards
 , Rank2Types
 #-}

module VisualTrace.Client.Tree(main) where

import Options.Applicative
import Data.List
import System.Random

import VisualTrace.Protocol.Graph
import VisualTrace.Client hiding (send)

type Sender = forall a. Show a => a -> IO ()

mkTree :: Int -> Int -> [Command]
mkTree 1 parent = [InsertNode parent (0,0)]
mkTree nForks parent =
  let forks = map (+ (parent*10)) [1..nForks]
      commands = map (InsertEdge parent) forks
      parentNode = InsertNode parent (0,0)
  in  [parentNode] ++ commands ++ concatMap (mkTree (nForks-1)) forks

sendTree :: Sender -> Int -> IO ()
sendTree send maxForks = do
  let isInsertNode InsertNode{} = True
      isInsertNode _ = False

      (nodes,edges) = partition isInsertNode (mkTree maxForks 1)

      setRndPos (InsertNode node _) = do
        pos <- rndPoint (-200,200) (-200,200)
        return $ InsertNode node pos
      setRndPos cmd = return cmd

  nodes' <- mapM setRndPos nodes
  mapM_ send nodes'
  mapM_ send edges

rndPoint :: (Float,Float) -> (Float,Float) -> IO (Float, Float)
rndPoint xr yr = do
  x <- randomRIO xr
  y <- randomRIO yr
  return (x,y)

data Config = Config
  { cfgMaxForks   :: Int
  , cfgHttpConfig :: HTTPConfig
  }

options :: Parser Config
options = Config
  <$> option
      ( long "maxForks"
     <> short 'f'
     <> metavar "INTEGER"
     <> help "Maximum number of forks for single node in the tree"
     <> value 4
     <> showDefault )
  <*> httpOptions

opts :: ParserInfo Config
opts = info
         (helper <*> options)
         (fullDesc <> progDesc desc)
  where
    desc = "Generate random tree and send it node-by-node, \
           \edge-by-edge to a server."

main :: IO ()
main = mainWrapper $ do
  Config{..} <- execParser opts

  let send :: Show a => HTTPConfig -> a -> IO ()
      send config a = delaySec 0.3 >>
                      sendWithConfig config a

  sendTree (send cfgHttpConfig) cfgMaxForks
