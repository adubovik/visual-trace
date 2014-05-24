{-# LANGUAGE
   TupleSections
 , RecordWildCards
 #-}

module VisualTrace.Client.Computations(main) where

import Options.Applicative
import System.Random
import Data.List
import Data.Function
import Text.Printf

import VisualTrace.HTTPConfig
import qualified VisualTrace.Client as Client
import VisualTrace.Data.ColorRead(fromColor)
import Graphics.Gloss.Data.Color

import VisualTrace.Protocol.ParallelComputation

mkCommand :: Int -> Int -> IO [(Int, Command)]
mkCommand nodeId wuid = do
  let nodeId' = printf "Node%03d" nodeId
      wuId' = printf "Wu%03d_%03d" wuid nodeId

      genRndTime = randomRIO (0,1000000)

      mkSubCommand ((statusMsg, clr), time) = do
        (time,) $
          Workunit
            nodeId'
            wuId'
            (fromColor clr, Just statusMsg)
            (statusMsg ++ " " ++ wuId')

      statuses = [ ("Starting"  , red)
                 , ("Processing", yellow)
                 , ("Finished"  , green)
                 ]

  times <- sort <$> (sequence $ replicate 3 genRndTime)
  let commands = map mkSubCommand $ zip statuses times
  return commands

mkCommands :: Int -> Int -> IO [Command]
mkCommands nodes wusPerNode = do
  commands <- sequence [ mkCommand node wu
                       | node <- [1..nodes]
                       , wu   <- [1..wusPerNode]
                       ]
  let commands' = map snd $ sortBy (compare `on` fst) $ concat commands
  return commands'

data Config = Config
  { cfgWusPerNode :: Int
  , cfgNodes      :: Int
  , cfgHttpConfig :: HTTPConfig
  }

options :: Parser Config
options = Config
  <$> option
      ( long "wusPerNode"
     <> short 'w'
     <> metavar "INTEGER"
     <> help "Number of workunits per single computational node"
     <> value 10
     <> showDefault )
  <*> option
      ( long "compNodes"
     <> short 'n'
     <> metavar "INTEGER"
     <> help "Number of computational nodes"
     <> value 6
     <> showDefault )
  <*> httpOptions

opts :: ParserInfo Config
opts = info
         (helper <*> options)
         fullDesc

main :: IO ()
main = do
  Config{..} <- execParser opts

  let send :: Show a => HTTPConfig -> a -> IO ()
      send config a = Client.delaySec 0.1 >>
                      Client.sendWithConfig config a

  commands <- mkCommands cfgNodes cfgWusPerNode
  mapM_ (send cfgHttpConfig) commands
