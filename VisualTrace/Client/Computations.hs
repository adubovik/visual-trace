{-# LANGUAGE
   TupleSections
 , RecordWildCards
 #-}

module VisualTrace.Client.Computations(main) where

import System.Console.GetOpt
import System.Random
import System.Environment

import Control.Applicative
import Data.List
import Data.Function
import Text.Printf

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

send :: Show a => a -> IO ()
send = Client.sendWithDelay 0.1 "localhost" 8888

data Config = Config
  { cfgWusPerNode :: Int
  , cfgNodes      :: Int
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgWusPerNode = 10
  , cfgNodes      = 6
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option ['w'] ["wusPerNode"]
      (ReqArg (\i opts -> opts { cfgWusPerNode = readSafe i }) "INTEGER")
      "Number of workunits per single computational node (10 default)."
  , Option ['n'] ["compNodes"]
      (ReqArg (\i opts -> opts { cfgNodes      = readSafe i }) "INTEGER")
      "Number of computational nodes (6 default)."
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
  commands <- mkCommands cfgNodes cfgWusPerNode
  mapM_ send commands
