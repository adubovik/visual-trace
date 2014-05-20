{-# language
   RecordWildCards
 #-}

module VisualTrace.Client.ProgressBar(main) where

import System.Console.GetOpt
import System.Environment

import qualified Data.Map as Map
import Control.Monad
import Text.Printf

import VisualTrace.Protocol.ProgressBar
import qualified VisualTrace.Client as Client

type ProgressState = Map.Map Int (Int,Int)

progressStep :: Int -> ProgressState -> IO (ProgressState, Bool)
progressStep curr bounds
  | curr == Map.size bounds
  = return (bounds, False)
  | val == limit = do
    let bounds' = Map.adjust (const (0,limit)) curr bounds
    send $ Init (show curr) Nothing
    progressStep (curr+1) bounds'
  | otherwise = do
    let bounds' = Map.adjust (const (val+1,limit)) curr bounds
    send $ Done (show curr) (Just $ "Done " ++ show val) 1
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

  mapM_ (\idx -> send $ Init (show idx) Nothing) progressIds
  go bounds

send :: Show a => a -> IO ()
send = Client.sendWithDelay 0.3 "localhost" 8888

data Config = Config
  { cfgBarBounds :: [Int]
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgBarBounds = [4,4,4]
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option ['b'] ["barBounds"]
      (ReqArg (\i opts -> opts { cfgBarBounds = readSafe i }) "[INTEGER]")
      "Bounds of progress bars ([4,4,4] default)."
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
  runProgress cfgBarBounds
