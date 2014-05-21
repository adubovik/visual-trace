{-# language
   RecordWildCards
 #-}

module VisualTrace.Client.ProgressBar(main) where

import Options.Applicative
import qualified Data.Map as Map
import Control.Monad

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

options :: Parser Config
options = Config
  <$> option
      ( long "barBounds"
     <> short 'b'
     <> metavar "[INTEGER]"
     <> help "List of progress bar bounds"
     <> value [4,4,4]
     <> showDefault )

opts :: ParserInfo Config
opts = info
         (helper <*> options)
         fullDesc

main :: IO ()
main = do
  Config{..} <- execParser opts
  runProgress cfgBarBounds
