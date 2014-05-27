{-# language
   RecordWildCards
 , Rank2Types
 #-}

module VisualTrace.Client.ProgressBar(main) where

import Options.Applicative
import Control.Monad
import qualified Data.Map as Map

import VisualTrace.Protocol.ProgressBar
import VisualTrace.Client hiding (send)

type ProgressState = Map.Map Int (Int,Int)
type Sender = forall a. Show a => a -> IO ()

progressStep :: Sender -> Int -> ProgressState -> IO (ProgressState, Bool)
progressStep send curr bounds
  | curr == Map.size bounds
  = return (bounds, False)
  | val == limit = do
    let bounds' = Map.adjust (const (0,limit)) curr bounds
    send $ Init (show curr) Nothing
    progressStep send (curr+1) bounds'
  | otherwise = do
    let bounds' = Map.adjust (const (val+1,limit)) curr bounds
    send $ Done (show curr) (Just $ "Done " ++ show val) 1
    return (bounds', True)
  where
    (val,limit) = bounds Map.! curr

runProgress :: Sender -> [Int] -> IO ()
runProgress send limits = do
  let progressIds = take (length limits) [0..]
      bounds = Map.fromList $ [ (i,(0,l))
                              | (i,l) <- zip progressIds limits
                              ]
      go bs = do
        (bs', continue) <- progressStep send 0 bs
        when continue $ go bs'

  mapM_ (\idx -> send $ Init (show idx) Nothing) progressIds
  go bounds

data Config = Config
  { cfgBarBounds  :: [Int]
  , cfgHttpConfig :: HTTPConfig
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
  <*> httpOptions

opts :: ParserInfo Config
opts = info
         (helper <*> options)
         fullDesc

main :: IO ()
main = mainWrapper $ do
  Config{..} <- execParser opts
  let send :: HTTPConfig -> Sender
      send config a = delaySec 0.3 >>
                      sendWithConfig config a

  runProgress (send cfgHttpConfig) cfgBarBounds
