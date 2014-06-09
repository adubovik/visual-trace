{-# language
   RecordWildCards
 , Rank2Types
 , MultiWayIf
 #-}

module VisualTrace.Client.ProgressBar(main) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Options.Applicative
import qualified Data.Map as Map

import VisualTrace.Protocol.ProgressBar
import VisualTrace.Client hiding (send)

type ProgressState = Map.Map Int (Int,Int)
type Sender = forall a. Show a => [a] -> IO ()

progressStep :: Int -> WriterT [Command] (State ProgressState) Bool
progressStep curr = do
  bounds <- get
  let (val,limit) = bounds Map.! curr
  if | curr == Map.size bounds ->
       return False
     | val == limit -> do
       modify $ Map.adjust (const (0,limit)) curr
       tell $ [Init (show curr) Nothing]
       progressStep (curr+1)
     | otherwise -> do
       modify $ Map.adjust (const (val+1,limit)) curr
       tell $ [Done (show curr) (Just $ "Done " ++ show val) 1]
       return True

runProgress :: Sender -> [Int] -> IO ()
runProgress send limits = do
  let progressIds = take (length limits) [0..]
      bounds = Map.fromList $ [ (i,(0,l))
                              | (i,l) <- zip progressIds limits
                              ]
      go bs = do
        let ((continue,commands),bs') =
              flip runState bs $
              runWriterT $
              progressStep 0
        send commands
        when continue $ go bs'

  send $ fmap (\idx -> Init (show idx) Nothing) progressIds
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
     <> value [8,8,8,8,8]
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
      send config as = delaySec 0.1 >>
                       mapM_ (sendWithConfig config) as

  runProgress (send cfgHttpConfig) cfgBarBounds
