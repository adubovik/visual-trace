{-# language
   RecordWildCards
 , StandaloneDeriving
 , DeriveFunctor
 #-}

module VisualTrace.VTServer(main) where

import Options.Applicative
import qualified Network.HTTP.Server as HTTP

import VisualTrace.Server
import qualified VisualTrace.Protocol.Image               as Image
import qualified VisualTrace.Protocol.Graph               as Graph
import qualified VisualTrace.Protocol.ProgressBar         as ProgressBar
import qualified VisualTrace.Protocol.ParallelComputation as ParallelComputation

data Image = ParallelComputation
           | ProgressBar
           | Graph
  deriving (Read, Enum, Bounded, Show)

data VTConfig = VTConfig
  { vtImage      :: Maybe Image
  , vtHttpConfig :: HTTP.Config
  }

vtOptions :: Parser VTConfig
vtOptions = VTConfig
 <$> nullOption
     ( reader ((Just <$>) <$> auto)
    <> value Nothing
    <> long "image"
    <> short 'i'
    <> help ("Image to use. One of " ++ show [(minBound::Image)..maxBound]))
 <*> httpOptions

opts :: ParserInfo VTConfig
opts = info
         (helper <*> vtOptions)
         fullDesc

main :: IO ()
main = do
  VTConfig{..} <- execParser opts

  case vtImage of
    Nothing -> error "You should specify image."
    Just image ->
      case image of
        Graph               ->
          runServerWithConfig vtHttpConfig (Image.initImage :: Graph.Image)
        ProgressBar         ->
          runServerWithConfig vtHttpConfig (Image.initImage :: ProgressBar.Image)
        ParallelComputation ->
          runServerWithConfig vtHttpConfig (Image.initImage :: ParallelComputation.Image)
