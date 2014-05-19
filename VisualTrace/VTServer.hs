{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# language
   RecordWildCards
 , StandaloneDeriving
 , DeriveFunctor
 #-}

module VisualTrace.VTServer(main) where

import System.Console.GetOpt
import System.Environment
import VisualTrace.Server
import qualified Network.HTTP.Server as HTTP

import qualified VisualTrace.Protocol.Image               as Image
import qualified VisualTrace.Protocol.Graph               as Graph
import qualified VisualTrace.Protocol.ProgressBar         as ProgressBar
import qualified VisualTrace.Protocol.ParallelComputation as ParallelComputation

import Text.Printf

-- no-warn-orphans
deriving instance Functor OptDescr
deriving instance Functor ArgDescr

data Image = ParallelComputation
           | ProgressBar
           | Graph
  deriving (Read, Enum, Bounded, Show)

data VTConfig = VTConfig
  { vtImage      :: Maybe Image
  , vtHttpConfig :: HTTP.Config
  }

defVtConfig :: VTConfig
defVtConfig = VTConfig { vtImage = Nothing
                       , vtHttpConfig = defaultHttpConfig
                       }

vtOptions :: [OptDescr (VTConfig -> VTConfig)]
vtOptions =
  fmap (fmap onHttpConfig) httpOptions ++
  [ Option ['i'] ["image"]
      (ReqArg (\i opts -> opts { vtImage = Just (readSafe i) }) "IMAGE")
      ("Image to use. One of " ++ show [(minBound::Image)..maxBound])
  ]
  where
    readSafe str = case reads str of
      [] -> error $ "Can't parse " ++ str
      (x,_):_ -> x

    onHttpConfig :: (HTTP.Config -> HTTP.Config) ->
                    VTConfig -> VTConfig
    onHttpConfig f vt = vt { vtHttpConfig = f (vtHttpConfig vt) }

parseVtOptions :: IO VTConfig
parseVtOptions = do
  argv <- getArgs
  pname <- getProgName
  let header = printf "Usage: %s [OPTION...]" pname
  case getOpt Permute vtOptions argv of
    (o,_n,[] ) -> return $ foldl (flip id) defVtConfig o
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header vtOptions))

main :: IO ()
main = do
  VTConfig{..} <- parseVtOptions

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
