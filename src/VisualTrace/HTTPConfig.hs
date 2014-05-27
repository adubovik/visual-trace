{-# language
   RecordWildCards
 #-}

module VisualTrace.HTTPConfig
 ( HTTPConfig(..)
 , httpOptions
 , httpOptInfo
 , toHTTPServerConfig
 ) where

import qualified Network.HTTP.Server as HTTP
import Options.Applicative
import Text.Printf

data HTTPConfig = HTTPConfig
  { httpHost :: String
  , httpPort :: Int
  }

instance Show HTTPConfig where
  show HTTPConfig{..} = printf "%s:%d" httpHost httpPort

httpOptInfo :: ParserInfo HTTPConfig
httpOptInfo = info
                (helper <*> httpOptions)
                fullDesc

httpOptions :: Parser HTTPConfig
httpOptions = HTTPConfig
 <$> strOption
     ( long "host"
    <> metavar "STRING"
    <> help "Server host"
    <> value "localhost"
    <> showDefault )
 <*> option
     ( long "port"
    <> metavar "INTEGER"
    <> help "Port to listen"
    <> value 8888
    <> showDefault )

toHTTPServerConfig :: HTTPConfig -> HTTP.Config
toHTTPServerConfig HTTPConfig{..} =
  HTTP.defaultConfig { HTTP.srvHost = httpHost
                     , HTTP.srvPort = fromInteger (fromIntegral httpPort)
                     }
