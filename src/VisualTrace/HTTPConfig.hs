{-# language
   RecordWildCards
 #-}

module VisualTrace.HTTPConfig
 ( HTTPConfig(..)
 , Side(..)
 , httpOptions
 , httpOptInfo
 , toHTTPServerConfig
 ) where

import qualified Network.HTTP.Server as HTTP
import Options.Applicative
import Text.Printf

data Side = Client | Server

data HTTPConfig = HTTPConfig
  { httpHost :: String
  , httpPort :: Int
  }

instance Show HTTPConfig where
  show HTTPConfig{..} = printf "%s:%d" httpHost httpPort

httpOptInfo :: Side -> ParserInfo HTTPConfig
httpOptInfo side = info
                    (helper <*> httpOptions side)
                    fullDesc

httpOptions :: Side -> Parser HTTPConfig
httpOptions side = HTTPConfig
 <$> strOption
     ( long "host"
    <> metavar "STRING"
    <> help "Server host"
    <> value (defaultHost side)
    <> showDefault )
 <*> option
     ( long "port"
    <> metavar "INTEGER"
    <> help "Port to listen"
    <> value 8888
    <> showDefault )
  where
    defaultHost Server = "0.0.0.0"
    defaultHost Client = "127.0.0.1"

toHTTPServerConfig :: HTTPConfig -> HTTP.Config
toHTTPServerConfig HTTPConfig{..} =
  HTTP.defaultConfig { HTTP.srvHost = httpHost
                     , HTTP.srvPort = fromInteger (fromIntegral httpPort)
                     }
