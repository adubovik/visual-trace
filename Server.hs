{-# language
   ScopedTypeVariables
 #-}

module Server where

import Network.HTTP.Server
import Network.Socket
import Network.URL
import Codec.Binary.UTF8.String

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

import Protocol.ProgressBar
import Graphics.Gloss.Data.ExtentF
import Graphics.Gloss.Utils

newtype ServerState = ServerState Image

handler :: MVar ServerState -> SockAddr -> URL -> Request String -> IO (Response String)
handler state addr url req = do
  print addr
  print url
  print req
  print $ rqBody req
  let (command :: Command) = read $ rqBody req
  modifyMVar_ state $ \(ServerState image) ->
    return $ ServerState (action command image)
  return $ sendText OK ("Server received:\n" ++ show req)

sendText :: StatusCode -> String -> Response String
sendText s v    = insertHeader HdrContentLength (show (length txt))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Response String) { rspBody = txt }
  where txt = encodeString v

render :: MVar ServerState -> IO ()
render state = do
  let windowFrame = InWindow "Trace server" (500,500) (100,100)
      fps = 30
  playIO
    windowFrame
    black
    fps
    state
    drawWorld
    eventHandler
    timeEvolution

eventHandler :: Event -> MVar ServerState -> IO (MVar ServerState)
eventHandler _ st = return st

timeEvolution :: Float -> MVar ServerState -> IO (MVar ServerState)
timeEvolution _ st = return st

drawWorld :: MVar ServerState -> IO Picture
drawWorld state = do
  ServerState world <- readMVar state
  let worldPic = draw world
      worldExt = getPictureExt worldPic
      focusExt = enlargeExt 1.1 1.1 worldExt
      focusT   = focusTrans focusExt (500, 500)

  return $ focusT $
           pictures [ color blue (drawExt focusExt)
                    , worldPic
                    ]

main :: IO ()
main = do
  let config = defaultConfig { srvHost = "localhost"
                             , srvPort = 8888
                             }
  putStrLn "Server is running..."
  state <- newMVar (ServerState mkImage)
  void $ forkIO (render state)
  serverWith config (handler state)
