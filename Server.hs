{-# language
   ScopedTypeVariables
 , ViewPatterns
 , RecordWildCards
 #-}

module Server where

import Network.HTTP.Server
import Network.Socket
import Network.URL
import Codec.Binary.UTF8.String

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ExtentF
import Graphics.Gloss.Data.ViewState hiding (Command)
import Graphics.Gloss.Utils

import Data.Monoid
import qualified Data.Map as Map

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

import Protocol.ProgressBar

newtype ServerImage = ServerImage Image

data World = World
 { wViewState :: ViewState
 , wImage     :: MVar ServerImage
 }

-- TODO: lens
onViewState :: (ViewState -> ViewState) -> (World -> World)
onViewState f w = w { wViewState = f (wViewState w) }

handler :: World -> SockAddr -> URL -> Request String -> IO (Response String)
handler (wImage -> state) _addr _url req = do
  putStrLn $ "Received: " ++ show (rqBody req)
  let (command :: Command) = read $ rqBody req
  modifyMVar_ state $ \(ServerImage image) ->
    return $ ServerImage (action command image)
  return $ sendText OK ("Server received:\n" ++ show req)

sendText :: StatusCode -> String -> Response String
sendText s v = insertHeader HdrContentLength (show (length txt))
             . insertHeader HdrContentEncoding "UTF-8"
             . insertHeader HdrContentEncoding "text/plain"
             $ (respond s :: Response String) { rspBody = txt }
  where txt = encodeString v

render :: World -> IO ()
render world = do
  let windowFrame = InWindow "Trace server" (500,500) (100,100)
      fps = 30
  playIO
    windowFrame
    black
    fps
    world
    drawWorld
    eventHandler
    timeEvolution

eventHandler :: Event -> World -> IO World
eventHandler (EventKey (Char 'r') Down _mod _pos) w = do
  ServerImage image <- readMVar (wImage w)
  let imageExt = getPictureExt $ draw image
      focusExt = enlargeExt 1.1 1.1 imageExt
      windowSize = (500,500)

  return $ onViewState (focusViewState focusExt windowSize) w

eventHandler e w = return $
  w { wViewState = updateViewStateWithEvent e (wViewState w) }

timeEvolution :: Float -> World -> IO World
timeEvolution _sec w = return w

drawWorld :: World -> IO Picture
drawWorld World{..} = do
  ServerImage image <- readMVar wImage
  return $
    applyViewPortToPicture viewPort $ draw image
  where
    viewPort = viewStateViewPort wViewState

mkWorld :: IO World
mkWorld = do
  image <- newMVar (ServerImage mkImage)
  return $ World
    { wViewState = viewStateInitWithConfig $ Map.toList $
                     Map.fromList commandConfig <>
                     Map.fromList defaultCommandConfig
    , wImage = image
    }
  where
    commandConfig = [oTranslate, oRotate, oRestore]
    oTranslate = ( CTranslate
                 , [ ( MouseButton RightButton
                     , Just (Modifiers { shift = Up, ctrl = Up, alt = Up })
                     )])
    oRotate  = (CRotate, [])
    oRestore = (CRestore, [])

main :: IO ()
main = do
  let config = defaultConfig { srvHost = "localhost"
                             , srvPort = 8888
                             }
  putStrLn "Server is running..."

  world <- mkWorld
  void $ forkIO (render world)
  serverWith config (handler world)
