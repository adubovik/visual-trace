{-# language
   ScopedTypeVariables
 , ViewPatterns
 , RecordWildCards
 , TupleSections
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
import Graphics.Gloss.Data.PictureF.Selection(selectWithExt)
import Graphics.Gloss.Data.PictureF(toPicture)
import qualified Graphics.Gloss.Text as T

import Data.Monoid
import qualified Data.Map as Map

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

import Protocol.ProgressBar

newtype ServerImage = ServerImage Image

data World = World
 { wViewState :: ViewState
 , wImage     :: MVar ServerImage
 , wAnnot     :: Maybe Picture -- Maybe ((Float,Float), String)
 , wMousePos  :: Maybe Point
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
eventHandler e@(EventMotion mousePos) w = do
  ServerImage image <- readMVar (wImage w)
  let mousePos' = invertViewPort viewPort mousePos
      viewPort = viewStateViewPort (wViewState w)
      annotPic = drawAnnot mousePos' <$> getAnnotation mousePos' image
      drawAnnot pos msg =
        color blue $
        uncurry translate pos $
        scale 0.01 0.01 $
        T.textWithBackground yellow msg
  return $ w { wAnnot = annotPic
             , wViewState = updateViewStateWithEvent e (wViewState w)
             , wMousePos = Just mousePos'
             }

eventHandler (EventKey (Char 'r') Down _mod _pos) w = do
  ServerImage image <- readMVar (wImage w)
  let imageExt = getPictureExt $ draw image
      focusExt = enlargeExt 1.1 1.1 imageExt
      -- TODO: get the actual size of the screen
      windowSize = (500,500)

  return $ onViewState (focusViewState focusExt windowSize) w

eventHandler e w = return $
  w { wViewState = updateViewStateWithEvent e (wViewState w) }

timeEvolution :: Float -> World -> IO World
timeEvolution _sec w = return w

drawWorld :: World -> IO Picture
drawWorld World{..} = do
  ServerImage image <- readMVar wImage
  let annotPic = maybe blank id wAnnot
      selectImage pic = case wMousePos of
        Nothing -> pic
        Just mousePos -> selectWithExt mousePos pic
  return $
    applyViewPortToPicture viewPort $
      pictures [ toPicture $ selectImage $ drawAnn image
               , annotPic
               ]
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
    , wAnnot = Nothing
    , wMousePos = Nothing
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
