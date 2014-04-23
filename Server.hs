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

import qualified Graphics.UI.GLUT as GLUT

import Graphics.Gloss.Interface.IO.Game

import Graphics.Gloss.Data.Ext
import Graphics.Gloss.Data.ViewState hiding (Command)
import Graphics.Gloss.Data.ViewState.Focus
import Graphics.Gloss.Data.Ext.Utils
import qualified Graphics.Gloss.Data.PictureF as PF
import Graphics.Gloss.Data.PictureF.Selection(selectWithExt)
import Graphics.Gloss.Data.PictureF.Trans(toPicture)
import qualified Graphics.Gloss.Text as T

import Data.Monoid
import qualified Data.Map as Map

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

-- import Protocol.ProgressBar
import Protocol.Graph

newtype ServerImage = ServerImage Image

data World = World
 { wViewState :: ViewState
 , wImage     :: MVar ServerImage
 , wAnnot     :: Maybe PF.Picture
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
  let invMousePos = invertViewPort viewPort mousePos
      annotPos = invertViewPort viewPort $ mousePos + (15,15)
      viewPort = viewStateViewPort (wViewState w)
      annotPic = drawAnnot annotPos <$> getAnnotation viewPort invMousePos image
      drawAnnot pos msg =
        PF.color blue $
        uncurry PF.translate pos $
        T.textWithBackground (Just 30) yellow msg
  return $ w { wAnnot = annotPic
             , wViewState = updateViewStateWithEvent e (wViewState w)
             , wMousePos = Just invMousePos
             }

eventHandler (EventKey (Char 'r') Down _mod _pos) w = do
  ServerImage image <- readMVar (wImage w)
  let imageExt = getPictureExt $ drawAnn image
      focusExt = enlargeExt 1.1 1.1 imageExt

  windowSize <- getWindowsSize
  return $ onViewState (focusViewState focusExt windowSize) w
  where
    getWindowsSize :: IO (Int,Int)
    getWindowsSize = do
      GLUT.Size width height <- GLUT.get GLUT.windowSize
      return (fromIntegral width, fromIntegral height)

eventHandler e w = return $
  w { wViewState = updateViewStateWithEvent e (wViewState w) }

timeEvolution :: Float -> World -> IO World
timeEvolution secElapsed w@(wImage -> state) = do
  modifyMVar_ state $ \(ServerImage image) -> do
    let image' = evolution secElapsed image
    return $ ServerImage image'
  return w

drawWorld :: World -> IO Picture
drawWorld World{..} = do
  ServerImage image <- readMVar wImage
  let annotPic = maybe PF.blank id wAnnot
      selectImage pic = case wMousePos of
        Nothing -> pic
        Just mousePos -> selectWithExt viewPort mousePos pic
  return $
    applyViewPortToPicture viewPort $
      toPicture viewPort $
        PF.pictures [ selectImage $ drawAnn image
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
