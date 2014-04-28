{-# language
   ScopedTypeVariables
 , ViewPatterns
 , RecordWildCards
 , TupleSections
 , NamedFieldPuns
 #-}

module Server where

import Network.HTTP.Server
import Network.Socket
import Network.URL
import Codec.Binary.UTF8.String

import qualified Graphics.UI.GLUT as GLUT

import Graphics.Gloss.Interface.IO.Game

import qualified Graphics.Gloss.Data.Event as Event
import Graphics.Gloss.Data.EventStorage
import Graphics.Gloss.Data.Ext
import Graphics.Gloss.Data.ViewState hiding (Command)
import Graphics.Gloss.Data.ViewState.Focus
import Graphics.Gloss.Data.Ext.Utils
import qualified Graphics.Gloss.Data.PictureF as PF
import Graphics.Gloss.Data.PictureF.Selection(selectWithExt, select)
import Graphics.Gloss.Data.PictureF.Trans(toPicture)
import qualified Graphics.Gloss.Text as T

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Typeable as Typeable

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

-- import Protocol.ProgressBar
import Protocol.Graph

type EventHandler = Event -> World -> IO World
newtype ServerImage = ServerImage { unServerImage :: Image }

onServerImage :: (Image -> Image) -> ServerImage -> ServerImage
onServerImage f = ServerImage . f . unServerImage

data World = World
 { wViewState :: ViewState
 , wImage     :: MVar ServerImage
 , wAnnot     :: Maybe PF.Picture
 , wMousePos  :: Maybe Point
 , wLastFeedback :: Maybe (PF.ExWrap PF.Feedback)
 , wEventStorage :: EventStorage
 }

-- TODO: lens
onViewState :: Monad m => (ViewState -> m ViewState) -> (World -> m World)
onViewState f w = do
  viewState' <- f (wViewState w)
  return $ w { wViewState = viewState' }

onEventStorage :: Monad m => (EventStorage -> m EventStorage) -> (World -> m World)
onEventStorage f w = do
  eventStorage' <- f (wEventStorage w)
  return $ w { wEventStorage = eventStorage' }

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
    (eventHook eventHandler)
    timeEvolution

eventHook :: EventHandler -> EventHandler
eventHook eh event =
      onEventStorage (return . updateEventStorage event)
  >=> eh event
  >=> onViewState (return . updateViewStateWithEvent event)

handeSelectionWithEvent :: Image -> World -> IO World
handeSelectionWithEvent oldImage w = do
  let mousePos = getCurrMousePos (wEventStorage w)
      localMousePos = invertViewPort viewPort mousePos
      viewPort = viewStateViewPort (wViewState w)

      annotPic = drawAnnot annotPos <$>
                 getAnnotation viewPort localMousePos oldImage
        where
          annotPos = invertViewPort viewPort $
                     mousePos + (15,15)
          drawAnnot pos msg =
            PF.color blue $
            uncurry PF.translate pos $
            let oneLineHeight = Just 30 in
            T.textWithBackground oneLineHeight yellow msg

  let selectedPic = fst $
                    select viewPort localMousePos id $
                    drawAnn oldImage

      newFeedback = case selectedPic of
        Just (PF.unWrap -> PF.SelectionTrigger fb _) -> Just fb
        _ -> Nothing

      oldFeedback = wLastFeedback w

  let runFeedbackWithEvent feedBack event =
        case feedBack of
          Just (PF.ExWrap fb) ->
            case (Typeable.cast fb) :: Maybe (PF.Feedback Image) of
              Just PF.Feedback{..} -> do
                modifyMVar_ (wImage w) $ \(ServerImage im) -> do
                  fbSideEffect event im
                  return $ ServerImage $ fbTransform event im
              Nothing -> return ()
          Nothing -> return ()

  let selectionSwitch = oldFeedback /= newFeedback
      event = mkEvent viewPort (wEventStorage w) selectionSwitch


  newFeedback' <-
    case event of
      Event.Drag _ -> do
        runFeedbackWithEvent oldFeedback event
        return oldFeedback
      _            -> do
        runFeedbackWithEvent oldFeedback Event.InverseEvent
        runFeedbackWithEvent newFeedback event
        return newFeedback

  return $ w { wAnnot = annotPic
             , wMousePos = Just localMousePos
             , wLastFeedback = newFeedback'
             }

mkEvent :: ViewPort -> EventStorage -> Bool -> Event.Event
mkEvent viewPort eventStorage _
  | isMousePressed LeftButton eventStorage
  , let toLocal = invertViewPort viewPort
  , (toLocal -> newMousePos) <- getCurrMousePos eventStorage
  = Event.Drag newMousePos
mkEvent _ _ _ = Event.Event

eventHandler :: EventHandler
eventHandler (EventMotion _) w = do
  ServerImage oldImage <- readMVar (wImage w)
  handeSelectionWithEvent oldImage w

eventHandler (EventKey (Char 'r') Down _mod _pos) w = do
  ServerImage image <- readMVar (wImage w)
  let imageExt = getPictureExt $ drawAnn image
      focusExt = enlargeExt 1.1 1.1 imageExt

  windowSize <- getWindowsSize
  onViewState (return . focusViewState focusExt windowSize) w
  where
    getWindowsSize :: IO (Int,Int)
    getWindowsSize = do
      GLUT.Size width height <- GLUT.get GLUT.windowSize
      return (fromIntegral width, fromIntegral height)

eventHandler _e w = return w

timeEvolution :: Float -> World -> IO World
timeEvolution secElapsed w = do
  ServerImage oldImage <- readMVar (wImage w)

  modifyMVar_ (wImage w) $
    return . onServerImage (evolution secElapsed)

  handeSelectionWithEvent oldImage w

drawWorld :: World -> IO Picture
drawWorld World{..} = do
  ServerImage image <- readMVar wImage
  let annotPic = maybe PF.blank id wAnnot
      selectImage pic = case wMousePos of
        Nothing       -> pic
        Just mousePos -> snd $ selectWithExt viewPort mousePos pic

      picture = selectImage $ drawAnn image

  return $
    applyViewPortToPicture viewPort $
      toPicture viewPort $
        PF.pictures [ picture
                    , annotPic
                    ]
  where
    viewPort = viewStateViewPort wViewState

initWorld :: IO World
initWorld = do
  image <- newMVar (ServerImage mkImage)
  return $ World
    { wViewState = viewStateInitWithConfig $ Map.toList $
                     Map.fromList commandConfig <>
                     Map.fromList defaultCommandConfig
    , wImage = image
    , wAnnot = Nothing
    , wMousePos = Nothing
    , wLastFeedback = Nothing
    , wEventStorage = initEventStorage
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

  world <- initWorld
  void $ forkIO (render world)
  serverWith config (handler world)
