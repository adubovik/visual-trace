{-# language
   ScopedTypeVariables
 , ViewPatterns
 , RecordWildCards
 , TupleSections
 , NamedFieldPuns
 #-}

module VisualTrace.Server(main) where

import Network.HTTP.Server
import Network.Socket
import Network.URL
import Codec.Binary.UTF8.String

import qualified Graphics.UI.GLUT as GLUT

import Graphics.Gloss.Interface.IO.Game

import VisualTrace.Data.EventInfo
import VisualTrace.Data.Ext
import Graphics.Gloss.Data.ViewState hiding (Command)
import VisualTrace.Data.ViewState.Focus
import VisualTrace.Data.Ext.Utils
import qualified VisualTrace.Data.PictureF as PF
import VisualTrace.Data.Feedback
import VisualTrace.Data.PictureF.Selection(selectWithExt, select)
import VisualTrace.Data.PictureF.Trans(toPicture,desugarePicture)

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Typeable as Typeable

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

-- import VisualTrace.Protocol.ProgressBar
-- import VisualTrace.Protocol.Graph
import VisualTrace.Protocol.ParallelComputation

type EventHandler = Event -> World -> IO World
newtype ServerImage = ServerImage { unServerImage :: Image }

data World = World
 { wViewState :: ViewState
 , wImage     :: MVar ServerImage
 , wMousePos  :: Maybe Point
 , wLastFeedback :: Maybe (ExWrap Feedback)
 , wEventHistory :: EventHistory
 }

onServerImage :: Functor m => (Image -> m Image) -> ServerImage -> m ServerImage
onServerImage f = (ServerImage <$>) . f . unServerImage

readImage :: World -> IO Image
readImage =  (unServerImage <$>) . readMVar . wImage

onImage :: (Image -> IO Image) -> World -> IO ()
onImage f w = modifyMVar_ (wImage w) $ onServerImage f

-- TODO: lens
onViewState :: Monad m => (ViewState -> m ViewState) -> (World -> m World)
onViewState f w = do
  viewState' <- f (wViewState w)
  return $ w { wViewState = viewState' }

onEventHistory :: Monad m => (EventHistory -> m EventHistory) -> (World -> m World)
onEventHistory f w = do
  eventHistory' <- f (wEventHistory w)
  return $ w { wEventHistory = eventHistory' }

getWindowSize :: IO (Int,Int)
getWindowSize = do
  GLUT.Size width height <- GLUT.get GLUT.windowSize
  return (fromIntegral width, fromIntegral height)

handler :: World -> SockAddr -> URL -> Request String -> IO (Response String)
handler world _addr _url req = do
  putStrLn $ "Received: " ++ show (rqBody req)
  let (command :: Command) = read $ rqBody req
  onImage (return . action command) world
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
      return -- traceHook event
  >=> onEventHistory (         updateEventHistoryIO     event)
  >=> eh event
  >=> onViewState    (return . updateViewStateWithEvent event)
  where
    _traceHook :: Event -> World -> IO World
    _traceHook e world = do
      putStrLn $ "Event = " ++ show e
      return world

updateEventHistoryIO :: Event -> EventHistory -> IO EventHistory
updateEventHistoryIO event eh = do
  windowSize <- getWindowSize
  return $ updateEventHistory windowSize event eh

handleEventStep :: (Image -> Image) -> Event -> World -> IO World
handleEventStep imageEvolution event world@World{..} = do
  oldImage <- readImage world
  onImage (return . imageEvolution) world

  let mousePos = getCurrMousePos wEventHistory
      viewPort = viewStateViewPort wViewState
      localMousePos = invertViewPort viewPort mousePos

  let selectedPic = fst $
                    select viewPort localMousePos id $
                    drawAnn oldImage

      newFeedback = case selectedPic of
        Just (PF.unWrap -> PF.SelectionTrigger fb _) -> Just fb
        _ -> Nothing
      oldFeedback = wLastFeedback

  let runFeedbackWithEvent feedBack eventInfo
        | Just (ExWrap wrapedFeedback) <- feedBack
        , Just fb <- Typeable.cast wrapedFeedback
        = do
            flip onImage world $ runFeedback fb eventInfo
            return $ getFocusCapture fb eventInfo
        | otherwise
        = return FocusReleased

  let focusSwith = oldFeedback /= newFeedback
      focusOld | focusSwith = FocusLost
               | otherwise  = FocusStill
      focusNew | focusSwith = FocusGained
               | otherwise  = FocusStill
      mkEventInfo focus = EventInfo { efFocus = focus
                                    , efEvent = event
                                    , efEventHistory =
                                        let toLocal = invertViewPort viewPort
                                        in  onMousePosHistory toLocal wEventHistory
                                    }

  captureOld <- runFeedbackWithEvent oldFeedback (mkEventInfo focusOld)
  newFeedback' <-
    case captureOld of
         FocusCaptured ->
           return oldFeedback
         FocusReleased -> do
           void $ runFeedbackWithEvent newFeedback (mkEventInfo focusNew)
           return newFeedback

  return $ world { wMousePos = Just localMousePos
                 , wLastFeedback = newFeedback'
                 }

eventHandler :: EventHandler
eventHandler e@(EventMotion _) w = do
  handleEventStep id e w

eventHandler (EventKey (Char 'r') Down _mod _pos) w@World{..} = do
  ServerImage image <- readMVar wImage
  let imageExt = getPictureExt $ desugarePicture viewPort $ drawAnn image
      focusExt = enlargeExt 1.1 1.1 imageExt

  windowSize <- getWindowSize
  onViewState (return . focusViewState focusExt windowSize) w
  where
    viewPort = viewStateViewPort wViewState

eventHandler _e w = return w

timeEvolution :: Float -> World -> IO World
timeEvolution secElapsed w = do
  let emitFakeEvent world = do
        world' <- onEventHistory (updateEventHistoryIO event) world
        return (event, world')
        where
          mousePos = getCurrMousePos $ wEventHistory world
          event = EventMotion mousePos

  (event, w') <- emitFakeEvent w
  handleEventStep (evolution secElapsed) event w'

  -- onImage (return . evolution secElapsed) w
  -- return w

drawWorld :: World -> IO Picture
drawWorld World{..} = do
  ServerImage image <- readMVar wImage
  let selectImage pic = case wMousePos of
        Nothing       -> pic
        Just mousePos -> snd $ selectWithExt viewPort mousePos pic

      picture = selectImage $ drawAnn image

  return $
    applyViewPortToPicture viewPort $
      toPicture viewPort $
        picture
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
    , wMousePos = Nothing
    , wLastFeedback = Nothing
    , wEventHistory = initEventHistory
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
