{-# language
   ScopedTypeVariables
 , ViewPatterns
 , RecordWildCards
 , TupleSections
 , NamedFieldPuns
 , ExistentialQuantification
 , Rank2Types
 #-}

module VisualTrace.Server
 ( runServer
 , runServerWithConfig

 , httpOptions
 , HTTPConfig(..)
 ) where

import Options.Applicative
import Network.HTTP.Server
import Network.Socket
import Network.URL
import Codec.Binary.UTF8.String

import Text.Printf
import Data.Proxy
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

import qualified Graphics.UI.GLUT as GLUT

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewState hiding (Command)

import VisualTrace.HTTPConfig(HTTPConfig(..),Side(..),toHTTPServerConfig)
import qualified VisualTrace.HTTPConfig as HTTPConfig

import VisualTrace.Data.EventInfo
import VisualTrace.Data.Ext
import VisualTrace.Data.ViewState.Focus
import VisualTrace.Data.Ext.Utils
import qualified VisualTrace.Data.Picture as PF
import VisualTrace.Data.Feedback
import VisualTrace.Data.Picture.Selection
import VisualTrace.Data.Picture.Trans(toPicture)

import VisualTrace.Protocol.Image
import VisualTrace.Protocol.Image.CachedImage

type EventHandler = Event -> World -> IO World
data ServerImage = forall i. Image i => ServerImage i

data World = World
 { wViewState :: !ViewState
 , wImage     :: !(MVar ServerImage)
 , wMousePos  :: !(Maybe Point)
 , wLastFeedback :: !(Maybe (ExWrap Feedback))
 , wEventHistory :: !EventHistory
 }

onServerImage :: Functor m => (forall i. Image i => i -> m i) ->
                 ServerImage -> m ServerImage
onServerImage f (ServerImage i) = ServerImage <$> f i

queryImage :: (forall i. Image i => i -> q) -> World -> IO q
queryImage f w = do
  ServerImage i <- readMVar (wImage w)
  return $ f i

onImage :: (forall i. Image i => i -> IO i) -> World -> IO ()
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
  putStrLn $ "Received: " ++ rqBody req

  let cmd :: String = rqBody req
      runCommand img = case interpretCommand cmd img of
        Left  msg  -> do
          putStrLn $ "Parse error: " ++ msg
          return img
        Right img' -> return img'

  onImage runCommand world
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

handleEventStep :: (forall i. Image i => i -> i) -> Event -> World -> IO World
handleEventStep imageEvolution event world@World{..} = do
  let mousePos = getCurrMousePos wEventHistory
      viewPort = viewStateViewPort wViewState
      localMousePos = invertViewPort viewPort mousePos

  oldPic <- queryImage (drawImage viewPort) world
  onImage (return . imageEvolution) world

  let selectedPic = select localMousePos oldPic

      newFeedback = case selectedPic of
        Just (PF.unWrap -> PF.SelectionTrigger fb _) -> Just fb
        _ -> Nothing
      oldFeedback = wLastFeedback

  let runFeedbackWithEvent (Just feedBack@(ExWrap wFeedback)) eventInfo = do
        onImage (runFeedbackOnImage feedBack eventInfo) world
        return $ getFocusCapture wFeedback eventInfo
      runFeedbackWithEvent Nothing _ =
        return FocusReleased

      runFeedbackOnImage :: Image i => ExWrap Feedback -> EventInfo ->
                            i -> IO i
      runFeedbackOnImage (ExWrap feedback) eventInfo =
        onBaseImage (runFeedback eventInfo feedback)

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
      -- TODO: cache `getPictureExt` calls in `CachedImage` as well
  let imageExt = getPictureExt $ drawImage viewPort image
      focusExt = enlargeExt 1.1 1.1 imageExt
      viewPort = viewStateViewPort wViewState

  windowSize <- getWindowSize
  onViewState (return . focusViewState focusExt windowSize) w

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
  handleEventStep (evolveImage secElapsed) event w'

drawWorld :: World -> IO Picture
drawWorld World{..} = do
  ServerImage image <- readMVar wImage

  let viewPort = viewStateViewPort wViewState
      picture = case wMousePos of
        Nothing       -> draw viewPort image
        Just mousePos -> toPicture $
                         selectWithBorder mousePos $
                         drawImage viewPort image

  return $ applyViewPortToPicture viewPort picture

initWorld :: Image i => i -> IO World
initWorld img = do
  image <- newMVar (ServerImage img)
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
    oRotate  = (CRotate , [])
    oRestore = (CRestore, [])

runServerWithConfig :: forall i. Image i => HTTPConfig -> Proxy i -> IO ()
runServerWithConfig config Proxy = do
  putStrLn $ printf "Server is running at %s..." (show config)
  world <- initWorld (initImage :: CachedImage i)

  void $ forkIO (render world)
  serverWith (toHTTPServerConfig config) (handler world)

httpOptions :: Parser HTTPConfig
httpOptions = HTTPConfig.httpOptions Server

runServer :: Image i => Proxy i -> IO ()
runServer proxy = do
  conf <- execParser (HTTPConfig.httpOptInfo Server)
  runServerWithConfig conf proxy
