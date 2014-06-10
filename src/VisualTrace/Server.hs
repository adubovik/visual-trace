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
 , httpOptInfo
 , HTTPConfig(..)

 , noBackground
 , gridBackground
 ) where

import Prelude hiding (init)

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

import VisualTrace.Grid
import VisualTrace.Data.EventInfo
import VisualTrace.Data.Ext
import VisualTrace.Data.ViewState.Focus
import VisualTrace.Data.Ext.Utils
import VisualTrace.Data.Feedback
import VisualTrace.Data.Feedback.FeedbackStorage

import VisualTrace.Protocol.Image
import VisualTrace.Protocol.Image.CachedImage

type EventHandler = Event -> World -> IO World
data ServerImage = forall i. Image i => ServerImage (ImageGroup i)

type ImageTransformM m = forall i. Image i => ImageGroup i -> m (ImageGroup i)
type ImageTransform    = forall i. Image i => ImageGroup i -> ImageGroup i
type ImageQuery q      = forall i. Image i => ImageGroup i -> q

type WindowSize = (Int,Int)
type BackgroudPicture = WindowSize -> (Point -> Point) -> Picture

data World = World
 { wViewState :: !ViewState
 , wImage     :: !(MVar ServerImage)
 , wMousePos  :: !Point
 , wLastFeedback :: !(Maybe (ExWrap Feedback))
 , wEventHistory :: !EventHistory
 , wBackground   :: BackgroudPicture
 }

onServerImage :: Functor m => ImageTransformM m ->
                 ServerImage -> m ServerImage
onServerImage f (ServerImage i) = ServerImage <$> f i

queryImage :: ImageQuery q -> World -> IO q
queryImage f w = do
  ServerImage i <- readMVar (wImage w)
  return $ f i

onImage :: ImageTransformM IO -> World -> IO ()
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

getWindowSize :: IO WindowSize
getWindowSize = do
  GLUT.Size width height <- GLUT.get GLUT.windowSize
  return (fromIntegral width, fromIntegral height)

handler :: World -> SockAddr -> URL -> Request String -> IO (Response String)
handler world _addr _url req = do
  putStrLn $ "Received: " ++ rqBody req

  let cmd :: String = rqBody req
      runCommand img = case interpret cmd img of
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
      return
      -- _traceHook event
  >=> onEventHistory (         updateEventHistoryIO     event)
  >=> eh event
  >=> onViewState    (return . updateViewStateWithEvent event)
  where
    _traceHook :: Event -> World -> IO World
    _traceHook e world = do
      putStrLn $ "TraceHook: Event = " ++ show e
      return world

updateEventHistoryIO :: Event -> EventHistory -> IO EventHistory
updateEventHistoryIO event eh = do
  windowSize <- getWindowSize
  return $ updateEventHistory windowSize event eh

handleEventStep :: ImageTransform -> Event -> World -> IO World
handleEventStep imageEvolution event world@World{..} = do
  let mousePos = getCurrMousePos wEventHistory
      viewPort = viewStateViewPort wViewState
      localMousePos = invertViewPort viewPort mousePos

  let getFeedback image = getFdFeedback <$>
                          getFeedbackDataUnderPoint viewPort image
                          localMousePos

  let oldFeedback = wLastFeedback
  newFeedback <- queryImage getFeedback world

  onImage (return . imageEvolution) world

  let runFeedbackWithEvent (Just feedBack@(ExWrap wFeedback)) eventInfo = do
        onImage (runFeedbackOnImage feedBack eventInfo) world
        return $ getFocusCapture wFeedback eventInfo
      runFeedbackWithEvent Nothing _ =
        return FocusReleased

      runFeedbackOnImage :: ExWrap Feedback -> EventInfo -> ImageTransformM IO
      runFeedbackOnImage (ExWrap feedback) eventInfo =
        onImageGroup (runFeedback eventInfo feedback)

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

  return $ world { wMousePos = localMousePos
                 , wLastFeedback = newFeedback'
                 }

eventHandler :: EventHandler
eventHandler e =
      handleEventStep id e
  >=> rescaleHandler e

rescaleHandler :: EventHandler
rescaleHandler (EventKey (Char 'r') Down _mod _pos) w@World{..} = do
  ServerImage image <- readMVar wImage
      -- TODO: cache `getPictureExt` calls in `CachedImage` as well (?)
  let imageExt = getPictureExt $ drawSimpl viewPort image
      focusExt = enlargeExt 1.1 1.1 imageExt
      viewPort = viewStateViewPort wViewState

  windowSize <- getWindowSize
  onViewState (return . focusViewState focusExt windowSize) w
rescaleHandler _e w = return w

timeEvolution :: Float -> World -> IO World
timeEvolution secElapsed w = do
  let emitFakeEvent world = do
        world' <- onEventHistory (updateEventHistoryIO event) world
        return (event, world')
        where
          mousePos = getCurrMousePos $ wEventHistory world
          event = EventMotion mousePos

  (event, w') <- emitFakeEvent w
  handleEventStep (evolve secElapsed) event w'

drawWorld :: World -> IO Picture
drawWorld world@World{..} = do
  let viewPort = viewStateViewPort wViewState
      getPicture image = drawWithBorder viewPort image wMousePos
  picture <- queryImage getPicture world

  windowSize <- getWindowSize
  let backgroud = wBackground windowSize (invertViewPort viewPort)

  return $ applyViewPortToPicture viewPort $
    pictures [ backgroud, picture ]

initWorld :: BackgroudPicture -> ImageQuery (IO World)
initWorld backgroud img = do
  image <- newMVar (ServerImage img)
  return $ World
    { wViewState = viewStateInitWithConfig $ Map.toList $
                     Map.fromList commandConfig <>
                     Map.fromList defaultCommandConfig
    , wImage = image
    , wMousePos = (0,0)
    , wLastFeedback = Nothing
    , wEventHistory = initEventHistory
    , wBackground = backgroud
    }
  where
    commandConfig = [oTranslate, oRotate, oRestore]
    oTranslate = ( CTranslate
                 , [ ( MouseButton RightButton
                     , Just (Modifiers { shift = Up, ctrl = Up, alt = Up })
                     )])
    oRotate  = (CRotate , [])
    oRestore = (CRestore, [])

runServerWithConfig :: forall i. Image i =>
                       BackgroudPicture -> HTTPConfig ->
                       Proxy i -> IO ()
runServerWithConfig backgroud config Proxy = do
  putStrLn $ printf "Server is running at %s..." (show config)
  world <- initWorld backgroud (init :: ImageGroup (CachedImage i))

  void $ forkIO (render world)
  serverWith (toHTTPServerConfig config) (handler world)

httpOptions :: Parser HTTPConfig
httpOptions = HTTPConfig.httpOptions Server

httpOptInfo :: ParserInfo HTTPConfig
httpOptInfo = HTTPConfig.httpOptInfo Server

noBackground, gridBackground :: BackgroudPicture
noBackground = const $ const blank
gridBackground = drawGrid

runServer :: Image i => BackgroudPicture -> Proxy i -> IO ()
runServer backgroud proxy = do
  conf <- execParser (HTTPConfig.httpOptInfo Server)
  runServerWithConfig backgroud conf proxy
