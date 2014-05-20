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

 , defaultHttpConfig
 , httpOptions
 ) where

import System.Console.GetOpt
import System.Environment
import Network.HTTP.Server
import Network.Socket
import Network.URL
import Codec.Binary.UTF8.String

import qualified Graphics.UI.GLUT as GLUT

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewState hiding (Command)

import VisualTrace.Data.EventInfo
import VisualTrace.Data.Ext
import VisualTrace.Data.ViewState.Focus
import VisualTrace.Data.Ext.Utils
import qualified VisualTrace.Data.PictureF as PF
import VisualTrace.Data.Feedback
import VisualTrace.Data.PictureF.Selection(selectWithExt, select)
import VisualTrace.Data.PictureF.Trans(toPicture)

import Text.Printf
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Typeable as Typeable

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

import VisualTrace.Protocol.Image

type EventHandler = Event -> World -> IO World
data ServerImage = forall i. Image i => ServerImage i

data World = World
 { wViewState :: ViewState
 , wImage     :: MVar ServerImage
 , wMousePos  :: Maybe Point
 , wLastFeedback :: Maybe (ExWrap Feedback)
 , wEventHistory :: EventHistory
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

  let command :: String = rqBody req
      runCommand img = case interpretCommand command img of
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

  let selectedPic = fst $
                    select localMousePos id $
                    oldPic

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
      runFeedbackOnImage (ExWrap wrappedFeedback) eventInfo img =
        case Typeable.cast wrappedFeedback of
          Just fb -> runFeedback fb eventInfo img
          Nothing -> return img

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
  let imageExt = getPictureExt $ drawImage viewPort image
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
  handleEventStep (evolveImage secElapsed) event w'

drawWorld :: World -> IO Picture
drawWorld World{..} = do
  ServerImage image <- readMVar wImage
  let selectImage pic = case wMousePos of
        Nothing       -> pic
        Just mousePos -> snd $ selectWithExt mousePos pic

      picture = selectImage $ drawImage viewPort image

  return $
    applyViewPortToPicture viewPort $
      toPicture picture
  where
    viewPort = viewStateViewPort wViewState

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

runServerWithConfig :: Image i => Config -> i -> IO ()
runServerWithConfig config initImg = do
  putStrLn $ printf "Server is running at %s:%s..." (srvHost config) (show (srvPort config))
  world <- initWorld initImg

  void $ forkIO (render world)
  serverWith config (handler world)

runServer :: Image i => i -> IO ()
runServer initImg = do
  conf <- parseHttpOptions
  runServerWithConfig conf initImg

defaultHttpConfig :: Config
defaultHttpConfig =
  defaultConfig { srvHost = "localhost"
                , srvPort = 8888
                }

httpOptions :: [OptDescr (Config -> Config)]
httpOptions =
  [ Option ['h'] ["host"]
      (ReqArg (\h opts -> opts { srvHost = h }) "HOST")
      "Server host (\"localhost\" default)."
  , Option ['p'] ["port"]
      (ReqArg (\p opts -> opts { srvPort = fromInteger (read p) }) "PORT")
      "Port to listen (8888 default)."
  ]

parseHttpOptions :: IO Config
parseHttpOptions = do
  argv <- getArgs
  pname <- getProgName
  let header = printf "Usage: %s [OPTION...]" pname
  case getOpt Permute httpOptions argv of
    (o,_n,[] ) -> return $ foldl (flip id) defaultHttpConfig o
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header httpOptions))
