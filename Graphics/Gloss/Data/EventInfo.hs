module Graphics.Gloss.Data.EventInfo
 ( Focus(..)
 , EventInfo(..)
 , FocusCapture(..)
 , EventHistory
 , initEventHistory
 , updateEventHistory
 , isMousePressed
 , wasMousePressed
 , getMousePosHistory
 , getPrevMousePos
 , getCurrMousePos
 , onMousePosBoth
 ) where

import Control.Arrow
import Control.Applicative
import qualified Data.Set as Set

import Graphics.Gloss.Interface.Pure.Game(Event(..), MouseButton(..), KeyState(..), Key(..), Point)

data Focus = FocusLost
           | FocusStill
           | FocusGained
  deriving Show

data FocusCapture = FocusCaptured
                  | FocusReleased
  deriving Show

data EventInfo = EventInfo
  { efFocus :: Focus
  , efEvent :: Event
  , efEventHistory :: EventHistory
  }
  deriving Show

data EventStoryPoint = EventStoryPoint
  { mouseState :: Set.Set MouseButton
  , mousePos :: Point
  }
  deriving Show

data EventHistory = EventHistory
  { prevPoint :: EventStoryPoint
  , currPoint :: EventStoryPoint
  }
  deriving Show

initEventHistory :: EventHistory
initEventHistory = EventHistory initPoint initPoint
  where
    initPoint = EventStoryPoint
      { mouseState = Set.empty
      , mousePos = (100,100)
      }

isMousePressedPoint :: MouseButton -> EventStoryPoint -> Bool
isMousePressedPoint mb = Set.member mb . mouseState

isMousePressed :: MouseButton -> EventHistory -> Bool
isMousePressed mb = isMousePressedPoint mb . currPoint

wasMousePressed :: MouseButton -> EventHistory -> Bool
wasMousePressed mb = isMousePressedPoint mb . prevPoint

getMousePosHistory :: EventHistory -> (Point,Point)
getMousePosHistory = getPrevMousePos &&& getCurrMousePos

getMousePos :: EventStoryPoint -> Point
getMousePos = mousePos

getCurrMousePos :: EventHistory -> Point
getCurrMousePos = getMousePos . currPoint

getPrevMousePos :: EventHistory -> Point
getPrevMousePos = getMousePos . prevPoint

updateMouseState :: KeyState -> MouseButton ->
                    Set.Set MouseButton -> Set.Set MouseButton
updateMouseState Down = Set.insert
updateMouseState Up   = Set.delete

saveCurrHistory :: EventHistory -> EventHistory
saveCurrHistory es = es { prevPoint = currPoint es }

onMouseState :: (Set.Set MouseButton -> Set.Set MouseButton) ->
                EventStoryPoint -> EventStoryPoint
onMouseState f p = p { mouseState = f (mouseState p) }

onCurrMouseState :: (Set.Set MouseButton -> Set.Set MouseButton) ->
                    EventHistory -> EventHistory
onCurrMouseState f es = es { currPoint = onMouseState f (currPoint es) }

onMousePos :: (Point -> Point) ->
              EventStoryPoint -> EventStoryPoint
onMousePos f p = p { mousePos = f (mousePos p) }

onMousePosBoth :: (Point -> Point) -> EventHistory -> EventHistory
onMousePosBoth = (.) <$> onCurrMousePos <*> onPrevMousePos

onCurrMousePos :: (Point -> Point) ->
                  EventHistory -> EventHistory
onCurrMousePos f es = es { currPoint = onMousePos f (currPoint es) }

onPrevMousePos :: (Point -> Point) ->
                  EventHistory -> EventHistory
onPrevMousePos f es = es { prevPoint = onMousePos f (prevPoint es) }

updateEventHistory :: Event -> EventHistory -> EventHistory
updateEventHistory =
  (.) <$> updateMouseButtonState <*> updateMousePosHistory

updateMouseButtonState :: Event -> EventHistory -> EventHistory
updateMouseButtonState (EventKey (MouseButton mouseButton) state _ _) =
  onCurrMouseState (updateMouseState state mouseButton) . saveCurrHistory
updateMouseButtonState _ = id

updateMousePosHistory :: Event -> EventHistory -> EventHistory
updateMousePosHistory (EventKey _ _ _ mousepos) =
  onCurrMousePos (const mousepos) . saveCurrHistory
updateMousePosHistory (EventMotion mousepos) =
  onCurrMousePos (const mousepos) . saveCurrHistory
updateMousePosHistory _ = id
