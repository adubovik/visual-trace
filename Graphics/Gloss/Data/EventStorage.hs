module Graphics.Gloss.Data.EventStorage
 ( EventStorage
 , initEventStorage
 , updateEventStorage
 , isMousePressed
 , getMousePosHistory
 ) where

import Control.Arrow
import Control.Applicative
import qualified Data.Set as Set

import Graphics.Gloss.Interface.Pure.Game(Event(..), MouseButton(..), KeyState(..), Key(..), Point)

data EventStorage = EventStorage
  { mouseState :: Set.Set MouseButton
  , currMousePos :: Point
  , prevMousePos :: Point
  }

initEventStorage :: EventStorage
initEventStorage = EventStorage
  { mouseState = Set.empty
  , currMousePos = (100,100)
  , prevMousePos = (100,100)
  }

isMousePressed :: MouseButton -> EventStorage -> Bool
isMousePressed mb = Set.member mb . mouseState

getMousePosHistory :: EventStorage -> (Point,Point)
getMousePosHistory = prevMousePos &&& currMousePos

updateMouseState :: KeyState -> MouseButton ->
                    Set.Set MouseButton -> Set.Set MouseButton
updateMouseState Down = Set.insert
updateMouseState Up   = Set.delete

onMouseState :: (Set.Set MouseButton -> Set.Set MouseButton) ->
                EventStorage -> EventStorage
onMouseState f es = es { mouseState = f (mouseState es) }

saveCurrMousePos :: EventStorage -> EventStorage
saveCurrMousePos es = es { prevMousePos = currMousePos es }

onCurrMousePos :: (Point -> Point) ->
                  EventStorage -> EventStorage
onCurrMousePos f es = es { currMousePos = f (currMousePos es) }

updateEventStorage :: Event -> EventStorage -> EventStorage
updateEventStorage =
  (.) <$> updateMouseButtonState <*> updateMousePosHistory

updateMouseButtonState :: Event -> EventStorage -> EventStorage
updateMouseButtonState (EventKey (MouseButton mouseButton) state _ _) =
  onMouseState (updateMouseState state mouseButton)
updateMouseButtonState _ = id

updateMousePosHistory :: Event -> EventStorage -> EventStorage
updateMousePosHistory (EventKey _ _ _ mousePos) =
  onCurrMousePos (const mousePos) . saveCurrMousePos
updateMousePosHistory (EventMotion mousePos) =
  onCurrMousePos (const mousePos) . saveCurrMousePos
updateMousePosHistory _ = id



