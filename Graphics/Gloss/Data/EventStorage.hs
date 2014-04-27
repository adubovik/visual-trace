module Graphics.Gloss.Data.EventStorage
 ( EventStorage
 , initEventStorage
 , updateEventStorage
 , isMousePressed
 ) where

import qualified Data.Set as Set

import Graphics.Gloss.Interface.Pure.Game(Event(..), MouseButton(..), KeyState(..), Key(..))

data EventStorage = EventStorage
  { mouseState :: Set.Set MouseButton
  }

initEventStorage :: EventStorage
initEventStorage = EventStorage
  { mouseState = Set.empty
  }

isMousePressed :: MouseButton -> EventStorage -> Bool
isMousePressed mb = Set.member mb . mouseState

updateMouseState :: KeyState -> MouseButton ->
                    Set.Set MouseButton -> Set.Set MouseButton
updateMouseState Up   = Set.insert
updateMouseState Down = Set.delete

updateEventStorage :: Event -> EventStorage -> EventStorage
updateEventStorage (EventKey (MouseButton mb) state _modifiers _mousePos) es =
  es { mouseState = updateMouseState state mb (mouseState es) }
updateEventStorage _ es = es
