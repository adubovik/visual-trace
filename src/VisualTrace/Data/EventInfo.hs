module VisualTrace.Data.EventInfo
 ( Focus(..)
 , EventInfo(..)
 , FocusCapture(..)
 , Modifier(..)
 , EventHistory

 , initEventHistory
 , updateEventHistory

 , isMousePressed
 , wasMousePressed

 , isKeyPressed
 , wasKeyPressed

 , isModifierPressed
 , wasModifierPressed

 , getPrevMousePos
 , getCurrMousePos

 , onMousePosHistory
 ) where

import Control.Applicative
import qualified Data.Set as Set

import Graphics.Gloss.Interface.Pure.Game
  ( Point
  , Event(..)
  , MouseButton(..)
  , KeyState(..)
  , Key(..)
  , Modifiers(..)
  )

data Focus = FocusLost
           | FocusStill
           | FocusGained
  deriving Show

data FocusCapture = FocusCaptured
                  | FocusReleased
  deriving Show

data Modifier = Shift | Ctrl | Alt
  deriving (Show,Eq,Ord)

data EventInfo = EventInfo
  { efFocus :: Focus
  , efEvent :: Event
  , efEventHistory :: EventHistory
  }
  deriving Show

data EventStoryPoint = EventStoryPoint
  { mousePos   :: Point
  , windowSize :: (Int,Int)
  , modifiers  :: Set.Set Modifier
  , keys       :: Set.Set Key
  }
  deriving Show

type EventHistory = [EventStoryPoint]

-- EventStoryPoint modifiers

onMousePos :: (Point -> Point) -> (EventStoryPoint -> EventStoryPoint)
onMousePos f sp = sp { mousePos = f (mousePos sp) }

onWindowSize :: ((Int,Int) -> (Int,Int)) -> (EventStoryPoint -> EventStoryPoint)
onWindowSize f sp = sp { windowSize = f (windowSize sp) }

onModifiers :: (Set.Set Modifier -> Set.Set Modifier) -> (EventStoryPoint -> EventStoryPoint)
onModifiers f sp = sp { modifiers = f (modifiers sp) }

onKeys :: (Set.Set Key -> Set.Set Key) -> (EventStoryPoint -> EventStoryPoint)
onKeys f sp = sp { keys = f (keys sp) }

-- EventStoryPoint elements updaters

updateSet :: Ord a => KeyState -> a -> (Set.Set a -> Set.Set a)
updateSet Down = Set.insert
updateSet Up   = Set.delete

updateModifier :: KeyState -> Modifier -> (Set.Set Modifier -> Set.Set Modifier)
updateModifier = updateSet

updateKey :: KeyState -> Key -> Set.Set Key -> Set.Set Key
updateKey = updateSet

-- EventHistory modifiers

getPrev, getCurr :: EventHistory -> EventStoryPoint
getPrev = (!!1)
getCurr = (!!0)

onCurr :: (EventStoryPoint -> EventStoryPoint) ->
          (EventHistory -> EventHistory)
onCurr _ [] = []
onCurr f (eh:ehs) = f eh : ehs

onAll :: (EventStoryPoint -> EventStoryPoint) ->
         (EventHistory -> EventHistory)
onAll = fmap

initEventHistory :: EventHistory
initEventHistory = [initPoint,initPoint]
  where
    initPoint = EventStoryPoint
      { mousePos = (0,0)
      , windowSize = (500,500)
      , modifiers = Set.empty
      , keys = Set.empty
      }

-- EventHistory org machinery

copyHistoryPoint :: EventHistory -> EventHistory
copyHistoryPoint eh = take 2 $ head eh : eh

-- Keys & modifiers getters

isKeyPressedPoint :: Key -> EventStoryPoint -> Bool
isKeyPressedPoint k = Set.member k . keys

isModifierPressedPoint :: Modifier -> EventStoryPoint -> Bool
isModifierPressedPoint m = Set.member m . modifiers

isKeyPressed :: Key -> EventHistory -> Bool
isKeyPressed k = isKeyPressedPoint k . getCurr

wasKeyPressed :: Key -> EventHistory -> Bool
wasKeyPressed k = isKeyPressedPoint k . getPrev

isModifierPressed :: Modifier -> EventHistory -> Bool
isModifierPressed m = isModifierPressedPoint m . getCurr

wasModifierPressed :: Modifier -> EventHistory -> Bool
wasModifierPressed m = isModifierPressedPoint m . getPrev

isMousePressed :: MouseButton -> EventHistory -> Bool
isMousePressed = isKeyPressed . MouseButton

wasMousePressed :: MouseButton -> EventHistory -> Bool
wasMousePressed = wasKeyPressed . MouseButton

-- Mouse position

getMousePos :: EventStoryPoint -> Point
getMousePos = mousePos

getCurrMousePos :: EventHistory -> Point
getCurrMousePos = getMousePos . getCurr

getPrevMousePos :: EventHistory -> Point
getPrevMousePos = getMousePos . getPrev

-- EventHistory updates

updateEventHistory :: (Int,Int) -> Event ->
                      EventHistory -> EventHistory
updateEventHistory winSize =
      updateKeys
  <.> updateModifiers
  <.> updateMousePos
  <.> updateWindowSize
  <.> (const updateWinSize)
  <.> (const copyHistoryPoint)
  where
    updateWinSize = onCurr $ onWindowSize (const winSize)
    (<.>) = liftA2 (.)

updateWindowSize :: Event -> EventHistory -> EventHistory
updateWindowSize (EventResize winSize) = onCurr $ onWindowSize (const winSize)
updateWindowSize _ = id

updateKeys :: Event -> EventHistory -> EventHistory
updateKeys (EventKey key state _ _) = onCurr $ onKeys (updateKey state key)
updateKeys _ = id

updateModifiers :: Event -> EventHistory -> EventHistory
updateModifiers (EventKey _ _ mds _) =
    onCurr (onModifiers (updateModifier (shift mds) Shift))
  . onCurr (onModifiers (updateModifier (ctrl  mds) Ctrl ))
  . onCurr (onModifiers (updateModifier (alt   mds) Alt  ))
updateModifiers _ = id

updateMousePos :: Event -> EventHistory -> EventHistory
updateMousePos (EventKey _ _ _ mousepos) = onCurr $ onMousePos (const mousepos)
updateMousePos (EventMotion    mousepos) = onCurr $ onMousePos (const mousepos)
updateMousePos _ = id

onMousePosHistory :: (Point -> Point) -> (EventHistory -> EventHistory)
onMousePosHistory = onAll . onMousePos
