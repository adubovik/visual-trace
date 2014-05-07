{-# language
   RecordWildCards
 #-}

module Graphics.Gloss.Data.EventInfo.Utils where

import Graphics.Gloss.Interface.Pure.Game(MouseButton(..), Point)

import Graphics.Gloss.Data.EventInfo

import Control.Monad
import Control.Applicative

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) = liftM2 (&&)

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) = liftM2 (||)

mouseButtonDrag :: MouseButton -> EventHistory -> Bool
mouseButtonDrag mouseButton =
   isMousePressed mouseButton .&&.
  wasMousePressed mouseButton

type FocusStrategy = EventInfo -> FocusCapture

stdFocusCapture :: FocusStrategy
stdFocusCapture = keepFocusedIf (const False)

keepFocusedIf :: (EventHistory -> Bool) -> FocusStrategy
keepFocusedIf keepFocus EventInfo{..}
  | FocusLost <- efFocus
  , keepFocus efEventHistory
  = FocusCaptured
  | FocusStill <- efFocus
  = FocusCaptured
  | otherwise
  = FocusReleased

type Transformer a = EventInfo -> (a -> a)
type TransformOnEvent a = (a -> a) -> Transformer a

onHoverIn :: TransformOnEvent a
onHoverIn trans EventInfo{..}
  | FocusGained <- efFocus
  = trans
  | otherwise
  = id

onHoverOut :: FocusStrategy -> TransformOnEvent a
onHoverOut focusCapture trans ef@EventInfo{..}
  | FocusLost <- efFocus
  , FocusReleased <- focusCapture ef
  = trans
  | otherwise
  = id

onMouseDrag :: FocusStrategy -> MouseButton ->
               ((Point -> Point -> a -> a) -> Transformer a)
onMouseDrag focusCapture mouseButton trans ef@EventInfo{..}
  | FocusCaptured <- focusCapture ef
  , mouseButtonDrag mouseButton efEventHistory
  , newPos <- getCurrMousePos efEventHistory
  , oldPos <- getPrevMousePos efEventHistory
  = trans oldPos newPos
  | otherwise
  = id

onMouseMove :: FocusStrategy ->
               ((Point -> Point -> a -> a) -> Transformer a)
onMouseMove focusCapture trans ef@EventInfo{..}
  | FocusCaptured <- focusCapture ef
  , newPos <- getCurrMousePos efEventHistory
  , oldPos <- getPrevMousePos efEventHistory
  = trans oldPos newPos
  | otherwise
  = id

andWhen :: Transformer a -> Transformer a -> Transformer a
andWhen x y = (.) <$> x <*> y



