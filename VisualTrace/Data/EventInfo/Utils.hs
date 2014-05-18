{-# language
   RecordWildCards
 #-}

module VisualTrace.Data.EventInfo.Utils where

import Graphics.Gloss.Interface.Pure.Game(MouseButton(..), Point)
import VisualTrace.Data.EventInfo

import Control.Monad

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

type Transformer a = FocusCapture -> EventInfo -> (a -> a)
type Trans a = (a -> a) -> Transformer a

andWhen :: Transformer a -> Transformer a -> Transformer a
andWhen x y = \a b -> x a b .
                      y a b

-- Transformers

onHoverIn :: Trans a
onHoverIn trans _focusCapture EventInfo{..}
  | FocusGained <- efFocus
  = trans
  | otherwise
  = id

onHoverOut :: Trans a
onHoverOut trans focusCapture EventInfo{..}
  | FocusLost <- efFocus
  , FocusReleased <- focusCapture
  = trans
  | otherwise
  = id

onMouseDrag :: MouseButton ->
               (Point -> Point -> a -> a) ->
               Transformer a
onMouseDrag mouseButton trans focusCapture EventInfo{..}
  | FocusCaptured <- focusCapture
  , mouseButtonDrag mouseButton efEventHistory
  , newPos <- getCurrMousePos efEventHistory
  , oldPos <- getPrevMousePos efEventHistory
  = trans oldPos newPos
  | otherwise
  = id

onMouseMove :: (Point -> Point -> a -> a) ->
               Transformer a
onMouseMove trans focusCapture EventInfo{..}
  | FocusCaptured <- focusCapture
  , newPos <- getCurrMousePos efEventHistory
  , oldPos <- getPrevMousePos efEventHistory
  = trans oldPos newPos
  | otherwise
  = id
