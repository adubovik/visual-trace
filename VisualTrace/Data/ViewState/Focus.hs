{-# language
   ViewPatterns
  #-}

module VisualTrace.Data.ViewState.Focus
 ( focusPic
 , focusViewState
 , focusTrans
 ) where

import Graphics.Gloss hiding (scale,translate)
import Graphics.Gloss.Data.ViewState
import VisualTrace.Data.ViewState.Utils
import VisualTrace.Data.Ext

focusPic :: Ext -> (Int, Int) -> (Picture -> Picture)
focusPic = focusTrans (uncurry Translate) (\s -> Scale s s)

focusViewState :: Ext -> (Int, Int) -> (ViewState -> ViewState)
focusViewState ext size =
  focusTrans translate scale ext size
  where
    scale factor = onViewPort
                 $ onViewPortScale (const factor)

    translate pos = onViewPort
                  $ onViewPortTranslate (const pos)

focusTrans ::
  ((Float,Float) -> a -> a) -> -- ^ xy translate transformation
  (Float -> a -> a) ->         -- ^ Uniform scale transformation
  Ext ->                       -- ^ Ext that depictes size of image
  (Int, Int) ->                -- ^ Size of window
  (a -> a)                     -- ^ Final transformation
focusTrans translate scale ext
           ( fromIntegral -> w
           , fromIntegral -> h)
  | Just ((cx,cy),(ex,ey)) <- getExt ext
  , let ratio = min (w/2/ex) (h/2/ey)
  = scale ratio
  . translate (-cx, -cy)
focusTrans _ _ _ _ = id
