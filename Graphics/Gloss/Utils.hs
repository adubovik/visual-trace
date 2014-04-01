{-# language
   ViewPatterns
  #-}

module Graphics.Gloss.Utils (
   getPictureExt
 , focusPic
 , focusViewState
 , focusTrans
 ) where

import Data.Monoid

import Graphics.Gloss.Data.ExtentF
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Data.ViewState.Utils
import Graphics.Gloss


getPictureExt :: Picture -> Ext
getPictureExt Blank = mempty
getPictureExt (Polygon path) = mconcat $ map pointExt path
getPictureExt (Line    path) = mconcat $ map pointExt path
getPictureExt (Circle   rad) = scaleExt rad rad unitExt
getPictureExt (ThickCircle th rad) = scaleExt (th+rad) (th+rad) unitExt
getPictureExt (Arc _ _ rad) = scaleExt rad rad unitExt
getPictureExt (ThickArc _ _ rad th) = scaleExt (th+rad) (th+rad) unitExt
getPictureExt (Text str) =
  let h = 10.0 -- FIX ME!
      w = fromIntegral $ 10 * length str
  in translateExt (w/2.0) (h/2.0) .
     scaleExt w h $
     unitExt
getPictureExt (Bitmap w h _ _) =
  scaleExt (fromIntegral w) (fromIntegral h) unitExt
getPictureExt (Color _ p) = getPictureExt p
getPictureExt (Translate x y p) = translateExt x y $ getPictureExt p
getPictureExt (Rotate _ p) = getPictureExt p -- FIX ME!
getPictureExt (Scale x y p) = scaleExt x y $ getPictureExt p
getPictureExt (Pictures ps) = mconcat $ map getPictureExt ps

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
