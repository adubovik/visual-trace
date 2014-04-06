{-# language
   ViewPatterns
  #-}

module Graphics.Gloss.Utils (
   getPictureExt
 , getBasicExt
 , focusPic
 , focusViewState
 , focusTrans
 ) where

import Data.Monoid

import Graphics.Gloss.Data.ExtentF
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Data.ViewState.Utils
import Graphics.Gloss
import qualified Graphics.Gloss.Data.PictureF as P
import Data.Fix

getPictureExt :: Picture -> Ext
getPictureExt = getPictureExtP . P.fromPicture

getPictureExtP :: P.Picture () -> Ext
getPictureExtP = cata (alg' . P.deAnn)
  where
    alg' p = getBasicExt p <> alg p

    alg P.Blank                 = mempty
    alg (P.Polygon path)        = mempty
    alg (P.Line    path)        = mempty
    alg (P.Circle   rad)        = mempty
    alg (P.ThickCircle th rad)  = mempty
    alg (P.Arc _ _ rad)         = mempty
    alg (P.ThickArc _ _ rad th) = mempty
    alg (P.Text str)            = mempty
    alg (P.Bitmap w h _ _)      = mempty
    alg (P.Color _ p)           = p
    alg (P.Translate x y p)     = translateExt x y p
    alg (P.Rotate _ p)          = p -- FIX ME!
    alg (P.Scale x y p)         = scaleExt x y p
    alg (P.Pictures ps)         = mconcat ps

getBasicExt :: P.PictureF a -> Ext
getBasicExt P.Blank                 = mempty
getBasicExt (P.Polygon path)        = mconcat $ map pointExt path
getBasicExt (P.Line    path)        = mconcat $ map pointExt path
getBasicExt (P.Circle   rad)        = scaleExt rad rad unitExt
getBasicExt (P.ThickCircle th rad)  = scaleExt (th+rad) (th+rad) unitExt
getBasicExt (P.Arc _ _ rad)         = scaleExt rad rad unitExt
getBasicExt (P.ThickArc _ _ rad th) = scaleExt (th+rad) (th+rad) unitExt
getBasicExt (P.Text str)            =
  let h = 10.0 -- FIX ME!
      w = fromIntegral $ 10 * length str
  in translateExt (w/2.0) (h/2.0) .
     scaleExt w h $
     unitExt
getBasicExt (P.Bitmap w h _ _)      =
  scaleExt (fromIntegral w) (fromIntegral h) unitExt
getBasicExt _ = mempty

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
