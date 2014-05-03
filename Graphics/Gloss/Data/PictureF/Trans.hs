{-# language
   ViewPatterns
 , TupleSections
 #-}

module Graphics.Gloss.Data.PictureF.Trans
 ( toPicture
 , fromPicture
 , eliminateFixedSize
 , eliminateVHCat
 ) where

import Data.Monoid
import Data.Fix
import Data.List

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Matrix
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.Ext
import Graphics.Gloss.Data.Ext.Utils

toPicture :: ViewPort -> Picture -> G.Picture
toPicture viewPort =
  cata alg . eliminateVHCat . eliminateFixedSize viewPort
  where
    alg :: PictureF G.Picture -> G.Picture
    alg pic = case pic of
      Blank             -> G.Blank
      Polygon p         -> G.Polygon p
      Line p            -> G.Line p
      Circle a          -> G.Circle a
      ThickCircle a b   -> G.ThickCircle a b
      Arc a b c         -> G.Arc a b c
      ThickArc a b c d  -> G.ThickArc a b c d
      Text s            -> G.Text s
      Bitmap a b c d    -> G.Bitmap a b c d
      Color c a         -> G.Color c a
      Translate a b c   -> G.Translate a b c
      Rotate a b        -> G.Rotate a b
      Scale a b c       -> G.Scale a b c
      Pictures p        -> G.Pictures p
      FixedSize{}       -> error "toPicture: FixedSize primitive shouldn't appear at this stage."
      HCat{}            -> error "toPicture: HCat primitive shouldn't appear at this stage."
      VCat{}            -> error "toPicture: VCat primitive shouldn't appear at this stage."
      Group _ p         -> p
      Annotate _ p      -> p
      SelectionTrigger _ p -> p

eliminateFixedSize :: ViewPort -> Picture -> Picture
eliminateFixedSize (viewPortToMatrix -> viewPortMatrix) =
  cataCtx iterateMatrix alg viewPortMatrix
  where
    iterateMatrix :: Matrix -> PictureF () -> Matrix
    iterateMatrix m pic = m <> getMatrix pic

    alg :: Matrix -> PictureF Picture -> Picture
    alg m pic = case pic of
      -- FIXME: get rid of multiple 'getPictureExt' calls
      FixedSize mw mh p -> let ext  = getPictureExt p
                               ext' = applyMatrixToExt m ext
                           in  fixSizeExt mw mh ext' p
      _ -> wrap pic

eliminateVHCat :: Picture -> Picture
eliminateVHCat = cata alg
  where
    alg :: PictureF Picture -> Picture
    alg pic = case pic of
      VCat padding ps ->
        pictures $ snd $ mapAccumL (catFolder False padding) mempty ps
      HCat padding ps ->
        pictures $ snd $ mapAccumL (catFolder  True padding) mempty ps
      _ -> wrap pic

    -- FIXME: copy-paste from Graphics.Gloss.Data.Ext.Utils.ext2Alg
    catFolder isHCat padding extAcc pic
      | Just ((cx ,cy ),(ex ,ey )) <- getExt extAcc
      , Just ((cx',cy'),(ex',ey')) <- getExt extPic
      , let realMinX   = cx' - ex'
      , let realMinY   = cy' - ey'
      , let targetMinX = cx + ex + padding
      , let targetMinY = cy + ey + padding
      , let transX = targetMinX - realMinX
      , let transY = targetMinY - realMinY
      = case isHCat of
          True  -> ( extAcc <> translateExt transX 0.0 extPic
                   , translate transX 0.0 pic
                   )
          False -> ( extAcc <> translateExt 0.0 transY extPic
                   , translate 0.0 transY pic
                   )
      | otherwise = (extPic, pic)
      where
        extPic = getPictureExt pic

fromPicture :: G.Picture -> Picture
fromPicture = ana coalg
  where
    coalg pic = case pic of
      G.Blank            -> Blank
      G.Polygon p        -> Polygon p
      G.Line p           -> Line p
      G.Circle a         -> Circle a
      G.ThickCircle a b  -> ThickCircle a b
      G.Arc a b c        -> Arc a b c
      G.ThickArc a b c d -> ThickArc a b c d
      G.Text s           -> Text s
      G.Bitmap a b c d   -> Bitmap a b c d
      G.Color c a        -> Color c a
      G.Translate a b c  -> Translate a b c
      G.Rotate a b       -> Rotate a b
      G.Scale a b c      -> Scale a b c
      G.Pictures p       -> Pictures p
