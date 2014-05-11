{-# language
   ViewPatterns
 , TupleSections
 #-}

module Graphics.Gloss.Data.PictureF.Trans
 ( toPicture
 , fromPicture
 , desugarePicture
 ) where

import Control.Applicative
import Data.Monoid
import Data.Fix
import Data.List
import Text.Printf

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Matrix
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.Ext
import Graphics.Gloss.Data.Ext2
import Graphics.Gloss.Data.Ext.Utils

-- TODO: fuse into one morphism
desugarePicture :: ViewPort -> Picture -> Picture
desugarePicture viewPort =
  eliminateInsidePrimitives .
  eliminateVHCat .
  eliminateFixedSize viewPort

toPicture :: ViewPort -> Picture -> G.Picture
toPicture viewPort = cata alg . desugarePicture viewPort
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
      Group _ p         -> p
      SelectionTrigger _ p -> p
      FixedSize{}       -> err "FixedSize"
      HCat{}            -> err "HCat"
      VCat{}            -> err "VCat"
      InsideRect{}      -> err "InsideRect"

    err = error . printf "toPicture: %s primitive shouldn't \
                         \appear at this stage."

eliminateFixedSize :: ViewPort -> Picture -> Picture
eliminateFixedSize (viewPortToMatrix -> viewPortMatrix) =
  fst . cataCtx iterateMatrix alg viewPortMatrix
  where
    iterateMatrix :: Matrix -> PictureF () -> Matrix
    iterateMatrix m pic = m <> getMatrix pic

    alg :: Matrix -> PictureF (Picture,Ext2) -> (Picture,Ext2)
    alg m picture = (,ext) $ case picture of
      FixedSize mw mh (p,e) -> let e' = applyMatrixToExt m $
                                        flattenExt2 e
                               in  fixSizeExt mw mh e' p
      _ -> wrap pic

      where
        pic = fst <$> picture
        ext = ext2Alg $
              snd <$> picture

eliminateVHCat :: Picture -> Picture
eliminateVHCat = fst . cata alg
  where
    alg :: PictureF (Picture,Ext2) -> (Picture,Ext2)
    alg picture = (,ext) $ case picture of
      VCat padding ps ->
        pictures $ snd $ mapAccumL (catFolder False padding) mempty ps
      HCat padding ps ->
        pictures $ snd $ mapAccumL (catFolder  True padding) mempty ps
      _ -> wrap pic
      where
        pic = fst <$> picture
        ext = ext2Alg $
              snd <$> picture

    -- FIXME: copy-paste from Graphics.Gloss.Data.Ext.Utils.ext2Alg
    catFolder isHCat padding extAcc (pic, flattenExt2 -> extPic)
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

eliminateInsidePrimitives :: Picture -> Picture
eliminateInsidePrimitives = fst . cata alg
  where
    alg :: PictureF (Picture,Ext2) -> (Picture,Ext2)
    alg picture = (,ext) $ case picture of
      InsideRect filling padding clr (p, flattenExt2 -> e) ->
        let e'    = enlargeExtAbs padding padding e
            rect  = drawExt filling e'
            rect' = maybe id color clr rect
        in  pictures [ rect'
                     , p
                     ]
      _ -> wrap pic
     where
        pic = fst <$> picture
        ext = ext2Alg $
              snd <$> picture

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
