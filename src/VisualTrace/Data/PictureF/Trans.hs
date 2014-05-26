{-# language
   ViewPatterns
 , TupleSections
 , RecordWildCards
 , PatternGuards
 #-}

module VisualTrace.Data.PictureF.Trans
 ( toPicture
 , fromPicture
 , desugarePicture
 , flattenPicture
 ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Arrow
import Control.Applicative
import Data.List
import Text.Printf

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.ViewPort

import VisualTrace.Data.Fix
import VisualTrace.Data.Matrix
import VisualTrace.Data.PictureF hiding (local)
import VisualTrace.Data.Ext
import VisualTrace.Data.Ext.Utils

expandScreenCoordinates :: ViewPort -> PictureG -> PictureL
expandScreenCoordinates ViewPort{..} =
  cata alg
  where
    alg :: PictureFG PictureL -> PictureL
    alg picture =
      wrap $ expandGFloat viewPortTranslate viewPortScale picture

    expandGFloat :: G.Point -> Float -> PictureFG a -> PictureFL a
    expandGFloat (transX,transY) scaleXY pic = case pic of
      Blank        -> Blank
      Line f ps    -> Line f $ map transPoint ps
      Arc t as pos rad -> Arc t as (transPoint pos) (transCoord rad)
      Text pos s   -> Text (transPoint pos) s

      Translate x y p -> Translate (transCoord x) (transCoord y) p
      Scale     x y p -> Scale     (transCoord x) (transCoord y) p
      Color       c p -> Color c p

      Pictures ps -> Pictures ps

      SelectionTrigger fp p -> SelectionTrigger fp p
      InsideRect padding f c p -> InsideRect (transCoord padding) f c p
      VCat padding ps -> VCat (transCoord padding) ps
      HCat padding ps -> HCat (transCoord padding) ps

      where
        sc    = (/scaleXY)
        scTrX = (+transX) . (/scaleXY)
        scTrY = (+transY) . (/scaleXY)

        transCoord = onG sc
        transPoint = onG scTrX *** onG scTrY

        onG :: (Float -> Float) -> GFloat -> Float
        onG g (Screen f) = g f
        onG _ (Local  f) = f

-- TODO: fuse into one morphism
desugarePicture :: ViewPort -> PictureG -> PictureL
desugarePicture viewPort =
  expandInsidePrimitives .
  expandVHCat .
  expandScreenCoordinates viewPort

type FlatEnv = (Last Color, Matrix)
type FlatM = ReaderT FlatEnv (Writer [PictureL])

onMatrix :: (Matrix -> Matrix) -> FlatEnv -> FlatEnv
onMatrix = second

onColor :: (Last Color -> Last Color) -> FlatEnv -> FlatEnv
onColor = first

flattenPicture :: PictureL -> PictureL
flattenPicture =
  pictures .
  snd . runWriter .
  flip runReaderT mempty .
  cata alg
  where
    tellPrimitive pic = do
      (clr, matrix) <- ask
      tell [applyMatrixToPrimitive (getLast clr) matrix pic]

    applyMatrixToPrimitive :: Maybe Color -> Matrix -> PictureFL a -> PictureL
    applyMatrixToPrimitive clr mat = toColor clr . toTransform . toPictureL
      where
        toPictureL = wrap . fmap (const blank)

        toTransform = let (mx,my) = mTranslate mat
                          (sx,sy) = mScale mat
                      in scale     sx sy .
                         translate mx my

        toColor Nothing  = id
        toColor (Just c) = color c

    alg :: PictureFL (FlatM ()) -> FlatM ()
    alg pic = case pic of
      Blank  -> return ()
      Line{} -> tellPrimitive pic
      Arc{}  -> tellPrimitive pic
      Text{} -> tellPrimitive pic

      Translate x y p -> local (onMatrix (<> identityTranslate (x,y))) p
      Scale     x y p -> local (onMatrix (<> identityScale     (x,y))) p
      Color       c p -> local (onColor  (<> Last (Just c)          )) p

      Pictures     ps -> sequence_ ps
      SelectionTrigger _ p -> p

      HCat{}            -> err "HCat"
      VCat{}            -> err "VCat"
      InsideRect{}      -> err "InsideRect"

    err = error . printf "flattenPicture: %s primitive shouldn't \
                         \appear at this stage."

toPicture :: PictureL -> G.Picture
toPicture = cata alg
  where
    alg :: PictureFL G.Picture -> G.Picture
    alg pic = case pic of
      Blank             -> G.Blank
      Line Fill   p     -> G.Polygon p
      Line NoFill p     -> G.Line p
      Arc  Nothing        Nothing pos r -> translateTo pos $ G.Circle r
      Arc (Just t)        Nothing pos r -> translateTo pos $ G.ThickCircle t r
      Arc  Nothing (Just (a1,a2)) pos r -> translateTo pos $ G.Arc a1 a2 r
      Arc (Just t) (Just (a1,a2)) pos r -> translateTo pos $ G.ThickArc a1 a2 r t
      Text pos s                        -> translateTo pos $ G.Text s
      Color c a         -> G.Color c a
      Translate a b c   -> G.Translate a b c
      Scale a b c       -> G.Scale a b c
      Pictures p        -> G.Pictures p
      SelectionTrigger _ p -> p
      HCat{}            -> err "HCat"
      VCat{}            -> err "VCat"
      InsideRect{}      -> err "InsideRect"

    translateTo (x,y) = G.Translate x y

    err = error . printf "toPicture: %s primitive shouldn't \
                         \appear at this stage."

expandVHCat :: PictureL -> PictureL
expandVHCat = fst . cata alg
  where
    alg :: PictureFL (PictureL,Ext) -> (PictureL,Ext)
    alg picture = (,ext) $ case picture of
      VCat padding ps ->
        pictures $ snd $ mapAccumL (catFolder False padding) mempty ps
      HCat padding ps ->
        pictures $ snd $ mapAccumL (catFolder  True padding) mempty ps
      _ -> wrap pic
      where
        pic = fst <$> picture
        ext = extAlg $
              snd <$> picture

    -- FIXME: copy-paste from VisualTrace.Data.Ext.Utils.ext2Alg
    catFolder isHCat padding extAcc (pic, extPic)
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

expandInsidePrimitives :: PictureL -> PictureL
expandInsidePrimitives = fst . cata alg
  where
    alg :: PictureFL (PictureL,Ext) -> (PictureL,Ext)
    alg picture = (,ext) $ case picture of
      InsideRect padding filling clr (p, e) ->
        let e'    = enlargeExtAbs padding padding e
            rect  = drawExt filling e'
            rect' = maybe id color clr rect
        in  pictures [rect', p]
      _ -> wrap pic
     where
        pic = fst <$> picture
        ext = extAlg $
              snd <$> picture

fromPicture :: G.Picture -> PictureL
fromPicture = ana coalg
  where
    coalg pic = case pic of
      G.Blank            -> Blank
      G.Polygon p        -> Line Fill   p
      G.Line p           -> Line NoFill p
      G.Circle a         -> Arc  Nothing      Nothing (0,0) a
      G.ThickCircle a b  -> Arc (Just a)      Nothing (0,0) b
      G.Arc a b c        -> Arc  Nothing (Just (a,b)) (0,0) c
      G.ThickArc a b c d -> Arc (Just c) (Just (a,b)) (0,0) d
      G.Text s           -> Text (0,0) s
      G.Bitmap{}         -> error "fromPicture: Bitmap isn't supported."
      G.Color c a        -> Color c a
      G.Translate a b c  -> Translate a b c
      G.Rotate{}         -> error "fromPicture: Rotate isn't supported."
      G.Scale a b c      -> Scale a b c
      G.Pictures p       -> Pictures p
