{-# language
   ViewPatterns
 , RecordWildCards
  #-}

module Graphics.Gloss.Data.Ext.Utils (
   getPictureExt
 , getPictureExt2
 , getAtomExt
 , ext2Alg
 , drawExt2
 , drawExt
 ) where

import Data.Monoid

import Data.Fix
import Graphics.Gloss.Data.Ext
import Graphics.Gloss.Data.Ext2
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Text(textWidth, textHeight)

drawExt :: Filling -> Ext -> Picture
drawExt _ (Ext Nothing) = blank
drawExt filling (Ext (Just (ExtentF yM ym xM xm))) =
  let prim = case filling of
        Fill -> polygon
        NoFill -> line
  in
  prim [ (xM, yM)
       , (xM, ym)
       , (xm, ym)
       , (xm, yM)
       , (xM, yM)
       ]

drawExt2 :: Filling -> Ext2 -> Picture
drawExt2 filling Ext2{..} = case height of
  Nothing -> drawExt filling weakExt
  Just h -> pictures [ drawExt filling weakExt
                     , fixHeight h $
                       drawExt filling strongExt
                     ]
  where
    height = do
      (_,(_,h)) <- getExt strongExt
      return $ 2*h

getPictureExt :: Picture -> Ext
getPictureExt = flattenExt2 . getPictureExt2

getPictureExt2 :: Picture -> Ext2
getPictureExt2 = cata ext2Alg

ext2Alg :: PictureF Ext2 -> Ext2
ext2Alg pic = mkWeakExt (getAtomExt pic) <> alg pic
  where
    alg (Translate x y p)   = translateExt2 x y p
    alg (Rotate _ _)        = error "getPictureExt: Rotate isn't yet supported"
    alg (Scale x y p)       = scaleExt2 x y p
    alg (Pictures ps)       = mconcat ps
    alg (FixedSize mx my p) = fixSizeExt2 mx my p
    alg (Color _ p)         = p
    alg (Group _ p)         = p
    alg (SelectionTrigger _ p) = p
    -- Assume no FixedSize primitive in VCat/HCat elements
    alg (VCat padding ps)   = mkWeakExt $ foldl1 (catFolder False padding) $ map weakExt ps
    alg (HCat padding ps)   = mkWeakExt $ foldl1 (catFolder  True padding) $ map weakExt ps
    alg (InsideRect _ padding _ p) = onWeakExt (enlargeExtAbs padding padding) p
    alg _                   = mempty

    catFolder isHCat padding acc ext
      | Just ((cx ,cy ),(ex ,ey )) <- getExt acc
      , Just ((cx',cy'),(ex',ey')) <- getExt ext
      , let realMinX   = cx' - ex'
      , let realMinY   = cy' - ey'
      , let targetMinX = cx + ex + padding
      , let targetMinY = cy + ey + padding
      = acc <> case isHCat of
          True  -> translateExt (targetMinX - realMinX) 0.0 ext
          False -> translateExt 0.0 (targetMinY - realMinY) ext
      | otherwise = ext

getAtomExt :: PictureF a -> Ext
getAtomExt Blank                 = mempty
getAtomExt (Polygon path)        = mconcat $ map pointExt path
getAtomExt (Line    path)        = mconcat $ map pointExt path
getAtomExt (Circle   rad)        = scaleExt rad rad unitExt
getAtomExt (Arc _ _ rad)         = scaleExt rad rad unitExt
getAtomExt (ThickCircle th rad)  = scaleExt (th+rad) (th+rad) unitExt
getAtomExt (ThickArc _ _ rad th) = scaleExt (th+rad) (th+rad) unitExt
getAtomExt (Text str)            = let h = textHeight str
                                       w = textWidth str
                                   in  scaleExt w h unitExtQ1
getAtomExt (Bitmap w h _ _)      = scaleExt
                                      (fromIntegral w)
                                      (fromIntegral h)
                                      unitExtQ1
getAtomExt _                     = mempty
