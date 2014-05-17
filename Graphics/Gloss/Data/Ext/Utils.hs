{-# language
   ViewPatterns
 , RecordWildCards
  #-}

module Graphics.Gloss.Data.Ext.Utils (
   getPictureExt
 , getAtomExt
 , extAlg
 , drawExt
 ) where

import Data.Monoid

import Data.Fix
import Graphics.Gloss.Data.Ext
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Text(textWidth, textHeight)

drawExt :: Filling -> Ext -> PictureL
drawExt _ (Ext Nothing) = blank
drawExt filling (Ext (Just (ExtentF yM ym xM xm))) =
  let prim = case filling of
        Fill   -> polygon
        NoFill -> line
  in
  prim [ (xM, yM)
       , (xM, ym)
       , (xm, ym)
       , (xm, yM)
       , (xM, yM)
       ]

getPictureExt :: PictureL -> Ext
getPictureExt = cata extAlg

extAlg :: PictureFL Ext -> Ext
extAlg pic = getAtomExt pic <> alg pic
  where
    alg (Translate x y p)   = translateExt x y p
    alg (Scale x y p)       = scaleExt x y p
    alg (Pictures ps)       = mconcat ps
    alg (Color _ p)         = p
    alg (SelectionTrigger _ p) = p
    alg (InsideRect padding _ _ p) = enlargeExtAbs padding padding p
    alg (VCat padding ps)   = foldl1 (catFolder False padding) ps
    alg (HCat padding ps)   = foldl1 (catFolder  True padding) ps
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

getAtomExt :: PictureFL a -> Ext
getAtomExt Blank                   = mempty
getAtomExt (Line _ path          ) = mconcat $ map pointExt path
getAtomExt (Arc Nothing   _ _ rad) = scaleExt     rad      rad  unitExt
getAtomExt (Arc (Just th) _ _ rad) = scaleExt (th+rad) (th+rad) unitExt
getAtomExt (Text _ str           ) = let h = textHeight str
                                         w = textWidth  str
                                     in  scaleExt w h unitExtQ1
getAtomExt _                       = mempty
