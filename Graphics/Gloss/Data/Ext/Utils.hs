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

drawExt :: Ext -> Picture
drawExt (Ext Nothing) = blank
drawExt (Ext (Just (ExtentF yM ym xM xm))) =
  line [ (xM, yM)
       , (xM, ym)
       , (xm, ym)
       , (xm, yM)
       , (xM, yM)
       ]

drawExt2 :: Ext2 -> Picture
drawExt2 Ext2{..} = case height of
  Nothing -> drawExt weakExt
  Just h -> pictures [ drawExt weakExt
                     , fixHeight h $
                       drawExt strongExt
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
    alg (Annotate _ p)      = p
    alg _                   = mempty

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
                                   in  scaleExt w h unitExt
getAtomExt (Bitmap w h _ _)      = scaleExt
                                      (fromIntegral w)
                                      (fromIntegral h)
                                      unitExt
getAtomExt _                     = mempty
