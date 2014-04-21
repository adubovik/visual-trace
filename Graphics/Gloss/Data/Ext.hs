{-# language
   RecordWildCards
 #-}

module Graphics.Gloss.Data.Ext
 ( Ext
 , pointExt
 , unitExt
 , scaleExt
 , translateExt
 , enlargeExt
 , getExt
 , drawExt
 , pointInExt
 ) where

import Data.Monoid

import Graphics.Gloss

data ExtentF = ExtentF Float Float Float Float
  deriving (Show, Read, Eq, Ord)

makeExtentF
	:: Float -- ^ y max (north)
	-> Float -- ^ y min (south)
	-> Float -- ^ x max (east)
	-> Float -- ^ x min (west)
	-> ExtentF
makeExtentF = ExtentF

getExtentF :: ExtentF -> ((Float,Float), (Float,Float))
getExtentF (ExtentF a b c d) =
  let ex = (c - d)/2.0
      ey = (a - b)/2.0
      cx = (c + d)/2.0
      cy = (a + b)/2.0
  in ((cx,cy),(ex,ey))

pointExtentF :: (Float, Float) -> ExtentF
pointExtentF (x,y) = makeExtentF y y x x

scaleExtentF :: Float -> Float -> ExtentF -> ExtentF
scaleExtentF sx sy (ExtentF a b c d) = ExtentF (sy*a) (sy*b) (sx*c) (sx*d)

enlargeExtentF :: Float -> Float -> ExtentF -> ExtentF
enlargeExtentF sx sy ext =
  let ((cx,cy),(ex',ey')) = getExtentF ext
      ex = sx*ex'
      ey = sy*ey'
  in  makeExtentF (cy+ey) (cy-ey) (cx+ex) (cx-ex)

translateExtentF :: Float -> Float -> ExtentF -> ExtentF
translateExtentF tx ty (ExtentF a b c d) = ExtentF (ty+a) (ty+b) (tx+c) (tx+d)

mergeExtentFs :: ExtentF -> ExtentF -> ExtentF
mergeExtentFs (ExtentF a  b  c  d )
              (ExtentF a' b' c' d') =
  makeExtentF (max a a')
              (min b b')
              (max c c')
              (min d d')

newtype Ext = Ext { runExt :: Maybe ExtentF }
  deriving (Show, Read, Eq, Ord)

over :: (ExtentF -> ExtentF) -> Ext -> Ext
over f = Ext . fmap f . runExt

instance Monoid Ext where
  mempty = Ext Nothing
  mappend (Ext Nothing) y = y
  mappend x (Ext Nothing) = x
  mappend (Ext (Just x)) (Ext (Just y)) = Ext . Just $ x `mergeExtentFs` y

unitExt :: Ext
unitExt = Ext . Just $ makeExtentF 1 (-1) 1 (-1)

pointExt :: (Float, Float) -> Ext
pointExt = Ext . Just . pointExtentF

scaleExt :: Float -> Float -> Ext -> Ext
scaleExt x y = over (scaleExtentF x y)

translateExt :: Float -> Float -> Ext -> Ext
translateExt x y = over (translateExtentF x y)

drawExt :: Ext -> Picture
drawExt (Ext Nothing) = blank
drawExt (Ext (Just (ExtentF yM ym xM xm))) =
  Line [ (xM, yM)
       , (xM, ym)
       , (xm, ym)
       , (xm, yM)
       , (xM, yM)
       ]

enlargeExt :: Float -> Float -> Ext -> Ext
enlargeExt x y = over (enlargeExtentF x y)

getExt :: Ext -> Maybe ((Float,Float), (Float,Float))
getExt (Ext Nothing) = Nothing
getExt (Ext (Just ext)) = Just $ getExtentF ext

pointInExt :: Ext -> (Float,Float) -> Bool
pointInExt (Ext Nothing) _ = False
pointInExt (Ext (Just (ExtentF yM ym xM xm))) (x,y) =
  ym <= y && y <= yM && xm <= x && x <= xM
