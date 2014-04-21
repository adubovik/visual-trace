-- Somewhat similar to Graphics.Gloss.Data.ViewPort, but with
-- non-uniform scale.

{-# language
   RecordWildCards
 #-}

module Graphics.Gloss.Data.Matrix
 ( Matrix(..)
 , identityScale
 , identityTranslate
 , applyMatrix
 , applyMatrixToExt
 , invertMatrix
 , viewPortToMatrix
 , zeroScale
 , zeroTranslate
 ) where

import Graphics.Gloss.Data.Point(Point)
import Graphics.Gloss.Data.Ext
import Graphics.Gloss.Data.ViewPort

import Data.Monoid

-- Yeah, not really a matrix.
data Matrix = Matrix
  { mTranslate :: (Float, Float) -- applied 1st
  , mScale     :: (Float, Float) -- applied 2nd
  -- TODO: mRotate :: Float
  }
  deriving (Show)

instance Monoid Matrix where
  mempty = identityMatrix
  mappend = composeMatrices

viewPortToMatrix :: ViewPort -> Matrix
viewPortToMatrix ViewPort{..}
  | viewPortRotate /= 0.0 = error "viewPortToMatrix: rotation isn't yet supported."
  | otherwise = Matrix { mTranslate = viewPortTranslate
                       , mScale     = (viewPortScale, viewPortScale)
                       }

identityMatrix :: Matrix
identityMatrix = Matrix { mTranslate = (0,0)
                        , mScale     = (1,1)
                        }

identityScale :: (Float, Float) -> Matrix
identityScale s = Matrix { mTranslate = (0,0)
                         , mScale     = s
                         }

identityTranslate :: (Float, Float) -> Matrix
identityTranslate t = Matrix { mTranslate = t
                             , mScale     = (1,1)
                             }

invertScale :: (Float,Float) -> (Float, Float)
invertScale (x,y) = (1/x,1/y)

invertTranslate :: (Float,Float) -> (Float, Float)
invertTranslate (x,y) = (-x,-y)

zeroScale :: Matrix -> Matrix
zeroScale ext = ext { mScale = mScale identityMatrix }

zeroTranslate :: Matrix -> Matrix
zeroTranslate ext = ext { mTranslate = mTranslate identityMatrix }

-- Associative operator
composeMatrices :: Matrix -> Matrix -> Matrix
composeMatrices m1 m2 = Matrix { mTranslate = mTranslate m2 +
                                              (invertScale $ mScale m2) *
                                              mTranslate m1
                               , mScale = mScale m1 * mScale m2
                               }

applyMatrix :: Matrix -> Point -> Point
applyMatrix m = ((mScale m) *) . ((mTranslate m) + )

applyMatrixToExt :: Matrix -> Ext -> Ext
applyMatrixToExt m = (uncurry scaleExt (mScale m)) .
                     (uncurry translateExt (mTranslate m))

-- invertMatrix m <> m == mempty
-- invertMatrix . invertMatrix == id
invertMatrix :: Matrix -> Matrix
invertMatrix m = identityTranslate (invertTranslate $ mTranslate m) <>
                 identityScale     (invertScale     $ mScale     m) <>
                 mempty
