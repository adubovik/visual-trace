{-# language
   ViewPatterns
 , ScopedTypeVariables
 #-}

module VisualTrace.Grid
 ( drawGrid
 ) where

import Graphics.Gloss

toFloat :: Int -> Float
toFloat = fromIntegral

drawGrid :: (Int,Int) -> (Point -> Point) ->
            Picture
drawGrid windowSize fromScreenToLocal = grid
  where
    grid :: Picture
    grid =
      pictures
      [ color (greyN 0.2) $ mk1DTicks False (xMin,xMax,winX) (yMin,yMax,winY)
      , color (greyN 0.2) $ mk1DTicks True  (yMin,yMax,winY) (xMin,xMax,winX)
      ]

    (toFloat -> winX, toFloat -> winY) = windowSize
    (xMax,yMax) = fromScreenToLocal ( winX, winY)
    (xMin,yMin) = fromScreenToLocal (-winX,-winY)

    mk1DTicks :: Bool -> (Float,Float,Float) -> (Float,Float,Float) ->
                 Picture
    mk1DTicks rev (xm, xM, winXSize) (ym, yM, _winYSize) =
      pictures $
        fmap (\x -> line $ reversePath [(x,ym),(x,yM)]) $
          xTicks
      where
        minTickGapInPixs = 20.0
        maxTicksNumber::Int = round (winXSize / minTickGapInPixs)
        minTicksNumber      = maxTicksNumber `div` 10

        reversePath | rev = fmap (\(x,y) -> (y,x))
                    | otherwise = id

        factor = getFactor xm xM 1.0

        xM' = (toFloat $ ceiling (xM * factor))/factor
        xm' = (toFloat $ floor   (xm * factor))/factor

        xTicks = takeWhile (<= xM') $
                 fmap (\i -> xm' + i/factor) $
                 enumFrom (0.0::Float)

        getFactor vm vM f
          | round (vM - vm) > maxTicksNumber
          = getFactor (vm/10.0) (vM/10.0) (f/10.0)
          | minTicksNumber <= round (vM - vm) &&
            maxTicksNumber >= round (vM - vm)
          = f
          | otherwise
          = getFactor (vm*10.0) (vM*10.0) (f*10.0)
