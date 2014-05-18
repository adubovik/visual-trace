module VisualTrace.VisualTrace.Data.Graph.Layout.Physics
 ( logSpring
 , quadRepel
 , distance
 , lengthVector
 , mulVector
 , Force
 , Vector
 ) where

import Graphics.Gloss.Data.Point

type Vector = Point
type Force = Bool -> Point -> Point -> Vector

distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

mulVector :: Float -> Vector -> Vector
mulVector m (x,y) = (m*x,m*y)

lengthVector :: Vector -> Float
lengthVector (x,y) = sqrt (x**2 + y**2)

normalize :: Vector -> Vector
normalize v = (1 / lengthVector v) `mulVector` v

forceViaDist :: (Float -> Float) -> (Point -> Point -> Vector)
forceViaDist f = \p1 p2 ->
  (f $ distance p1 p2) `mulVector` (normalize $ p2 - p1)

logSpring :: (Float, Float) -> Force
logSpring (c1,c2) = force
  where
    f d = c1 * log (d / c2)
    force _areConnected@True  = forceViaDist f
    force _areConnected@False = \_ _ -> (0,0)

quadRepel :: Float -> Force
quadRepel c = force
  where
    f d = let d' = max 10 d
          in - (c / d'**2)
    force _areConnected = forceViaDist f


