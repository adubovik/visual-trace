{-# language
   ScopedTypeVariables
 , TupleSections
 #-}

module Data.Graph.Layout
 ( Graph2D
 , stdForces
 , applyForces
 , adjustNodePos
 ) where

import Graphics.Gloss.Data.Point
import Data.Graph.Dynamic.Annotated
import Data.Graph.Layout.Physics

import Control.Arrow
import qualified Data.Map as Map
import qualified Data.Set as Set

type Graph2D na ea key = AnnGraph (na, Point) ea key

stdForces :: [Force]
stdForces = [ logSpring (2,1)
            , quadRepel 50000
            ]

moveCoeff :: Float
moveCoeff = 0.3

_deltaEps :: Float
_deltaEps = 0.1

adjustNodePos :: (Ord key, Eq (Node key)) =>
                 (Point -> Point) -> Node key ->
                 Graph2D na ea key -> Graph2D na ea key
adjustNodePos changePos node = adjustNodeAnn adjPos
  where
    adjPos node' | node /= node' = id
                 | otherwise = second changePos

applyForces :: forall na ea key. (Ord key, Eq key, Eq ea, Eq na) =>
               [Force] ->
               Graph2D na ea key -> (Graph2D na ea key, Float)
applyForces forces g = (g', avgDelta)
  where
    nodePoss :: Map.Map (Node key) Point
    nodePoss = Map.map snd $ getNodeAnnotations g

    edges :: Set.Set (Edge key)
    edges = Set.fromList $ concat $
            [ [ (fr,to)
              , (to,fr)
              ]
            | (fr,to) <- Set.toList (getEdges g)
            ]

    _nodes :: Set.Set (Node key)
    _nodes = Set.fromList $ Map.keys nodePoss

    nodePoss' :: Map.Map (Node key) Point
    nodePoss' = Map.fromList $
      [ (fr, moveCoeff `mulVector` force + pos1)
      | (fr, pos1) <- Map.toList nodePoss
      , let force = sum $
                    [ forceVector
                    | (to, pos2) <- Map.toList nodePoss
                    , fr /= to
                    , let areConnected = Set.member (fr,to) edges
                    , let forceVector = sum $ map (\f -> f areConnected pos1 pos2) forces
                    ]
      ]

    nodeDeltas :: Map.Map (Node key) Float
    nodeDeltas = Map.map lengthVector $
                 Map.unionWith (-) nodePoss nodePoss'

    g' = adjustNodeAnn adjPos g
      where
        adjPos node (na,_oldPos) = (na,newPos)
          where
            newPos = maybe err id $ Map.lookup node nodePoss'
            err = error "applyForces: new position for node wasn't found."

    avgDelta = average $ Map.elems nodeDeltas
      where
        average ls = sum ls / (fromIntegral (length ls))
