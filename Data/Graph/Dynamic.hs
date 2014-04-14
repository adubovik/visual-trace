{-# language
   RecordWildCards
 #-}

module Data.Graph.Dynamic
 ( Edge
 , Node
 , Graph
 , empty
 , insertNode
 , deleteNode
 , getEdges
 , insertEdge
 , insertEdges
 , deleteEdge
 , deleteEdges
 , getChilds
 , getParents
 ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid
import Data.List(foldl')
import Data.Maybe(fromMaybe)

data Graph key = Graph { forwEdges :: Map.Map key (Set.Set key)
                       , backEdges :: Map.Map key (Set.Set key)
                       }
  deriving (Show, Read, Eq, Ord)

type Edge key = (key, key)
type Node key = key

empty :: Graph key
empty = Graph { forwEdges = Map.empty
              , backEdges = Map.empty
              }

-- FIXME: probably run `deleteNode` before the insertion...
insertNode :: Ord key => Node key -> Graph key -> Graph key
insertNode node Graph{..} =
  Graph { forwEdges = Map.insert node mempty forwEdges
        , backEdges = Map.insert node mempty backEdges
        }

deleteNode :: Ord key => Node key -> Graph key -> Graph key
deleteNode node Graph{..} =
  Graph { forwEdges = Map.map (Set.delete node)
                    . Map.delete node
                    $ forwEdges
        , backEdges = Map.map (Set.delete node)
                    . Map.delete node
                    $ backEdges
        }

getEdges :: Ord key => Graph key -> Set.Set (Edge key)
getEdges g = Set.fromList $
  [ (fr, to)
  | (fr, tos) <- Map.toList (forwEdges g)
  , to <- Set.toList tos
  ]

insertEdge :: Ord key => Edge key -> Graph key -> Graph key
insertEdge (fr,to) Graph{..} =
  Graph { forwEdges = Map.adjust (Set.insert to) fr forwEdges
        , backEdges = Map.adjust (Set.insert fr) to backEdges
        }

insertEdges :: Ord key => [Edge key] -> Graph key -> Graph key
insertEdges edges graph = foldl' (flip insertEdge) graph edges

deleteEdge :: Ord key => Edge key -> Graph key -> Graph key
deleteEdge (fr,to) Graph{..} =
  Graph { forwEdges = Map.adjust (Set.delete to) fr forwEdges
        , backEdges = Map.adjust (Set.delete fr) to backEdges
        }

deleteEdges :: Ord key => [Edge key] -> Graph key -> Graph key
deleteEdges edges graph = foldl' (flip deleteEdge) graph edges

getChilds :: Ord key => Node key -> Graph key -> Set.Set key
getChilds key graph = fromMaybe mempty $
  Map.lookup key (forwEdges graph)

getParents :: Ord key => Node key -> Graph key -> Set.Set key
getParents key graph = fromMaybe mempty $
  Map.lookup key (backEdges graph)
