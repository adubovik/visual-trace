{-# language
   RecordWildCards
 #-}

module Data.Graph.Dynamic.Annotated
 (
 -- * Types
   Edge
 , Node
 , AnnGraph
 -- * Node annotations manipulation
 , getNodeAnnotations
 , adjustNodeAnn
 -- * Usual graph handling
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

import Data.Graph.Dynamic(Graph,Edge,Node)
import qualified Data.Graph.Dynamic as D
import Data.Monoid
import Data.List(foldl')
import Data.Maybe(fromMaybe)

import qualified Data.Map as Map
import qualified Data.Set as Set

data AnnGraph na ea key = AnnGraph { graph :: Graph key
                                   , nodeAnns :: Map.Map (Node key) na
                                   , edgeAnns :: Map.Map (Edge key) ea
                                   }
  deriving (Show, Read, Eq, Ord)

getNodeAnnotations :: AnnGraph na ea key -> Map.Map (Node key) na
getNodeAnnotations = nodeAnns

empty :: AnnGraph na ea key
empty = AnnGraph { graph = D.empty
                 , nodeAnns = Map.empty
                 , edgeAnns = Map.empty
                 }

adjustNodeAnn :: Ord key => (key -> na -> na) ->
                 AnnGraph na ea key -> AnnGraph na ea key
adjustNodeAnn adjf g = g { nodeAnns = Map.mapWithKey adjf (nodeAnns g) }

insertNode :: Ord key => (Node key, Maybe na) ->
              AnnGraph na ea key -> AnnGraph na ea key
insertNode (node, nodeAnn) AnnGraph{..} =
  AnnGraph { graph = D.insertNode node graph
           , edgeAnns = edgeAnns
           , nodeAnns =
               case nodeAnn of
                 Nothing  -> nodeAnns
                 Just ann -> Map.insert node ann nodeAnns
           }

deleteNode :: Ord key => Node key ->
              AnnGraph na ea key -> AnnGraph na ea key
deleteNode node AnnGraph{..} =
  AnnGraph { graph = D.deleteNode node graph
           , edgeAnns = edgeAnns
           , nodeAnns = Map.delete node nodeAnns
           }

getEdges :: Ord key => AnnGraph na ea key -> Set.Set (Edge key)
getEdges = D.getEdges . graph

insertEdge :: Ord key => (Edge key, Maybe ea) ->
              AnnGraph na ea key -> AnnGraph na ea key
insertEdge (edge, edgeAnn) AnnGraph{..} =
  AnnGraph { graph = D.insertEdge edge graph
           , edgeAnns =
               case edgeAnn of
                 Nothing  -> edgeAnns
                 Just ann -> Map.insert edge ann edgeAnns
           , nodeAnns = nodeAnns
           }

insertEdges :: Ord key => [(Edge key, Maybe ea)] ->
               AnnGraph na ea key -> AnnGraph na ea key
insertEdges edges graph = foldl' (flip insertEdge) graph edges

deleteEdge :: Ord key => Edge key ->
              AnnGraph na ea key -> AnnGraph na ea key
deleteEdge edge AnnGraph{..} =
  AnnGraph { graph = D.deleteEdge edge graph
           , edgeAnns = Map.delete edge edgeAnns
           , nodeAnns = nodeAnns
           }

deleteEdges :: Ord key => [Edge key] ->
               AnnGraph na ea key -> AnnGraph na ea key
deleteEdges edges graph = foldl' (flip deleteEdge) graph edges

getChilds :: Ord key => Node key -> AnnGraph na ea key -> Set.Set key
getChilds key = D.getChilds key . graph

getParents :: Ord key => Node key -> AnnGraph na ea key -> Set.Set key
getParents key = D.getParents key . graph
