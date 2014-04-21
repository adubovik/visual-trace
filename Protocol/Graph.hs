{-# language
   RecordWildCards
 #-}

module Protocol.Graph
 ( Image
 -- Abstract for Server
 , Command(..)
 , mkImage
 , action
 , drawAnn
 , draw
 , getAnnotation
 , evolution
 ) where

import Data.Graph.Dynamic.Annotated
import Data.Graph.Layout

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.PictureF
import Graphics.Gloss.Data.PictureF.Selection
import Graphics.Gloss.Data.PictureF.Trans

data Command = InsertEdge Key Key
             | InsertNode Key Point
  deriving (Show, Read, Eq, Ord)

type Key = Int
type Graph = Graph2D () () Key

data Image = Image
  { graph2d :: Graph
  }
  deriving (Show, Read, Eq, Ord)

onAnnGraph2d :: (Graph -> Graph) -> (Image -> Image)
onAnnGraph2d f im = im { graph2d = f (graph2d im) }

mkImage :: Image
mkImage = Image { graph2d = empty }

action :: Command -> Image -> Image
action (InsertEdge fr to) = onAnnGraph2d $ insertEdge ((fr,to),Nothing)
action (InsertNode node pos) =
  onAnnGraph2d $ insertNode (node, Just ((), pos))

drawAnn :: Image -> Picture
drawAnn Image{..} = pictures $ edgePics ++ nodePics
  where
    edgePics :: [Picture]
    edgePics = map drawEdge $ Set.toList edges
      where
        findPos node = fromMaybe err $ Map.lookup node nodePoss
          where
            err = error $ "drawAnn: Can't find position of " ++ show node

        drawEdge (fr,to) = color G.green $
                           line [ findPos fr
                                , findPos to
                                ]

    edges :: Set.Set (Edge Key)
    edges = getEdges graph2d

    nodePoss :: Map.Map (Node Key) Point
    nodePoss = Map.map snd $ getNodeAnnotations graph2d

    nodePics :: [Picture]
    nodePics = map drawNode $ Map.toList nodePoss
      where
        drawNode (node, pos) = color G.red $
                               uncurry translate pos $
                               annotate (show node) $
                               group node $
                               pictures [ translate 0 20.0 (circle 5.0) ,
                               fixWidth 20 $
                               pictures $
                               [ circle 20.0
                               , circle 10.0
                               ]
                               ]

evolution :: Float -> Image -> Image
evolution _secElapsed = onAnnGraph2d $ fst . applyForces stdForces

-- Common logic for all protocols

getAnnotation :: (Float, Float) -> Image -> Maybe String
getAnnotation mousePos = annotationUnderPoint mousePos . drawAnn

draw :: ViewPort -> Image -> G.Picture
draw vp = toPicture vp . drawAnn
