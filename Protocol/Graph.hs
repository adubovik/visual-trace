{-# language
   RecordWildCards
 , DeriveDataTypeable
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
import Data.Typeable
import Text.Printf

import Graphics.Gloss.Data.Event
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
  , nodeUnderMouse :: Maybe (Node Key)
  }
  deriving (Show, Read, Eq, Ord, Typeable)

onGraph :: (Graph -> Graph) -> (Image -> Image)
onGraph f im = im { graph2d = f (graph2d im) }

mkImage :: Image
mkImage = Image { graph2d = empty
                , nodeUnderMouse = Nothing
                }

action :: Command -> Image -> Image
action (InsertEdge fr to) = onGraph $ insertEdge ((fr,to),Nothing)
action (InsertNode node pos) =
  onGraph $ insertNode (node, Just ((), pos))

drawAnn :: Image -> Picture
drawAnn Image{..} = pictures $ edgePics ++ nodePics
  where
    edgePics :: [Picture]
    edgePics = map drawEdge $ Set.toList edges
      where
        findPos node = fromMaybe err $ Map.lookup node nodePoss
          where
            err = error $ "drawAnn: Can't find position of " ++ show node

        drawEdge (fr,to) = color G.white $
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
        nodeColor node | Just node' <- nodeUnderMouse
                       , node == node'
                       = G.red
                       | otherwise = G.green

        drawNode (node, pos) = color (nodeColor node) $
                               uncurry translate pos $
                               annotate ("Annotation\n" ++ show node) $
                               selectionTrigger (ExWrap $ nodeFeedback (node,pos)) $
                               group node $
                               pictures [
                                 translate 0 20.0 (circle 5.0) ,
                                 fixWidth 20 $
                                 pictures $
                                 [ circle 20.0
                                 , circle 10.0
                                 ]
                               ]

        nodeFeedback :: (Node Key, Point) -> Feedback Image
        nodeFeedback (node, pos) = Feedback
          { fbSideEffect = \event _image -> putStrLn $ printf "Event %s on %s " (show event) (show (node,pos))
          , fbTransform  = transform
          , fbId         = show node
          }
          where
            transform Event        image = image { nodeUnderMouse = Just node }
            transform InverseEvent image = image { nodeUnderMouse = Nothing }
            transform (Drag newPos)image = onGraph (adjustNodePos (const newPos) node) image

evolution :: Float -> Image -> Image
evolution _secElapsed = onGraph $ fst . applyForces stdForces

-- Common logic for all protocols

getAnnotation :: ViewPort -> (Float, Float) -> Image -> Maybe String
getAnnotation viewPort mousePos = annotationUnderPoint viewPort mousePos . drawAnn

draw :: ViewPort -> Image -> G.Picture
draw vp = toPicture vp . drawAnn
