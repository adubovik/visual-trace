{-# language
   RecordWildCards
 , DeriveDataTypeable
 , TypeFamilies
 , PatternGuards
 , DoAndIfThenElse
 #-}

module VisualTrace.Protocol.Graph
 ( Image()
 -- Abstract for Server
 , Command(..)
 ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)
import Data.Typeable

import Graphics.Gloss.Interface.Pure.Game(MouseButton(..))
import Graphics.Gloss.Data.Point
import qualified Graphics.Gloss as G

import VisualTrace.Data.Graph.Dynamic.Annotated
import VisualTrace.Data.Graph.Layout
import VisualTrace.Data.EventInfo.Utils
import VisualTrace.Data.EventInfo.StdLib
import VisualTrace.Data.PictureF
import VisualTrace.Data.Feedback
import qualified VisualTrace.Protocol.Image as I

data Command = InsertEdge Key Key
             | InsertNode Key Point
  deriving (Show, Read, Eq, Ord)

type Key = Int
type Graph = Graph2D () () Key

data Image = Image
  { graph2d :: Graph
  , nodeHighlighted :: Maybe (Node Key)
  , nodeAnnotation :: Maybe (Point, Node Key)
  }
  deriving (Show, Read, Eq, Ord, Typeable)

instance I.Image Image where
  type Command Image = Command
  initImage = mkImage
  drawImageG = drawAnn'
  evolveImage = evolution
  interpret = action

onGraph :: (Graph -> Graph) -> (Image -> Image)
onGraph f im = im { graph2d = f (graph2d im) }

mkImage :: Image
mkImage = Image { graph2d = empty
                , nodeHighlighted = Nothing
                , nodeAnnotation = Nothing
                }

action :: Command -> Image -> Image
action (InsertEdge fr to)    = onGraph $ insertEdge ((fr,to),Nothing)
action (InsertNode node pos) = onGraph $ insertNode (node, Just ((), pos))

drawAnn' :: Image -> PictureG
drawAnn' Image{..} =
  pictures $
    edgePics ++
    nodePics ++
    [annotationPic]
  where
    annotationPic = maybe blank annotationDraw nodeAnnotation
    annotationDraw (mousePos,node) =
      stdAnnotationDraw mousePos $ show node

    edgePics :: [PictureG]
    edgePics = map drawEdge $ Set.toList edges
      where
        findPos node = fromMaybe (findErr node) $ Map.lookup node nodePoss
        findErr node = error $ "drawAnn: Can't find position of " ++ show node

        drawEdge (fr,to) = color G.white $
                           toPictureG $
                           line [ findPos fr
                                , findPos to
                                ]

    edges :: Set.Set (Edge Key)
    edges = getEdges graph2d

    nodePoss :: Map.Map (Node Key) Point
    nodePoss = Map.map snd $ getNodeAnnotations graph2d

    nodePics :: [PictureG]
    nodePics = map drawNode $ Map.toList nodePoss
      where
        nodeColor node | Just node' <- nodeHighlighted
                       , node == node'
                       = G.red
                       | otherwise = G.green

        drawNode (node, pos) = color (nodeColor node) $
                               uncurry translate (toLocalPoint pos) $
                               selectionTrigger (nodeFeedback (node,pos)) $
                               pictures [
                                 toPictureG $
                                   translate 0 20.0 $
                                     circle (0,0) 5.0 ,
                                 scale' (screen 1.0) $
                                   toPictureG $
                                     pictures $
                                       [ circle (0,0) 10.0
                                       , circle (0,0) 5.0
                                       ]
                               ]

        nodeFeedback :: (Node Key, Point) -> Feedback Image
        nodeFeedback (node,_pos) =
          mkFeedback focusCapture feedbackId $
            mkCompFeedback (traceSideEffect feedbackId) transform
          where
            focusCapture = keepFocusedIf (mouseButtonDrag LeftButton)
            feedbackId   = show node

            transform =
              stdHighlightTransfrom  hoverOn      hoverOff     `andWhen`
              stdAnnotationTransform mkAnnotation rmAnnotation `andWhen`
              onMouseDrag LeftButton moveNode

            mkAnnotation _old newPos image =
              image { nodeAnnotation = Just (newPos, node) }
            rmAnnotation image = image { nodeAnnotation = Nothing }

            hoverOn  image = image { nodeHighlighted = Just node }
            hoverOff image = image { nodeHighlighted = Nothing   }

            moveNode _oldPos newPos =
              -- TODO: replace with "+ (newPos - oldPos)"
              onGraph (adjustNodePos (const newPos) node)

evolution :: Float -> Image -> Image
evolution _secElapsed = onGraph $ fst . applyForces stdForces
