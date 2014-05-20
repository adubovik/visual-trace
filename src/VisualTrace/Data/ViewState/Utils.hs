module VisualTrace.Data.ViewState.Utils
 ( onViewPort
 , onViewPortScale
 , onViewPortTranslate
 ) where

import Graphics.Gloss.Data.ViewState

onViewPort :: (ViewPort -> ViewPort) -> (ViewState -> ViewState)
onViewPort f s = s { viewStateViewPort = f (viewStateViewPort s) }

onViewPortScale :: (Float -> Float) -> (ViewPort -> ViewPort)
onViewPortScale f p = p { viewPortScale = f (viewPortScale p) }

onViewPortTranslate :: ((Float,Float) -> (Float,Float)) -> (ViewPort -> ViewPort)
onViewPortTranslate f p = p { viewPortTranslate = f (viewPortTranslate p) }
