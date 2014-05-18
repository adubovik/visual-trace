-- Graphics.Gloss.Data.Color has no Read instance...

module VisualTrace.Data.ColorRead
 ( Color
 , toColor
 , fromColor
 ) where

import qualified Graphics.Gloss.Data.Color as C

data Color
  = RGBA  !Float !Float !Float !Float
  deriving (Show, Read, Eq, Ord)

toColor :: Color -> C.Color
toColor (RGBA a b c d) = C.makeColor a b c d

fromColor :: C.Color -> Color
fromColor clr = let (a,b,c,d) = C.rgbaOfColor clr
                in  RGBA a b c d
