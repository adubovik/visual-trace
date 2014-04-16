-- Somewhat similar to Graphics.Gloss.Data.ViewPort, but with
-- non-uniform scale.

module Graphics.Gloss.Data.Matrix
 ( Matrix
 , identityMatrix
 ) where

-- Yeah, not really a matrix.
data Matrix = Matrix
  { mTranslate :: (Float, Float)
  , mScale     :: (Float, Float)
  }

identityMatrix :: Matrix
identityMatrix = Matrix { mTranslate = (0,0)
                        , mScale     = (1,1)
                        }

