module Graphics.Gloss.Data.Event
 ( Event(..)
 ) where

data Event = Event | InverseEvent | Drag (Float,Float)
  deriving (Eq, Show)
