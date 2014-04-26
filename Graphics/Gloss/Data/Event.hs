module Graphics.Gloss.Data.Event
 ( Event(..)
 ) where

data Event = Event | InverseEvent
  deriving (Eq, Show)
