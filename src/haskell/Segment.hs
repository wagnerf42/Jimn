module Segment( Segment(..)
              , Segment.box
              ) where
import Point
import Data.List

data Segment = Segment Point Point deriving(Show)
box :: Segment -> Box
box (Segment (Point c1) (Point c2)) = Box (map minimum c) (map maximum c) where
  c = transpose [c1, c2]
