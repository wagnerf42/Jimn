module Segment( Segment(..)
              , Segment.box
              , Segment.intersect
              ) where
import Point
import Data.List

data Segment = Segment Point Point deriving(Show)
box :: Segment -> Box
box (Segment (Point c1) (Point c2)) = Box (map minimum c) (map maximum c) where
  c = transpose [c1, c2]

intersect :: Double -> Segment -> Maybe Point
intersect height (Segment p1 p2)
  | divisor == 0 = Nothing
  | otherwise = Just (Point (take 2 intersectingCoordinates)) where
    diff = minus p2 p1
    Point [_,_,divisor] = diff
    Point [_,_,z1] = p1
    factor = (height - z1) / divisor
    Point intersectingCoordinates = plus p1 (times diff factor)
