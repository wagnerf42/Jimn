{-|
Module      : Segment
Description : Segments
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides a Segment type and its related functions.
-}
module Segment( Segment(..)
              , Segment.box
              , Segment.intersect
              , Segment.svg
              ) where
import Point
import Data.List

-- | Oriented Segment between two Points.
data Segment = Segment Point Point deriving(Show)

-- | Returns the smallest Box around given Segment.
box :: Segment -> Box
box (Segment (Point c1) (Point c2)) = Box (map minimum c) (map maximum c) where
  c = transpose [c1, c2]

-- | Intersects given 3D Segment with an horizontal plane at given height.
intersect :: Double -> Segment -> Maybe Point
intersect height (Segment p1 p2)
  | divisor == 0 = Nothing
  | otherwise = Just (Point (take 2 intersectingCoordinates)) where
    diff = minus p2 p1
    Point [_,_,divisor] = diff
    Point [_,_,z1] = p1
    factor = (height - z1) / divisor
    Point intersectingCoordinates = plus p1 (times diff factor)

-- | Takes a ViewPort and a Segment and generates the
-- corresponding String for displaying the Segment into an svg file.
svg :: ViewPort -> Segment -> String
svg v (Segment p1 p2) = "<line"++pos++"/>\n" where
  pos = labelJoin ["x1", "y1", "x2", "y2"] $ concatMap (svgCoordinates v) [p1, p2]
