{-|
Module      : Point
Description : Points
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides the Point type.
-}
module Point( Point(..)
            , norm
            , distanceBetween
            , minus
            , plus
            , times
            , Point.box
            ) where

import Box
import Display

-- | Point type storing points in any dimensions
data Point = Point [Double] deriving (Show, Eq, Ord)

-- points functions
-- | Returns norm of given point.
norm :: Point -> Double
norm (Point c) = sqrt $ sum $ map (^2) c

-- | Euclidean distance between two Points.
distanceBetween :: Point -> Point -> Double
distanceBetween p1 p2 = norm $ minus p2 p1

-- | Takes point1 point2 and returns point1 - point2
minus :: Point -> Point -> Point
minus (Point c1) (Point c2) = Point $ zipWith (-) c1 c2

-- | Takes point1 point2 and returns point1 + point2
plus :: Point -> Point -> Point
plus (Point c1) (Point c2) = Point $ zipWith (+) c1 c2

-- | Takes Point p and a factor f and scales coordinates of p by f.
times :: Point -> Double -> Point
times (Point c) scalar = Point $ map (*scalar) c

-- box functions
-- | Returns the smallest Box aroung given Point.
box :: Point -> Box
box (Point c) = Box c c

-- | Takes a Point and generates the
-- corresponding String for displaying it into an svg file.
instance DisplaySVG Point where
  svg (Point coordinates) = "<circle"++pos++" r=\"0.1\"/>\n" where
    pos = labelJoin ["cx", "cy"] coordinates
  box = Point.box
