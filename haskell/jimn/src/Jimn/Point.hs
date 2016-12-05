{-|
Module      : Jimn.Point
Description : Points
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides the Point type.
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Jimn.Point( Point(..)
            , norm
            , distanceBetween
            , minus
            , plus
            , times
            , Jimn.Point.box
            ) where

import System.Random
import Jimn.Box
import Jimn.Display
import GHC.Generics (Generic)
import Control.DeepSeq


-- | Point type storing points in any dimensions
newtype Point = Point [Double] deriving (Show, Eq, Ord, Generic, NFData)

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
  box = Jimn.Point.box

-- randomly generate 2d points
instance Random Point where
  random g = (Point [x, y], g2) where
    (x, g1) = random g
    (y, g2) = random g1

  randomR (Point[x1, y1], Point[x2,y2]) g = (Point [x, y], g2) where
    (x, g1) = randomR (x1, x2) g
    (y, g2) = randomR (y1, y2) g
