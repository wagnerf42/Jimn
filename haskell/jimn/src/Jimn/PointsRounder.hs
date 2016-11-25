{-|
Module      : Jimn.PointsRounder
Description : Rounds point to nearby points very fast.
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides the Rounder type allowing to identify nearby points
in O(1).
-}
module Jimn.PointsRounder ( Rounder(..)
                       , empty
                       ) where

import Numeric
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Jimn.Point

-- | Rounder structure.
-- Rounder is a structure providing a very fast way (O(1)) of
-- merging nearby coordinates in points.
-- when initializing a new hash you need to provide the dimension of the space
-- and a wanted_precision.
--
-- we ensure that :
-- - if any two points have for all coordinates a difference less than
-- 0.5*10**-wanted_precision then they will be merged to the value of the
-- most ancient in the hash.
-- - if two points are merged then IN GENERAL their coordinates'
-- differences are less than 2*10**-wanted_precision (transitivity might
-- increase this value but it is very unlikely).

data Rounder = Rounder { precision :: Int
                       , maps :: [Map.HashMap String Point]
                       }

-- | Key for given float at given precision. (Round at precision decimals)
baseKey :: Int -> Double -> String
baseKey precision f = showFFloat (Just precision) f ":"

-- | Displaced key for given float at given precision.
-- (Round float+1/2*10^-precision at precision decimals).
displacedKey :: Int -> Double -> String
displacedKey precision f =
  showFFloat (Just precision) (f+0.5*10^^(-precision)) ":"

-- | A Point is hashed 2^dimension times. each coordinate is hashed
-- with a base/displaced key.
-- We take here a precision, a Point and return all possible keys.
pointKey :: Int -> Point -> [String]
pointKey precision (Point c) = keys where
  baseKeys = map (baseKey precision) c
  displacedKeys = map (displacedKey precision) c
  keys = map concat $ sequence $ transpose [baseKeys, displacedKeys]

-- | Creates a new Rounder from a given precision and space dimension.
empty :: Int -> Int -> Rounder
empty precision dimension =
  Rounder precision $ map (const Map.empty) [1..2^dimension]

-- | Looks up in the rounder if a nearby Point is already there.
lookup :: Rounder -> Point -> Maybe Point
lookup (Rounder precision maps) p
  | Just result  <- find isJust values  = result
  | otherwise = Nothing where
    keys = pointKey precision p
    values = zipWith Map.lookup keys maps

-- | Adds new Point in Rounder, eventually erasing nearby Points.
insert :: Rounder -> Point -> Rounder
insert (Rounder precision maps) p = where
  keys = pointKey precision p
  --TODO: map here ? how to reverse args ?

--wagnerf: for reference, the HashMap I'm talking      │ AdituV
--                   | about is from the unordered-containers package
