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
                          , add
                          , insert
                          , Jimn.PointsRounder.lookup
                          , adjust
                          ) where

import Numeric
import Data.List (transpose, find)
import Data.Maybe
--import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as Map
import Jimn.Point
import Control.Monad.State
import Debug.Trace

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
                       --, maps :: [Map.Map String Point]
                       , maps :: [Map.HashMap String Point]
                       } deriving(Show)

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
lookup (Rounder precision maps) point = listToMaybe $ catMaybes values where
    keys = pointKey precision point
    values = zipWith Map.lookup keys maps

-- | Adds new Point in Rounder replacing nearby Points.
insert :: Rounder -> Point -> Rounder
insert (Rounder precision maps) point = Rounder precision newMaps where
  keys = pointKey precision point
  newMaps = zipWith3 Map.insert keys (repeat point) maps

-- | Adds given point to rounder. We first try to find a nearby point inside. If
-- there is none, we add ourselves and return ourselves. If there is one we
-- return it directly.
add :: Rounder -> Point -> (Rounder, Point)
add rounder point
  | Just existingPoint <- Jimn.PointsRounder.lookup rounder point =
    (rounder, existingPoint)
  | otherwise = (insert rounder point, point)

adjust :: Point -> State Rounder Point
adjust p = do
  rounder <- get
  let (newRounder, p2) = add rounder p in do
      put newRounder
      return p2
