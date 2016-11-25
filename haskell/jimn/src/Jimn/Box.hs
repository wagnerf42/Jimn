{-|
Module      : Jimn.Box
Description : Boxes
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides a Box type for delimiting subsets of space between
coordinates limits.
-}
module Jimn.Box( Box(..)
          , fuseBoxes
          , emptyBox
          ) where

-- | A Box allows to delimit a region of space between minimal and
-- maximal coordinates.
data Box = Box [Double] [Double] deriving (Show)

-- | Creates a box containing nothing in space of given dimensions.
emptyBox :: Int -> Box
emptyBox dimensions = Box minc maxc where
  infinity = read "Infinity" :: Double
  minc = map (const infinity) [1..dimensions]
  maxc = map (const (-infinity)) [1..dimensions]

-- | Fuses two Boxes into smallest Box containing both inital ones.
fuseBoxes :: Box -> Box -> Box
fuseBoxes (Box minc1 maxc1) (Box minc2 maxc2) = Box minc maxc where
  minc = zipWith min minc1 minc2
  maxc = zipWith max maxc1 maxc2
