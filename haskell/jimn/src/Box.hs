{-|
Module      : Box
Description : Boxes, svg related functions
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides a Box type and basic svg related types.
-}
module Box( Box(..)
            , fuseBoxes
            , ViewPort(..)
            , view
            , labelJoin
          ) where

-- | A Box allows to delimit a region of space between minimal and
-- maximal coordinates.
data Box = Box [Double] [Double] deriving (Show)

-- | A ViewPort is used for svg displays. it encodes coordinates
-- transformations. Any point is first translated by substracting first vector,
-- then sees all its coordinates scaled by given factor and the translated
-- again by adding last vector.
data ViewPort = ViewPort [Double] Double [Double] deriving (Show)

-- | Fuses two Boxes into smallest Box containing both inital ones.
fuseBoxes :: Box -> Box -> Box
fuseBoxes (Box minc1 maxc1) (Box minc2 maxc2) = Box minc maxc where
  minc = zipWith min minc1 minc2
  maxc = zipWith max maxc1 maxc2

-- svg functions
-- | Takes an image size (2 doubles), a box of things to display and
-- computes the viewport to apply to all things in order to fit into
-- the given size.
view :: [Double] -> Box -> ViewPort
view svgSize (Box minc maxc) = ViewPort minc scale translation where
  dimensions = zipWith (-) maxc minc
  ratios = zipWith (/) svgSize dimensions
  scale = minimum ratios
  scaledDimensions = map (*scale) dimensions
  translation = map (/2) $ zipWith (-) svgSize scaledDimensions

-- | Helper function to easily generate svg properties.
-- we take some properties lables, some variables holding properties content
-- and build a usable svg string holding all properties, correctly labeled.
labelJoin :: (Show a) => [String] -> [a] -> String
labelJoin strings things = concat $ zipWith together strings things where
  together s t = " "++s++"=\""++show t++"\""

class DisplayableInSvg a where
  svg :: ViewPort -> a -> String
  box :: a -> Box
