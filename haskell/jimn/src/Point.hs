{-|
Module      : Point
Description : Points, Boxes, Svg related functions
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides the most basic types in Jimn.
-}
module Point( Point(..)
            , norm
            , distanceBetween
            , minus
            , plus
            , times
            , Box(..)
            , box
            , fuseBoxes
            , ViewPort
            , view
            , svgCoordinates
            , svg
            , labelJoin
            ) where

-- | Point type storing points in any dimensions
data Point = Point [Double] deriving (Show, Eq, Ord)
-- | A Box allows to delimit a region of space between minimal and
-- maximal coordinates.
data Box = Box [Double] [Double] deriving (Show)

-- | A ViewPort is used for svg displays. it encodes coordinates
-- transformations. Any point is first translated by substracting first vector,
-- then sees all its coordinates scaled by given factor and the translated
-- again by adding last vector.
data ViewPort = ViewPort [Double] Double [Double] deriving (Show)

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

-- | Applies Viewport coordinates transformations to given Point.
-- Returns a coordinates list.
svgCoordinates :: ViewPort -> Point -> [Double]
svgCoordinates (ViewPort minc scale translation) (Point c) = new_coordinates where
  translatedCoordinates = zipWith (-) c minc
  scaledCoordinates = map (*scale) translatedCoordinates
  new_coordinates = zipWith (+) scaledCoordinates translation

-- | Helper function to easily generate svg properties.
-- we take some properties lables, some variables holding properties content
-- and build a usable svg string holding all properties, correctly labeled.
labelJoin :: (Show a) => [String] -> [a] -> String
labelJoin strings things = concat $ zipWith together strings things where
  together s t = " "++s++"=\""++show t++"\""

-- | Takes a ViewPort and a Point and generates the
-- corresponding String for displaying the Point into an svg file.
svg :: ViewPort -> Point -> String
svg v p = "<circle"++pos++"/>\n" where
  pos = labelJoin ["cx", "cy"] $ svgCoordinates v p
