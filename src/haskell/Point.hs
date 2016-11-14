module Point( Point(..)
            , distanceBetween
            , norm
            , minus
            , plus
            , times
            , box
            , svg
            , Box(..)
            , ViewPort
            , svgCoordinates
            , view
            ) where

data Point = Point [Double] deriving (Show)
data Box = Box [Double] [Double] deriving (Show)

-- translate (before scale) then scale then translate (after scale)
data ViewPort = ViewPort [Double] Double [Double] deriving (Show)
view :: [Double] -> Box -> ViewPort
view svgSize (Box minc maxc) = ViewPort minc scale translation where
  dimensions = zipWith (-) maxc minc
  ratios = zipWith (/) svgSize dimensions
  scale = minimum ratios
  scaledDimensions = map (*scale) dimensions
  translation = map (/2) $ zipWith (-) svgSize scaledDimensions

svgCoordinates :: ViewPort -> Point -> [Double]
svgCoordinates (ViewPort minc scale translation) (Point c) = new_coordinates where
  translatedCoordinates = zipWith (-) c minc
  scaledCoordinates = map (*scale) translatedCoordinates
  new_coordinates = zipWith (+) scaledCoordinates translation

labelJoin :: (Show a) => [String] -> [a] -> String
labelJoin strings things = concat $ zipWith together strings things where
  together s t = " "++s++"=\""++show t++"\""

svg :: ViewPort -> String -> Point -> String
svg v color p = "<circle"++pos++" fill=\""++color++"\"/>" where
  pos = labelJoin ["cx", "cy"] $ svgCoordinates v p

norm :: Point -> Double
norm (Point c) = sqrt $ sum $ map (^2) c

distanceBetween :: Point -> Point -> Double
distanceBetween p1 p2 = norm $ minus p2 p1

minus :: Point -> Point -> Point
minus (Point c1) (Point c2) = Point $ zipWith (-) c2 c1

plus :: Point -> Point -> Point
plus (Point c1) (Point c2) = Point $ zipWith (+) c2 c1

times :: Point -> Double -> Point
times (Point c) scalar = Point $ map (*scalar) c

box :: Point -> Box
box (Point c) = Box c c
