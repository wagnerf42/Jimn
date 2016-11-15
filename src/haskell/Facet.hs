module Facet( Facet(..)
            , Facet.intersect
            ) where

import Point
import Segment
import Data.Maybe
data Facet = Facet Point Point Point deriving(Show)

intersect :: Double -> Facet -> Maybe Segment
intersect height (Facet p1 p2 p3)
  | [i1,i2] <- intersections = Just $ Segment i1 i2
  | otherwise = Nothing where
  pointsPairs = [(p1,p2), (p1,p3), (p2,p3)]
  segments = map (uncurry Segment) pointsPairs
  intersections = mapMaybe (Segment.intersect height) segments
