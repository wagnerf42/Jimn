module Facet( Facet(..)
            , Facet.intersect
            , Slice(..)
            , slice
            ) where

import Point
import Segment
import Data.Maybe
import Data.List

data Facet = Facet Point Point Point deriving(Show, Eq, Ord)

intersect :: Double -> Facet -> Maybe Segment
intersect height (Facet p1 p2 p3)
  | [i1,i2] <- intersections = Just $ Segment i1 i2
  | otherwise = Nothing where
  pointsPairs = [(p1,p2), (p1,p3), (p2,p3)]
  segments = map (uncurry Segment) pointsPairs
  intersections = mapMaybe (Segment.intersect height) segments

box :: Facet -> Box
box (Facet (Point c1) (Point c2) (Point c3)) = Box (map minimum c) (map maximum c) where
  c = transpose [c1, c2, c3]

data Slice = Slice Double [Segment] deriving(Show)

sliceAt :: [Facet] -> Double -> Maybe Slice
--sliceAt facets height | trace ("sliceAt " ++ show facets ++ " " ++ show height) False = undefined
sliceAt facets height
  | null segments = Nothing
  | otherwise = Just $ Slice height segments where
    segments = mapMaybe (Facet.intersect height) facets

slice :: [Facet] -> Double -> [Slice]
slice facets sliceHeight = mapMaybe (sliceAt facets) [minZ,minZ+sliceHeight..maxZ] where
  boxes = map Facet.box facets
  bounding_box = foldl1' fuseBoxes boxes
  Box [_,_,minZ] [_,_,maxZ] = bounding_box
