module Facet( Facet(..)
            , Facet.intersect
            , Slice(..)
            , slice
            ) where

import Point
import Segment
import Data.Maybe
import Data.List
import qualified Data.Set as Set

data Facet = Facet Point Point Point deriving(Show, Eq, Ord)

intersect :: Double -> Facet -> Maybe Segment
intersect height (Facet p1 p2 p3)
  | [i1,i2] <- intersections = Just $ Segment i1 i2
  | otherwise = Nothing where
  pointsPairs = [(p1,p2), (p1,p3), (p2,p3)]
  segments = map (uncurry Segment) pointsPairs
  intersections = mapMaybe (Segment.intersect height) segments

box :: Facet -> Box
box (Facet (Point c1) (Point c2) (Point c3)) =
  Box (map minimum c) (map maximum c) where
  c = transpose [c1, c2, c3]


data Slice = Slice Double [Segment] deriving(Show)

sliceAt :: [Facet] -> Double -> Maybe Slice
--sliceAt facets height | trace (show facets ++ show height) False = undefined
sliceAt facets height
  | null segments = Nothing
  | otherwise = Just $ Slice height segments where
    segments = mapMaybe (Facet.intersect height) facets

data Event = SlicingEvent Double
           | FacetStartEvent Double Facet
           | FacetEndEvent Double Facet
           deriving(Show)

facetEvents :: Facet -> [Event]
facetEvents facet = [FacetStartEvent minZ facet, FacetEndEvent maxZ facet] where
  Box [_,_,minZ] [_,_,maxZ] = Facet.box facet

facetsEvents :: [Facet] -> [Event]
facetsEvents = concatMap facetEvents

sliceEvents :: [Facet] -> Double -> [Event]
sliceEvents facets sliceHeight =
  map SlicingEvent [minZ,minZ+sliceHeight..maxZ] where
  boxes = map Facet.box facets
  bounding_box = foldl1' fuseBoxes boxes
  Box [_,_,minZ] [_,_,maxZ] = bounding_box

eventKey :: Event -> (Double, Int)
eventKey (FacetStartEvent d _) = (d, 0)
eventKey (SlicingEvent d) = (d, 1)
eventKey (FacetEndEvent d _) = (d, 2)

allEvents :: [Facet] -> Double -> [Event]
allEvents facets sliceHeight =
  sortOn eventKey events where
    events = facetsEvents facets ++ sliceEvents facets sliceHeight

slice :: [Facet] -> Double -> [Slice]
slice facets sliceHeight = slices where
  events = allEvents facets sliceHeight
  handleEvent (currentFacets, slices) (FacetStartEvent _ facet) =
    (Set.insert facet currentFacets, slices)
  handleEvent (currentFacets, slices) (FacetEndEvent _ facet) =
    (Set.delete facet currentFacets, slices)
  handleEvent (currentFacets, slices) (SlicingEvent height) =
    (currentFacets,
    slices ++ maybeToList (sliceAt (Set.toList currentFacets) height))
  (_, slices) = foldl' handleEvent (Set.empty, []) events
