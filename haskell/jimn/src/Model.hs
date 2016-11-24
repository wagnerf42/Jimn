module Model( Model(..)
            , load
            , pyramid
            , slice
            , sliceAt
            , Slice(..)
            ) where

import Control.Exception
import Debug.Trace
import Debug
import Point
import Box
import Segment
import Data.Maybe
import Data.List
import qualified Data.Set as Set

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import GHC.Float
import Control.Monad

-- | 3D Facet (3 3D points)
data Facet = Facet Point Point Point deriving(Show, Eq, Ord)
-- | A Model stores a 3D object as a list of Facets.
data Model = Model [Facet] deriving(Show)
-- | Slice of a 3D Model at a given height. Contains the height
-- and a list of Segments.
data Slice = Slice Double [Segment] deriving(Show)

-- io related functions

-- | Parses all Facets in a stl file bytestring (after header).
parseFacets :: Get [Facet]
parseFacets = do
  facetsNumber <- getInt32le
  forM [1..facetsNumber] (const parseFacet)

-- | Parses one Facet in a stl file bytestring.
parseFacet :: Get Facet
parseFacet = do
  normal <- sequence [getFloatle, getFloatle, getFloatle]
  c1 <- sequence [getFloatle, getFloatle, getFloatle]
  c2 <- sequence [getFloatle, getFloatle, getFloatle]
  c3 <- sequence [getFloatle, getFloatle, getFloatle]
  skip 2
  let coordinates = [c1, c2, c3]
      doubleCoordinates = map (map float2Double) coordinates
      [p1, p2, p3] = map Point doubleCoordinates in
      return $ Facet p1 p2 p3

-- | Loads given stl file as a Model.
load :: FilePath -> IO Model
load filename = do
  contents <- B.readFile filename
  return $ Model $ runGet parseFacets (B.drop 80 contents)

-- | Returns a pyramid Model for testing purposes.
pyramid :: Model
pyramid = Model facets where
  points = [Point [x, y, 0] | (x,y) <- [(-1,-1),(-1,1),(1,1),(1,-1)]]
  pairs = zip points $ tail $ cycle points
  top = Point [0, 0, 1]
  topFacets = map (\(p1, p2) -> Facet p1 p2 top) pairs
  bottomFacet1 = Facet (Point [1,1,0]) (Point [1,-1,0]) (Point [-1,-1,0])
  bottomFacet2 = Facet (Point [-1,-1,0]) (Point [1,1,0]) (Point [-1,1,0])
  facets = topFacets ++ [bottomFacet1, bottomFacet2]

-- facets related functions

-- | Returns minimal Box (3D) around given Facet.
facetBox :: Facet -> Box
facetBox (Facet (Point c1) (Point c2) (Point c3)) =
  Box (map minimum c) (map maximum c) where
  c = transpose [c1, c2, c3]

-- | Intersects given Facet with horizontal plane at given height.
-- Returns nothing if it is only a Point.
intersectFacet :: Double -> Facet -> Maybe Segment
intersectFacet height (Facet p1 p2 p3)
  | [i1,i2] <- intersections = Just $ Segment i1 i2
  | otherwise = Nothing where
  pointsPairs = [(p1,p2), (p1,p3), (p2,p3)]
  segments = map (uncurry Segment) pointsPairs
  intersections = mapMaybe (Segment.intersect height) segments

-- | Intersects a list of Facets at given height. Returns a Slice.
sliceAt :: [Facet] -> Double -> Maybe Slice
sliceAt facets height
  | null segments = Nothing
  | otherwise = Just $ Slice height segments where
    segments = mapMaybe (intersectFacet height) facets

-- model slicing related functions

-- | Events for fast Model slicing.
data Event = SlicingEvent Double
           | FacetStartEvent Double Facet
           | FacetEndEvent Double Facet
           deriving(Show)

facetEvents :: Facet -> [Event]
facetEvents facet = [FacetStartEvent minZ facet, FacetEndEvent maxZ facet] where
  Box [_,_,minZ] [_,_,maxZ] = facetBox facet

facetsEvents :: [Facet] -> [Event]
facetsEvents = concatMap facetEvents

sliceEvents :: [Facet] -> Double -> [Event]
sliceEvents facets sliceHeight =
  map SlicingEvent
  (assert (slicesNumber<1000) [minZ,minZ+sliceHeight..maxZ]) where
  boxes = map facetBox facets
  bounding_box = foldl1' fuseBoxes boxes
  Box [_,_,minZ] [_,_,maxZ] = bounding_box
  slicesNumber = (maxZ-minZ)/sliceHeight

eventKey :: Event -> (Double, Int)
eventKey (FacetStartEvent d _) = (d, 0)
eventKey (SlicingEvent d) = (d, 1)
eventKey (FacetEndEvent d _) = (d, 2)

allEvents :: [Facet] -> Double -> [Event]
allEvents facets sliceHeight =
  sortOn eventKey events where
    events = facetsEvents facets ++ sliceEvents facets sliceHeight

handleEvent :: (Set.Set Facet, [Slice]) -> Event -> (Set.Set Facet, [Slice])
handleEvent (currentFacets, slices) (FacetStartEvent _ facet) =
  (Set.insert facet currentFacets, slices)
handleEvent (currentFacets, slices) (FacetEndEvent _ facet) =
  (Set.delete facet currentFacets, slices)
handleEvent (currentFacets, slices) (SlicingEvent height) =
  (currentFacets,
    slices ++ maybeToList (sliceAt (Set.toList currentFacets) height))

-- | Slices Model into Slices with slices of height of the given value.
-- We use here a sweeping line algorithm where only active facets are
-- intersected at a given height.
slice :: Model -> Double -> [Slice]
slice (Model facets) sliceHeight = slices where
  events = allEvents facets sliceHeight
  (_, slices) = foldl' handleEvent (Set.empty, []) events
