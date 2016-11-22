module Main where
--import Tycat
import qualified Model
import Box
import Point
import qualified Segment


main = do
  print(concatMap (Segment.svg viewport) [s1, s2]) where
    p1 = Point [0,0]
    p2 = Point [3,5]
    p3 = Point [2,0]
    s1 = Segment.Segment p1 p2
    s2 = Segment.Segment p1 p3
    bbox = fuseBoxes (Segment.box s1) (Segment.box s2)
    viewport = view [640, 480] bbox
  --model <- Model.load "../../test_files/cordoba.stl"
  --print(model)
--  let slices = Model.slice model 0.3 in
--      forM_ slices (\(Slice _ segments) -> tycat segments)
