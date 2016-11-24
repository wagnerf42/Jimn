module Main where
--import Tycat
import qualified Model
import Box
import Point
import Segment


main = do
  tycat string where
    p1 = Point [0, 0]
    p2 = Point [1, 1]
    p3 = Point [2, 4]
    s = Segment p1 p3
    string = svgString p3 s [p1,p2]
  --model <- Model.load "../../test_files/cordoba.stl"
  --print(model)
--  let slices = Model.slice model 0.3 in
--      forM_ slices (\(Slice _ segments) -> tycat segments)
