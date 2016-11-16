module Main where
import Point
import Segment
import Facet
import qualified Model


main = do
  model <- Model.load "../../test_files/cordoba.stl"
  print(model)
