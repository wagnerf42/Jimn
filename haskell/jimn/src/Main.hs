module Main where
--import Tycat
import qualified Model


main = do
  model <- Model.load "../../test_files/cordoba.stl"
  print(model)
--  let slices = Model.slice model 0.3 in
--      forM_ slices (\(Slice _ segments) -> tycat segments)
