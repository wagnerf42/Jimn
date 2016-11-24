module Main where
import qualified Model
import Display
import Control.Monad

main = do
  model <- Model.load "../../test_files/cordoba.stl"
  let slices = Model.slice model 0.3 in
      forM_ slices (\(Model.Slice _ segments) -> tycat segments)
