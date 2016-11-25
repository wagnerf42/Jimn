module Main where
import qualified Jimn.Model as Model
import Jimn.Display
import Control.Monad

main = do
  model <- Model.load "../../test_files/cordoba.stl"
  print "starting slicing"
  let slices = Model.slice model 0.3 in
      forM_ slices (\(Model.Slice height segments) -> do
        print height
        tycat segments)
