module Model( Model(..)
            , load
            ) where

import Facet
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Debug.Trace

data Model = Model [Facet] deriving(Show)

parser :: Get [Facet]
parser = do
  a <- getFloatle
  return []

load :: FilePath -> IO Model
load filename = do
  contents <- B.readFile filename
  return $ Model $ runGet parser (B.drop 84 contents)
