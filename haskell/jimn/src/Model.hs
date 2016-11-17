module Model( Model(..)
            , load
            ) where

import Point
import Facet
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Debug.Trace
import GHC.Float
import Control.Monad

data Model = Model [Facet] deriving(Show)

parseFacets :: Get [Facet]
parseFacets = do
  facetsNumber <- getInt32le
  forM [1..facetsNumber] (const parseFacet)

parseFacet :: Get Facet
parseFacet = do
  normal <- sequence [getFloatle, getFloatle, getFloatle]
  c1 <- sequence [getFloatle, getFloatle, getFloatle]
  c2 <- sequence [getFloatle, getFloatle, getFloatle]
  c3 <- sequence [getFloatle, getFloatle, getFloatle]
  let coordinates = [c1, c2, c3]
      doubleCoordinates = map (map float2Double) coordinates
      [p1, p2, p3] = map Point doubleCoordinates in
      return $ Facet p1 p2 p3


load :: FilePath -> IO Model
load filename = do
  contents <- B.readFile filename
  return $ Model $ runGet parseFacets (B.drop 80 contents)
