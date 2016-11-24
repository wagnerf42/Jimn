{-|
Module      : Box
Description : Boxes, svg related functions
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides a Box type and basic svg related types.
-}
{-# LANGUAGE FlexibleInstances #-}
module Box( Box(..)
          , fuseBoxes
          , labelJoin
          , DisplaySVG(..)
          , svgString
          , tycat
          ) where

import Data.List
import System.Process

-- | A Box allows to delimit a region of space between minimal and
-- maximal coordinates.
data Box = Box [Double] [Double] deriving (Show)

-- | Creates a box containing nothing in space of given dimensions.
emptyBox :: Int -> Box
emptyBox dimensions = Box minc maxc where
  infinity = read "Infinity" :: Double
  minc = map (const infinity) [1..dimensions]
  maxc = map (const (-infinity)) [1..dimensions]

-- | Fuses two Boxes into smallest Box containing both inital ones.
fuseBoxes :: Box -> Box -> Box
fuseBoxes (Box minc1 maxc1) (Box minc2 maxc2) = Box minc maxc where
  minc = zipWith min minc1 minc2
  maxc = zipWith max maxc1 maxc2

-- | Helper function to easily generate svg properties.
-- we take some properties labels, some variables holding properties content
-- and build a usable svg string holding all properties, correctly labeled.
labelJoin :: (Show a) => [String] -> [a] -> String
labelJoin strings things = concat $ zipWith together strings things where
  together s t = " "++s++"=\""++show t++"\""

class DisplaySVG a where
  svg :: a -> String
  box :: a -> Box

instance DisplaySVG a => DisplaySVG [a] where
  svg = concatMap svg
  box objects = foldl1' fuseBoxes $ map box objects


-- some magic for a variadic function
-- see http://rosettacode.org/wiki/Variadic_function#Haskell

class SvgStringType t where
  process :: (Box, [String]) -> t

-- compute final string
instance SvgStringType [Char] where
  process (bbox, strings) = header ++ strokeGroup ++ groups ++ footer where
    colors = cycle ["red", "green", "blue", "purple"]
    groupsStart = map (\c -> "<g stroke=\"" ++ c ++ "\">\n") colors
    groupsContent = map (++"</g>\n") strings
    groups = concat $ zipWith (++) groupsStart groupsContent
    Box minc maxc = bbox
    viewBoxParameters = minc ++ zipWith (-) maxc minc
    viewBox = unwords $ map show viewBoxParameters
    header = "<svg width=\"800\" height=\"600\" viewBox=\"" ++ viewBox ++ "\">\n"
    strokeGroup = "<g stroke-width=\"0.1\">\n"
    footer = "</g></svg>"


-- accumulate fusing boxes and storing strings
instance (DisplaySVG a, SvgStringType r) => SvgStringType (a -> r) where
  process (ibox, istrings) a = process (bbox, strings) where
    bbox = fuseBoxes ibox $ box a
    strings = istrings ++ [svg a]

-- init
svgString :: (SvgStringType t) => t
svgString = process (emptyBox 2, [])

tycat :: String -> IO ()
tycat s = do
  writeFile "/tmp/test.svg" s
  callCommand "tycat /tmp/test.svg 2> /dev/null"
  return ()
