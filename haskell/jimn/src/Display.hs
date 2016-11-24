{-|
Module      : Display
Description : all svg related functions. see tycat.
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides tycat variadic function taking as arguments
any object or list of object instanciating the DisplaySVG typeclass.
It displays all of them in different colors under e17 terminal's terminology.
-}
{-# LANGUAGE FlexibleInstances #-}
module Display( DisplaySVG(..)
          , labelJoin
          , tycat
          ) where

import System.Process
import System.IO
import Data.List
import Box

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

-- | Turns Box surrounding objects and all objects' svg Strings into
-- a full svg file String.
svgString :: (Box, [String]) -> String
svgString (bbox, strings) = header ++ strokeGroup ++ groups ++ footer where
  colors = cycle ["red", "green", "blue", "purple", "orange"]
  groupsStart = map (\c -> "<g stroke=\"" ++ c ++ "\">\n") colors
  groupsContent = map (++"</g>\n") strings
  groups = concat $ zipWith (++) groupsStart groupsContent
  Box minc maxc = bbox
  viewBoxParameters = minc ++ zipWith (-) maxc minc
  viewBox = unwords $ map show viewBoxParameters
  header = "<svg width=\"800\" height=\"600\" viewBox=\"" ++ viewBox ++ "\">\n"
  strokeGroup = "<g stroke-width=\"0.1\">\n"
  footer = "</g></svg>"

class TycatType t where
  process :: (Box, [String]) -> t

-- final step
instance TycatType (IO a) where
  process args = do
    (tmpFile, h) <- openTempFile "/tmp" "jimn.svg"
    hPutStr h $ svgString args
    hClose h
    callCommand $ "tycat " ++ tmpFile ++ " 2> /dev/null"
    return undefined

-- accumulate fusing boxes and storing strings
instance (DisplaySVG a, TycatType r) => TycatType (a -> r) where
  process (ibox, istrings) a = process (bbox, strings) where
    bbox = fuseBoxes ibox $ box a
    strings = istrings ++ [svg a]

-- init
tycat :: (TycatType t) => t
tycat = process (emptyBox 2, [])
