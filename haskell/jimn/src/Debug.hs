{-|
Module      : Debug
Description : debug related utilities
Copyright   : (c) frederic wagner
License     : GPL-3
Maintainer  : frederic.wagner@imag.fr
Stability   : experimental
Portability : POSIX

This modules provides debugging functions.
-}
module Debug(debug1
            , debug2
            ) where

import Debug.Trace

debug1 f x = let res = f x in
                 traceShow (x, res) res

debug2 f x y = let res = f x y in
                   traceShow (x, y, res) res
