{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time.Clock
import System.Random
import Jimn.Point
import Jimn.PointsRounder
import Control.Monad.State
import Control.DeepSeq

main = do
    gen <- getStdGen

    start <- getCurrentTime
    let !points = force $ take 100000 $ randoms (gen) :: [Point]
    end <- getCurrentTime
    putStrLn $ "created 10^5 points in" ++ show (diffUTCTime end start)

    start <- getCurrentTime
    let points = take 100000 $ randoms (gen) :: [Point]
        rounder = empty 5 2
        !hpoints = force $ evalState (mapM adjust points) rounder
    end <- getCurrentTime
    putStrLn $ "hashed 10^5 points in" ++ show (diffUTCTime end start)
