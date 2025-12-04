{-# LANGUAGE BangPatterns #-}
module BSGS.Parallel (bsgsParallel) where

import Control.Parallel.Strategies
import qualified Data.HashMap.Strict as HM
import ModPow
import Data.Maybe (listToMaybe)

bsgsParallel :: Integer -> Integer -> Integer -> Int -> Maybe Integer
bsgsParallel g h p threads =
    let
        m = ceiling (sqrt (fromIntegral p :: Double))
        mInt = fromIntegral m :: Int

        -- baby table паралельно
        chunk = max 1 (mInt `div` (threads * 8))
        babyList = [ (modPow g j p, j) | j <- [0..m-1] ]
                   `using` parListChunk chunk rdeepseq
        babyTable = HM.fromList babyList

        inv = case modInverse (modPow g m p) p of
            Just x -> x
            _ -> error "No inverse"

        ranges = [(start, min (m-1) (start + fromIntegral chunk - 1))
                 | start <- [0, fromIntegral chunk .. m-1]]

        check (start, end) =
            let loop i
                    | i > end = Nothing
                    | otherwise =
                        let gamma = (h * modPow inv i p) `mod` p
                        in case HM.lookup gamma babyTable of
                            Just j  -> Just (i*m + j)
                            Nothing -> loop (i+1)
            in loop start

        results = map check ranges `using` parListChunk chunk rdeepseq
    in listToMaybe [x | Just x <- results]