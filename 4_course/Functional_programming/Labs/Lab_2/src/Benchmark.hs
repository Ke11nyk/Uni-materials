module Benchmark (measure) where

import Control.DeepSeq
import System.CPUTime

-- | Вимірювання часу
measure :: NFData a => IO a -> IO (a, Double)
measure action = do
    start <- getCPUTime
    result <- action
    result `deepseq` return ()
    end <- getCPUTime

    let secs = fromIntegral (end - start) / 1e12
    return (result, secs)
