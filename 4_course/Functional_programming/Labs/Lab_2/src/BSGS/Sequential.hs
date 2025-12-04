module BSGS.Sequential (bsgsSequential) where

import qualified Data.HashMap.Strict as HM
import ModPow

bsgsSequential :: Integer -> Integer -> Integer -> Maybe Integer
bsgsSequential g h p = do
    let m = ceiling (sqrt (fromIntegral p :: Double))
    let baby = HM.fromList [(modPow g j p, j) | j <- [0..m-1]]

    inv <- modInverse (modPow g m p) p

    let search i
            | i >= m    = Nothing
            | otherwise =
                let gamma = (h * modPow inv i p) `mod` p
                in case HM.lookup gamma baby of
                    Just j  -> Just (i*m + j)
                    Nothing -> search (i+1)

    search 0
