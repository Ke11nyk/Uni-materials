{-# LANGUAGE BangPatterns #-}

module ModPow where

modPow :: Integer -> Integer -> Integer -> Integer
modPow _ 0 m = 1 `mod` m
modPow base expo modulus = go 1 (base `mod` modulus) expo
  where
    go !acc !_ 0 = acc
    go !acc !b !e
        | odd e     = go ((acc * b) `mod` modulus) ((b*b) `mod` modulus) (e `div` 2)
        | otherwise = go acc ((b*b) `mod` modulus) (e `div` 2)

modInverse :: Integer -> Integer -> Maybe Integer
modInverse a m =
    case egcd a m of
        (1, x, _) -> Just (x `mod` m)
        _         -> Nothing
  where
    egcd 0 b = (b, 0, 1)
    egcd a b =
        let (g, x, y) = egcd (b `mod` a) a
        in (g, y - (b `div` a)*x, x)
