{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Parallel.Strategies
import Control.Monad
import Data.List (find)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.DeepSeq
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Conc (getNumCapabilities)

-- | Швидке модульне піднесення до степеня (a^b mod m)
modPow :: Integer -> Integer -> Integer -> Integer
modPow base expo modulus
    | modulus == 1 = 0
    | otherwise = go 1 base expo
  where
    go !result !b !e
        | e == 0    = result
        | odd e     = go ((result * b) `mod` modulus) (b * b `mod` modulus) (e `div` 2)
        | otherwise = go result (b * b `mod` modulus) (e `div` 2)

-- | Розширений алгоритм Евкліда для знаходження оберненого елемента
modInverse :: Integer -> Integer -> Maybe Integer
modInverse a m = 
    case extGCD a m of
        (g, x, _) | g == 1 -> Just (x `mod` m)
                  | otherwise -> Nothing
  where
    extGCD 0 b = (b, 0, 1)
    extGCD a b = let (g, x, y) = extGCD (b `mod` a) a
                 in (g, y - (b `div` a) * x, x)

-- | Baby-step Giant-step алгоритм (послідовна версія)
babyStepGiantStep :: Integer -> Integer -> Integer -> Maybe Integer
babyStepGiantStep g h p = do
    let m = ceiling (sqrt (fromIntegral p :: Double))
    
    -- Baby step: будуємо таблицю g^j mod p для j = 0..m-1
    let babyTable = Map.fromList [(modPow g j p, j) | j <- [0..m-1]]
    
    -- Обчислюємо g^(-m) mod p
    gInv <- modInverse (modPow g m p) p
    
    -- Giant step: шукаємо збіг
    let gammas = [(h * modPow gInv i p) `mod` p | i <- [0..m-1]]
    let result = find (\(gamma, i) -> Map.member gamma babyTable) 
                     (zip gammas [0..])
    
    case result of
        Just (gamma, i) -> 
            let j = babyTable Map.! gamma
            in Just (i * m + j)
        Nothing -> Nothing

-- | Паралельна версія Baby-step Giant-step
parallelBabyStepGiantStep :: Integer -> Integer -> Integer -> Int -> Maybe Integer
parallelBabyStepGiantStep g h p numThreads = do
    let m = ceiling (sqrt (fromIntegral p :: Double))
    
    -- Baby step (послідовно, бо потрібна хеш-таблиця)
    let babyTable = Map.fromList [(modPow g j p, j) | j <- [0..m-1]]
    
    -- Обчислюємо g^(-m) mod p
    gInv <- modInverse (modPow g m p) p
    
    -- Giant step (паралельно)
    let chunkSize = max 1 ((m + fromIntegral numThreads - 1) `div` fromIntegral numThreads)
    let ranges = [(i, min (i + chunkSize - 1) (m - 1)) | i <- [0, chunkSize .. m-1]]
    
    -- Паралельно обчислюємо гамми для кожного діапазону
    let checkRange (start, end) = 
            let gammas = [((h * modPow gInv i p) `mod` p, i) | i <- [start..end]]
                result = find (\(gamma, _) -> Map.member gamma babyTable) gammas
            in case result of
                Just (gamma, i) -> Just (i * m + babyTable Map.! gamma)
                Nothing -> Nothing
    
    let results = parMap rdeepseq checkRange ranges `using` parList rdeepseq
    
    listToMaybe [x | Just x <- results]

-- | Перевірка результату
verifyResult :: Integer -> Integer -> Integer -> Integer -> Bool
verifyResult g h p x = modPow g x p == h

main :: IO ()
main = do
    args <- getArgs
    
    -- Отримати реальну кількість потоків з RTS
    numThreads <- getNumCapabilities
    
    let (g, h, p) = case args of
            [g', h', p'] -> 
                ( fromMaybe 5 (readMaybe g')
                , fromMaybe 17 (readMaybe h')
                , fromMaybe 23 (readMaybe p')
                )
            _ -> (5, 17, 23) -- Приклад за замовчуванням: 5^x ≡ 17 (mod 23), x = 7
    
    putStrLn "=== Розв'язання задачі дискретного логарифмування ==="
    putStrLn $ "Знайти x: " ++ show g ++ "^x ≡ " ++ show h ++ " (mod " ++ show p ++ ")"
    putStrLn $ "Доступно потоків: " ++ show numThreads
    putStrLn ""
    
    -- Послідовна версія
    putStrLn "1. Послідовний алгоритм Baby-step Giant-step:"
    case babyStepGiantStep g h p of
        Just x -> do
            putStrLn $ "   Знайдено x = " ++ show x
            putStrLn $ "   Перевірка: " ++ show g ++ "^" ++ show x ++ " mod " 
                      ++ show p ++ " = " ++ show (modPow g x p)
            putStrLn $ "   Результат: " ++ if verifyResult g h p x 
                                           then "✓ Правильно" 
                                           else "✗ Помилка"
        Nothing -> putStrLn "   Розв'язку не знайдено"
    
    putStrLn ""
    
    -- Паралельна версія з реальною кількістю потоків
    putStrLn $ "2. Паралельний алгоритм Baby-step Giant-step (" 
              ++ show numThreads ++ " потоків):"
    case parallelBabyStepGiantStep g h p numThreads of
        Just x -> do
            putStrLn $ "   Знайдено x = " ++ show x
            putStrLn $ "   Перевірка: " ++ show g ++ "^" ++ show x ++ " mod " 
                      ++ show p ++ " = " ++ show (modPow g x p)
            putStrLn $ "   Результат: " ++ if verifyResult g h p x 
                                           then "✓ Правильно" 
                                           else "✗ Помилка"
        Nothing -> putStrLn "   Розв'язку не знайдено"
    
    putStrLn ""
    putStrLn "Програма завершена."
