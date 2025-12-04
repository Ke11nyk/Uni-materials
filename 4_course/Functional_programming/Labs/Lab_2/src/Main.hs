module Main where

import System.Environment (getArgs)
import Benchmark
import BSGS.Sequential
import BSGS.Parallel
import ModPow
import GHC.Conc (getNumCapabilities)

main :: IO ()
main = do
    args <- getArgs

    numThreads <- getNumCapabilities

    let (g,h,p) = case args of
            [g',h',p'] -> (read g', read h', read p')
            _          -> (5, 17, 23)

    putStrLn "=== Дискретний логарифм — оптимізований BSGS ==="
    putStrLn $ "Потрібно знайти x у рівнянні:"
    putStrLn $ "  " ++ show g ++ "^x ≡ " ++ show h ++ " (mod " ++ show p ++ ")"
    putStrLn $ "Потоків: " ++ show numThreads
    putStrLn ""

    putStrLn "1) Послідовна версія:"
    (seqRes, seqT) <- measure $ return $ bsgsSequential g h p
    putStrLn $ "   x = " ++ show seqRes
    putStrLn $ "   Перевірка: " ++ show (maybe False (\x -> modPow g x p == h) seqRes)
    putStrLn $ "   Час: " ++ show seqT ++ " секунд"
    putStrLn ""

    putStrLn "2) Паралельна версія:"
    (parRes, parT) <- measure $ return $ bsgsParallel g h p numThreads
    putStrLn $ "   x = " ++ show parRes
    putStrLn $ "   Перевірка: " ++ show (maybe False (\x -> modPow g x p == h) parRes)
    putStrLn $ "   Час: " ++ show parT ++ " секунд"
    putStrLn ""

    putStrLn "=== Порівняння ==="
    putStrLn $ "Прискорення (speedup): " ++ show (seqT / parT)
