module Main where

import HeapSort

main :: IO ()
main = do
  putStrLn "=== Пірамідальне сортування ==="
  putStrLn ""
  
  putStrLn "Тест 1: Цілі числа"
  let test1 = [64, 34, 25, 12, 22, 11, 90]
  putStrLn $ "Вхідний масив: " ++ show test1
  putStrLn $ "Відсортований:  " ++ show (heapSort test1)
  putStrLn ""
  
  putStrLn "Тест 2: Малий масив"
  let test2 = [5, 1, 4, 2, 8]
  putStrLn $ "Вхідний масив: " ++ show test2
  putStrLn $ "Відсортований:  " ++ show (heapSort test2)
  putStrLn ""
  
  putStrLn "Тест 3: З від'ємними числами"
  let test3 = [3, 0, 2, 5, -1, 4, 1]
  putStrLn $ "Вхідний масив: " ++ show test3
  putStrLn $ "Відсортований:  " ++ show (heapSort test3)
  putStrLn ""
  
  putStrLn "Тест 4: Рядки"
  let test4 = ["banana", "apple", "cherry", "date"]
  putStrLn $ "Вхідний масив: " ++ show test4
  putStrLn $ "Відсортований:  " ++ show (heapSort test4)
  putStrLn ""
  
  putStrLn "Тест 5: Дробові числа"
  let test5 = [3.14, 1.41, 2.71, 0.5, 4.2]
  putStrLn $ "Вхідний масив: " ++ show test5
  putStrLn $ "Відсортований:  " ++ show (heapSort test5)