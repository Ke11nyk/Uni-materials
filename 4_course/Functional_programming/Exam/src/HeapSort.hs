module HeapSort
    ( heapSort
    ) where

-- Пірамідальне сортування
heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort xs = heapSort' (length xs) (buildMaxHeap (length xs - 1) xs)
  where
    heapSort' 0 ys = ys
    heapSort' 1 ys = ys
    heapSort' n ys = 
      let swapped = swap 0 (n-1) ys
          heapified = heapify 0 (n-2) swapped
      in heapSort' (n-1) heapified

-- Побудова max-heap з масиву
buildMaxHeap :: Ord a => Int -> [a] -> [a]
buildMaxHeap end xs = buildMaxHeap' ((end `div` 2)) end xs
  where
    buildMaxHeap' i e ys
      | i < 0     = ys
      | otherwise = buildMaxHeap' (i-1) e (heapify i e ys)

-- Відновлення властивості купи (heapify)
heapify :: Ord a => Int -> Int -> [a] -> [a]
heapify i end xs
  | i > end   = xs
  | largest /= i = heapify largest end (swap i largest xs)
  | otherwise    = xs
  where
    left = 2 * i + 1
    right = 2 * i + 2
    
    largest = 
      let l = if left <= end && (xs !! left) > (xs !! i) 
              then left 
              else i
      in if right <= end && (xs !! right) > (xs !! l)
         then right
         else l

-- Обмін елементів у списку
swap :: Int -> Int -> [a] -> [a]
swap i j xs
  | i == j    = xs
  | otherwise = 
      let elemI = xs !! i
          elemJ = xs !! j
          replace k
            | k == i    = elemJ
            | k == j    = elemI
            | otherwise = xs !! k
      in map replace [0..length xs - 1]