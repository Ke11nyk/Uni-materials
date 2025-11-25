module TextProcessing
  ( normalizeSpaces
  , normalizeWord
  , stripDelimiters
  , extractWords
  , sortByVowelRatio
  ) where

import Types
import Constants
import TextAnalysis
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (isLetter, toLower)

-- Нормалізація пробілів
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

-- Нормалізація слова (до нижнього регістру)
normalizeWord :: Word' -> Word'
normalizeWord = map toLower

-- Видалення розділових знаків
stripDelimiters :: Word' -> Word'
stripDelimiters = filter (\c -> not (c `elem` delimiters))

-- Отримання слів з тексту
extractWords :: String -> [Word']
extractWords text = filter isValidWord $ map (normalizeWord . stripDelimiters) $ words normalizedText
  where
    normalizedText = normalizeSpaces text
    isValidWord w = not (null w) && any isLetter w

-- Сортування слів за зростанням частки голосних
sortByVowelRatio :: [Word'] -> [Word']
sortByVowelRatio = sortBy (comparing vowelRatio)
