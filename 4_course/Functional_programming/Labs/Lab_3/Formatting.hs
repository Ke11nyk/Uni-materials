module Formatting
  ( formatResultsNumbered
  , textStatistics
  ) where

import Types
import TextAnalysis
import TextProcessing
import Data.List (nub)

-- Форматування результатів з номером
formatResultsNumbered :: [(Word', Double)] -> String
formatResultsNumbered results = unlines $ header : separator : zipWith formatLine [1..] results
  where
    header = "№  | Слово                          | Частка голосних | Голосних/Букв"
    separator = replicate 80 '-'
    formatLine n (word, ratio) = 
      let num = show n ++ replicate (3 - length (show n)) ' '
          wordPadded = word ++ replicate (30 - length word) ' '
          ratioStr = take 6 (show ratio ++ repeat '0')
          vowelCount = countVowels word
          letterCount = countLetters word
      in num ++ "| " ++ wordPadded ++ "| " ++ ratioStr ++ "          | " ++ 
         show vowelCount ++ "/" ++ show letterCount

-- Обробка тексту (допоміжна функція для статистики)
processText :: String -> [(Word', Double)]
processText text = map (\w -> (w, vowelRatio w)) sortedWords
  where
    wordsFromText = extractWords text
    sortedWords = sortByVowelRatio wordsFromText

-- Статистика
textStatistics :: String -> String
textStatistics text = unlines
  [ "\n=== Статистика тексту ==="
  , "Загальна кількість слів: " ++ show totalWords
  , "Унікальних слів: " ++ show uniqueCount
  , "Середня частка голосних: " ++ take 6 (show avgRatio ++ repeat '0')
  , "Мінімальна частка голосних: " ++ take 6 (show minRatio ++ repeat '0')
  , "Максимальна частка голосних: " ++ take 6 (show maxRatio ++ repeat '0')
  , ""
  , "Слова з найменшою часткою голосних:"
  , "  " ++ unwords (take 3 $ map fst results)
  , ""
  , "Слова з найбільшою часткою голосних:"
  , "  " ++ unwords (take 3 $ reverse $ map fst results)
  ]
  where
    results = processText text
    words' = map fst results
    totalWords = length words'
    uniqueCount = length $ nub words'
    ratios = map snd results
    avgRatio = if null ratios then 0.0 else sum ratios / fromIntegral (length ratios)
    minRatio = if null ratios then 0.0 else minimum ratios
    maxRatio = if null ratios then 0.0 else maximum ratios
