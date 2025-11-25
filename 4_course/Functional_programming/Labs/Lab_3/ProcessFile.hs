{-# LANGUAGE OverloadedStrings #-}

-- Програма обробки тексту підручника з програмування
-- Сортування слів за зростанням частки голосних букв

import Data.List (sortBy, nub)
import Data.Char (toLower, isLetter, isSpace)
import Data.Ord (comparing)
import System.IO
import System.Environment (getArgs)

-- Основні типи даних
type Symbol = Char
type Word' = String
type Sentence = String
type Delimiter = Char

-- Голосні букви (українські та англійські)
vowels :: [Symbol]
vowels = "аеиіоуАЕИІОУaeiouyAEIOUY"

-- Розділові знаки
delimiters :: [Delimiter]
delimiters = ".!?,;:—-()[]{}\"'«»"

-- Перевірка чи є символ голосною буквою
isVowel :: Symbol -> Bool
isVowel c = c `elem` vowels

-- Підрахунок голосних букв у слові
countVowels :: Word' -> Int
countVowels = length . filter isVowel

-- Підрахунок всіх букв у слові
countLetters :: Word' -> Int
countLetters = length . filter isLetter

-- Обчислення частки голосних букв
vowelRatio :: Word' -> Double
vowelRatio word
  | totalLetters == 0 = 0.0
  | otherwise = fromIntegral vowelCount / fromIntegral totalLetters
  where
    vowelCount = countVowels word
    totalLetters = countLetters word

-- Нормалізація пробілів
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

-- Видалення розділових знаків
stripDelimiters :: Word' -> Word'
stripDelimiters = filter (\c -> not (c `elem` delimiters))

-- Отримання слів з тексту
extractWords :: String -> [Word']
extractWords text = filter (not . null) $ map stripDelimiters $ words normalizedText
  where
    normalizedText = normalizeSpaces text

-- Сортування слів за зростанням частки голосних
sortByVowelRatio :: [Word'] -> [Word']
sortByVowelRatio = sortBy (comparing vowelRatio)

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

-- Обробка тексту
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
  , "  " ++ show (take 3 $ map fst results)
  , ""
  , "Слова з найбільшою часткою голосних:"
  , "  " ++ show (take 3 $ reverse $ map fst results)
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

main :: IO ()
main = do
  -- Встановлення UTF-8 кодування
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  
  args <- getArgs
  let filename = if null args then "textbook.txt" else head args
  
  -- Читання файлу з UTF-8 кодуванням
  handle <- openFile filename ReadMode
  hSetEncoding handle utf8
  content <- hGetContents handle
  
  putStrLn $ "\n=== Обробка файлу: " ++ filename ++ " ===\n"
  
  let results = processText content
  
  putStrLn $ formatResultsNumbered results
  putStrLn $ textStatistics content
  
  hClose handle