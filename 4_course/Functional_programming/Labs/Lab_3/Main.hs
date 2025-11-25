{-# LANGUAGE OverloadedStrings #-}

-- Програма обробки тексту підручника з програмування
-- Сортування слів за зростанням частки голосних букв

module Main where

import System.IO
import System.Environment (getArgs)
import TextProcessing
import TextAnalysis
import Formatting

-- Обробка тексту
processText :: String -> [(String, Double)]
processText text = map (\w -> (w, vowelRatio w)) sortedWords
  where
    wordsFromText = extractWords text
    sortedWords = sortByVowelRatio wordsFromText

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
