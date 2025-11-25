module TextAnalysis
  ( isVowel
  , countVowels
  , countLetters
  , vowelRatio
  ) where

import Types
import Constants
import Data.Char (isLetter)

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
