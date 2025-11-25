{-# LANGUAGE OverloadedStrings #-}

module Constants
  ( vowels
  , delimiters
  ) where

import Types

-- Голосні букви (українські та англійські)
vowels :: [Symbol]
vowels = "аеиіоуАЕИІОУaeiouyAEIOUY"

-- Розділові знаки
delimiters :: [Delimiter]
delimiters = ".!?,;:—–-()[]{}\"'«»"
