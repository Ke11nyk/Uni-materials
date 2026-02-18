{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_heap_sort (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "heap_sort"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "\1055\1110\1088\1072\1084\1110\1076\1072\1083\1100\1085\1077 \1089\1086\1088\1090\1091\1074\1072\1085\1085\1103 \1085\1072 Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
