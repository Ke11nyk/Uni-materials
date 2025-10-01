{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_faculty_newspaper (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "faculty_newspaper"
version :: Version
version = Version [1,0,0,0] []

synopsis :: String
synopsis = "\1052\1077\1088\1077\1078\1077\1074\1072 \1075\1072\1079\1077\1090\1072 \1092\1072\1082\1091\1083\1100\1090\1077\1090\1091 - \1110\1085\1092\1086\1088\1084\1072\1094\1110\1081\1085\1072 \1089\1080\1089\1090\1077\1084\1072"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
