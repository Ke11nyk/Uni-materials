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
synopsis = "Faculty Newspaper Information System"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
