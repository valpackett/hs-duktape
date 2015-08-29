{-# LANGUAGE UnicodeSyntax #-}

module TestCommon (
  json
, r
) where

import           Language.Haskell.TH.Quote
import           Text.RawString.QQ
import           Data.Aeson.QQ

-- renames for vim
json ∷ QuasiQuoter
json = aesonQQ
