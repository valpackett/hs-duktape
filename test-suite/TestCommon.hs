{-# LANGUAGE UnicodeSyntax #-}

module TestCommon (
  json
) where

import           Language.Haskell.TH.Quote
import           Data.Aeson.QQ

-- renames for vim
json ∷ QuasiQuoter
json = aesonQQ
