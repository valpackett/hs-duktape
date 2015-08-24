{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Scripting.DuktapeSpec (spec) where

import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon
import           Data.Maybe
import           Data.Aeson hiding (json)
import           Scripting.Duktape

spec ∷ Spec
spec = do
  describe "evalDuktape" $ do
    it "evaluates ECMAScript expressions" $ do
      ctx ← createDuktapeCtx
      r0 ← evalDuktape (fromJust ctx) ""
      r0 `shouldBe` Right Nothing
      rE ← evalDuktape (fromJust ctx) "print('hello"
      rE `shouldBe` Left "SyntaxError: eof or line terminator while parsing string literal (line 1)"
      rT ← evalDuktape (fromJust ctx) "1 == 1"
      rT `shouldBe` (Right $ Just $ Bool True)
      rF ← evalDuktape (fromJust ctx) "1 == 2"
      rF `shouldBe` (Right $ Just $ Bool False)
      rN ← evalDuktape (fromJust ctx) "12345"
      rN `shouldBe` (Right $ Just $ Number 12345)
      rS ← evalDuktape (fromJust ctx) "'string'"
      rS `shouldBe` (Right $ Just $ String "string")
      rA ← evalDuktape (fromJust ctx) "[1,2,'string',{what:'ever'}]"
      rA `shouldBe` (Right $ Just [json|[1,2,"string",{"what":"ever"}]|])
      rO ← evalDuktape (fromJust ctx) "(function () { return {what: ['ever', 1, 2, 3]} })()"
      -- WTF: top level {what: ['ever', 1, 2, 3]} is serialized to JSON by duktape as just ['ever', 1, 2, 3]
      rO `shouldBe` (Right $ Just [json|{"what":["ever", 1, 2, 3]}|])
