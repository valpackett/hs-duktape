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
      rO ← evalDuktape (fromJust ctx) "({what: ['ever', 1, 2, 3]})"
      rO `shouldBe` (Right $ Just [json|{"what":["ever", 1, 2, 3]}|])

  describe "callDuktape" $ do
    it "calls ECMAScript functions" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) [r|
function returnsObj () { return {hello: {from: [1, 2, 3]}} }
function throwsErr () { throw new Error('hi') }
|]
      rE ← callDuktape (fromJust ctx) Nothing "n0thing!" []
      rE `shouldBe` (Left "TypeError: not callable")
      rO ← callDuktape (fromJust ctx) Nothing "returnsObj" []
      rO `shouldBe` (Right $ Just [json|{"hello": {"from": [1, 2, 3]}}|])
      rT ← callDuktape (fromJust ctx) Nothing "throwsErr" []
      rT `shouldBe` (Left "Error: hi")

    it "calls ECMAScript functions on objects" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) [r|
var Stuff = { x: 'testFromThis' }
Stuff.fun = function () { return this.x }
|]
      rS ← callDuktape (fromJust ctx) (Just "Stuff") "fun" []
      rS `shouldBe` (Right $ Just $ String "testFromThis")
      rE ← callDuktape (fromJust ctx) (Just "failNonExistent") "fun" []
      rE `shouldBe` (Left "Nonexistent property of global object: \"failNonExistent\"")

    it "calls ECMAScript functions with args" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) [r|
function double (x, y) { return x*2 + y*2 }
function awesomeString (x) { return '-= ' + x + ' =-' }
function boolTest (x, y, z) { return typeof(x) === 'boolean' && x && (y && !z) }
function arrTest (a) { return a.map(function (el) { return typeof(el) }).join('') }
function objTest (obj) { return obj.name + obj.stuff.filter(function (x) { return x != null && (typeof(x) == 'string' || typeof(x) == 'object') })
                                                    .map(function(x) { return typeof(x) == 'object' ? x.value : x } ).join('') }
|]
      rN ← callDuktape (fromJust ctx) Nothing "double" [Number 7, Number 8]
      rN `shouldBe` (Right $ Just $ Number 30)
      rS ← callDuktape (fromJust ctx) Nothing "awesomeString" [String "hello"]
      rS `shouldBe` (Right $ Just $ String "-= hello =-")
      rB ← callDuktape (fromJust ctx) Nothing "boolTest" [Bool True, Bool True, Bool False]
      rB `shouldBe` (Right $ Just $ Bool True)
      rA ← callDuktape (fromJust ctx) Nothing "arrTest" [[json|[1, 2, "test", null]|]]
      rA `shouldBe` (Right $ Just $ String "numbernumberstringobject")
      rO ← callDuktape (fromJust ctx) Nothing "objTest" [[json|{"stuff": [1, 2, "hello", null, {"value": "world"}], "name": "test"}|]]
      rO `shouldBe` (Right $ Just $ String "testhelloworld")

  describe "exposeFnDuktape" $ do
    it "exposes Haskell functions to ECMAScript" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) "var X = {}"
      let dbl (Number x) = return $ Number $ x * 2 ∷ IO Value
          dbl _ = return $ String "wtf"
          cnst = return $ Number 123 ∷ IO Value
      _ ← exposeFnDuktape (fromJust ctx) Nothing "double" dbl
      _ ← exposeFnDuktape (fromJust ctx) (Just "X") "cnst" cnst
      rd ← evalDuktape (fromJust ctx) "double(7) + X.cnst()"
      rd `shouldBe` (Right $ Just $ Number 137)
      rD ← callDuktape (fromJust ctx) Nothing "double" [Number 7]
      rD `shouldBe` (Right $ Just $ Number 14)
      rE ← callDuktape (fromJust ctx) Nothing "double" []
      rE `shouldBe` (Right $ Just $ String "wtf")
