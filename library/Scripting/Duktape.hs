{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax, CPP #-}

module Scripting.Duktape (
  DuktapeCtx
, createDuktapeCtx
, evalDuktape
) where

import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.Storable
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Aeson
import           Scripting.Duktape.Raw

#define DUK_TYPE_NONE                     0
#define DUK_TYPE_UNDEFINED                1
#define DUK_TYPE_NULL                     2
#define DUK_TYPE_BOOLEAN                  3
#define DUK_TYPE_NUMBER                   4
#define DUK_TYPE_STRING                   5
#define DUK_TYPE_OBJECT                   6
#define DUK_TYPE_BUFFER                   7
#define DUK_TYPE_POINTER                  8
#define DUK_TYPE_LIGHTFUNC                9

cMinusOne ∷ CInt
cMinusOne = fromIntegral $ -1

data DukType = DukNone | DukUndefined | DukNull | DukBoolean | DukNumber | DukString | DukObject | DukBuffer | DukPointer | DukLightFunc

getTypeOnStack ∷ DuktapeCtx → Int → IO DukType
getTypeOnStack ctx idx = withForeignPtr ctx $ \ctxPtr → do
  t ← c_duk_get_type ctxPtr $ fromIntegral idx
  return $ case t of
    DUK_TYPE_NONE      → DukNone
    DUK_TYPE_UNDEFINED → DukUndefined
    DUK_TYPE_NULL      → DukNull
    DUK_TYPE_BOOLEAN   → DukBoolean
    DUK_TYPE_NUMBER    → DukNumber
    DUK_TYPE_STRING    → DukString
    DUK_TYPE_OBJECT    → DukObject
    DUK_TYPE_BUFFER    → DukBuffer
    DUK_TYPE_POINTER   → DukPointer
    DUK_TYPE_LIGHTFUNC → DukLightFunc
    _                  → DukNone

getStringFromStack ∷ DuktapeCtx → Int → IO BS.ByteString
getStringFromStack ctx idx = withForeignPtr ctx $ \ctxPtr → do
  let cIdx = fromIntegral idx
  lenPtr ← malloc ∷ IO (Ptr CSize)
  str ← c_duk_get_lstring ctxPtr cIdx lenPtr
  len ← peek lenPtr
  retVal ← BS.packCStringLen (str, fromIntegral len)
  free lenPtr
  return retVal

getValueFromStack ∷ DuktapeCtx → Int → IO (Maybe Value)
getValueFromStack ctx idx = withForeignPtr ctx $ \ctxPtr → do
  retType ← getTypeOnStack ctx idx
  let cIdx = fromIntegral idx
      ret = return . Just
  case retType of
    DukNull → ret Null
    DukBoolean → c_duk_get_boolean ctxPtr cIdx >>= ret . Bool . (== 1) . fromIntegral
    DukNumber → c_duk_get_number ctxPtr cIdx >>= ret . Number . realToFrac
    DukString → getStringFromStack ctx idx >>= ret . String . decodeUtf8
    DukObject → c_duk_json_encode ctxPtr cIdx >> getStringFromStack ctx idx >>= return . decode . BL.fromStrict
    _ → return Nothing

createDuktapeCtx ∷ IO (Maybe DuktapeCtx)
createDuktapeCtx = createHeapF nullFunPtr

evalDuktape ∷ DuktapeCtx → BS.ByteString → IO (Either String (Maybe Value))
evalDuktape ctx src =
  withForeignPtr ctx $ \ctxPtr →
    BS.useAsCStringLen src $ \(srcCstr, srcLen) → do
      evalCode ← c_duk_peval_lstring ctxPtr srcCstr $ fromIntegral srcLen
      retVal ← if evalCode /= fromIntegral 0
         then c_duk_safe_to_string ctxPtr cMinusOne >>= peekCString >>= return . Left
         else getValueFromStack ctx (-1) >>= return . Right
      c_duk_pop ctxPtr
      return retVal
