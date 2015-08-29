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
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar (withMVar)
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

withCtx ∷ DuktapeCtx → (Ptr DuktapeHeap → IO α) → IO α
withCtx ctx a = withMVar ctx $ \fPtr → withForeignPtr fPtr a

getTypeOnStack ∷ Ptr DuktapeHeap → Int → IO DukType
getTypeOnStack ctxPtr idx = do
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

getStringFromStack ∷ Ptr DuktapeHeap → Int → IO BS.ByteString
getStringFromStack ctxPtr idx = alloca $ \lenPtr → do
  let cIdx = fromIntegral idx
  str ← c_duk_get_lstring ctxPtr cIdx lenPtr
  len ← peek lenPtr
  retVal ← BS.packCStringLen (str, fromIntegral len)
  return retVal

getValueFromStack ∷ Ptr DuktapeHeap → Int → IO (Maybe Value)
getValueFromStack ctxPtr idx = do
  retType ← getTypeOnStack ctxPtr idx
  let cIdx = fromIntegral idx
      ret = return . Just
  case retType of
    DukNull → ret Null
    DukBoolean → c_duk_get_boolean ctxPtr cIdx >>= ret . Bool . (== 1) . fromIntegral
    DukNumber → c_duk_get_number ctxPtr cIdx >>= ret . Number . realToFrac
    DukString → getStringFromStack ctxPtr idx >>= ret . String . decodeUtf8
    DukObject → c_duk_json_encode ctxPtr cIdx >> getStringFromStack ctxPtr idx >>= return . decode . BL.fromStrict
    _ → return Nothing

createDuktapeCtx ∷ MonadIO μ ⇒ μ (Maybe DuktapeCtx)
createDuktapeCtx = liftIO $ createHeapF nullFunPtr

evalDuktape ∷ MonadIO μ ⇒ DuktapeCtx → BS.ByteString → μ (Either String (Maybe Value))
evalDuktape ctx src =
  liftIO $ withCtx ctx $ \ctxPtr →
    BS.useAsCStringLen src $ \(srcCstr, srcLen) → do
      evalCode ← c_duk_peval_lstring ctxPtr srcCstr $ fromIntegral srcLen
      retVal ← if evalCode /= fromIntegral 0
         then c_duk_safe_to_string ctxPtr cMinusOne >>= peekCString >>= return . Left
         else getValueFromStack ctxPtr (-1) >>= return . Right
      c_duk_pop ctxPtr
      return retVal
