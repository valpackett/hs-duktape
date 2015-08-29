{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax, CPP #-}

-- | Haskell bindings for duktape, a very compact embedded ECMAScript (JavaScript) engine.
-- 
-- Concurrency note: you can't execute code with the same context from multiple threads in parallel.
-- If you use the same context from multiple threads, only one will run at a time, others will block.
module Scripting.Duktape (
  DuktapeCtx
, createDuktapeCtx
, evalDuktape
, callDuktape
) where

import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.Storable
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar (withMVar)
import           Control.Monad (void, forM_)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS
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

getValueOrError ∷ Ptr DuktapeHeap → CInt → IO (Either String (Maybe Value))
getValueOrError ctxPtr code =
  if code /= fromIntegral 0
     then c_duk_safe_to_string ctxPtr cMinusOne >>= peekCString >>= return . Left
     else getValueFromStack ctxPtr (-1) >>= return . Right

pop ∷ Ptr DuktapeHeap → α → IO α
pop ctxPtr retVal = c_duk_pop ctxPtr >> return retVal

pushValue ∷ Ptr DuktapeHeap → Value → IO ()
pushValue ctxPtr Null = c_duk_push_null ctxPtr
pushValue ctxPtr (Bool True) = c_duk_push_boolean ctxPtr 1
pushValue ctxPtr (Bool False) = c_duk_push_boolean ctxPtr 0
pushValue ctxPtr (Number n) = c_duk_push_number ctxPtr $ realToFrac n
pushValue ctxPtr (String s) = void $ BS.useAsCStringLen (encodeUtf8 s) $ \(sCstr, sLen) →
                                c_duk_push_lstring ctxPtr sCstr $ fromIntegral sLen
pushValue ctxPtr (Array v) = do
  idx ← c_duk_push_array ctxPtr
  let pushElement x i = pushValue ctxPtr x >> c_duk_put_prop_index ctxPtr idx i
  V.zipWithM_ pushElement v (V.enumFromN 0 $ V.length v)
pushValue ctxPtr (Object m) = do
  idx ← c_duk_push_object ctxPtr
  forM_ (HMS.toList m) $ \(k, x) →
    BS.useAsCString (encodeUtf8 k) $ \kCstr →
      pushValue ctxPtr x >> c_duk_put_prop_string ctxPtr idx kCstr

-- | Creates a Duktape context.
createDuktapeCtx ∷ MonadIO μ ⇒ μ (Maybe DuktapeCtx)
createDuktapeCtx = liftIO $ createHeapF nullFunPtr

-- | Evaluates a string of ECMAScript on a given Duktape context.
--
-- Note: a top level object, like {some: 'thing'}, is not a valid string to evaluate.
-- You'd need to at least wrap it in parens, like ({some: 'thing'}).
evalDuktape ∷ MonadIO μ ⇒ DuktapeCtx → BS.ByteString → μ (Either String (Maybe Value))
evalDuktape ctx src =
  liftIO $ withCtx ctx $ \ctxPtr →
    BS.useAsCStringLen src $ \(srcCstr, srcLen) →
      pop ctxPtr =<< getValueOrError ctxPtr =<< c_duk_peval_lstring ctxPtr srcCstr (fromIntegral srcLen)

-- | Calls an ECMAScript function on a given Duktape context, passing in arguments from the Haskell world.
callDuktape ∷ MonadIO μ ⇒ DuktapeCtx
                        → Maybe BS.ByteString -- ^ The name of the object that contains the function (Nothing is the global object)
                        → BS.ByteString -- ^ The function name
                        → [Value] -- ^ The arguments
                        → μ (Either String (Maybe Value))
callDuktape ctx oname fname args =
  liftIO $ withCtx ctx $ \ctxPtr →
    BS.useAsCStringLen fname $ \(fnameCstr, fnameLen) → do
      case oname of
        Just on → void $ BS.useAsCString on $ \onameCstr →
          c_duk_get_global_string ctxPtr onameCstr
        Nothing → c_duk_push_global_object ctxPtr
      void $ c_duk_push_lstring ctxPtr fnameCstr $ fromIntegral fnameLen
      forM_ args $ pushValue ctxPtr
      pop ctxPtr =<< pop ctxPtr =<< getValueOrError ctxPtr =<< c_duk_pcall_prop ctxPtr (fromIntegral $ -2 - length args) (fromIntegral $ length args)
