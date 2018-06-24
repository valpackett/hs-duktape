{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax, CPP, FlexibleInstances #-}

-- | Haskell bindings for duktape, a very compact embedded ECMAScript (JavaScript) engine.
-- 
-- Concurrency note: you can't execute code with the same context from multiple threads in parallel.
-- If you use the same context from multiple threads, only one will run at a time, others will block.
module Scripting.Duktape (
  DuktapeCtx
, createDuktapeCtx
, evalDuktape
, callDuktape
, exposeFnDuktape
, Duktapeable
) where

import           Foreign.Ptr
import           Foreign.ForeignPtr hiding (addForeignPtrFinalizer)
import           Foreign.Storable
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Concurrent (addForeignPtrFinalizer)
import           Foreign.Marshal.Alloc
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar (withMVar)
import           Control.Monad (void, forM_, liftM)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Foreign as TF
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe (fromMaybe)
import           Scripting.Duktape.Raw

cMinusOne ∷ CInt
cMinusOne = fromIntegral $ -1

data DukType = DukNone | DukUndefined | DukNull | DukBoolean | DukNumber | DukString | DukObject | DukBuffer | DukPointer | DukLightFunc

withCtx ∷ DuktapeCtx → (Ptr DuktapeHeap → IO α) → IO α
withCtx ctx a = withMVar ctx $ \fPtr → withForeignPtr fPtr a

getTypeOnStack ∷ Ptr DuktapeHeap → Int → IO DukType
getTypeOnStack ctxPtr idx = liftM readType $ c_duk_get_type ctxPtr $ fromIntegral idx
  where readType x | x == c_DUK_TYPE_NONE      = DukNone
                   | x == c_DUK_TYPE_UNDEFINED = DukUndefined
                   | x == c_DUK_TYPE_NULL      = DukNull
                   | x == c_DUK_TYPE_BOOLEAN   = DukBoolean
                   | x == c_DUK_TYPE_NUMBER    = DukNumber
                   | x == c_DUK_TYPE_STRING    = DukString
                   | x == c_DUK_TYPE_OBJECT    = DukObject
                   | x == c_DUK_TYPE_BUFFER    = DukBuffer
                   | x == c_DUK_TYPE_POINTER   = DukPointer
                   | x == c_DUK_TYPE_LIGHTFUNC = DukLightFunc
                   | otherwise                 = DukNone

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
pushValue ctxPtr (String s) = void $ TF.withCStringLen s $ \(sCstr, sLen) →
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

pushObjectOrGlobal ∷ Ptr DuktapeHeap → Maybe BS.ByteString → IO Bool
pushObjectOrGlobal ctxPtr (Just on) = liftM (/= 0) $ BS.useAsCString on $ \onameCstr → c_duk_get_global_string ctxPtr onameCstr
pushObjectOrGlobal ctxPtr Nothing   = c_duk_push_global_object ctxPtr >> return True

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
      oVal ← pushObjectOrGlobal ctxPtr oname
      if oVal
         then do
           void $ c_duk_push_lstring ctxPtr fnameCstr $ fromIntegral fnameLen
           forM_ args $ pushValue ctxPtr
           pop ctxPtr =<< pop ctxPtr =<< getValueOrError ctxPtr =<< c_duk_pcall_prop ctxPtr (fromIntegral $ -2 - length args) (fromIntegral $ length args)
         else pop ctxPtr $ Left $ "Nonexistent property of global object: " ++ show (fromMaybe "" oname)

class Duktapeable ξ where
  runInDuktape ∷ Int → ξ → Ptr DuktapeHeap → IO CInt
  argCount ∷ ξ → Int

instance Duktapeable (IO ()) where
  runInDuktape _ f _ = f >> return 0
  argCount _ = 0

instance (ToJSON v) => Duktapeable (IO v) where
  runInDuktape _ f ctxPtr = f >>= pushValue ctxPtr . toJSON >> return 1
  argCount _ = 0

instance (Duktapeable ξ, FromJSON v) => Duktapeable (v -> ξ) where
  argCount f = 1 + argCount (f undefined)
  runInDuktape stackIdx f ctxPtr = do
    stackVal <- fromMaybe Null <$> getValueFromStack ctxPtr stackIdx
    case parseEither parseJSON stackVal of
      Left err -> pushValue ctxPtr (toJSON err) >> return c_DUK_RET_TYPE_ERROR
      Right val -> runInDuktape (stackIdx + 1) (f val) ctxPtr

-- | Makes a Haskell function available in ECMAScript.
exposeFnDuktape ∷ (MonadIO μ, Duktapeable ξ)
                ⇒ DuktapeCtx
                → Maybe BS.ByteString -- ^ The name of the object that will contain the function (Nothing is the global object)
                → BS.ByteString -- ^ The function name
                → ξ -- ^ The function itself
                → μ (Either String ())
exposeFnDuktape ctx oname fname f =
  liftIO $ withMVar ctx $ \fPtr → withForeignPtr fPtr $ \ctxPtr →
    BS.useAsCString fname $ \fnameCstr → do
      oVal ← pushObjectOrGlobal ctxPtr oname
      if oVal
         then do
           wrapped ← c_wrapper $ runInDuktape 0 f
           addForeignPtrFinalizer fPtr (freeHaskellFunPtr wrapped)
           void $ c_duk_push_c_function ctxPtr wrapped $ fromIntegral $ argCount f
           void $ c_duk_put_prop_string ctxPtr (-2) fnameCstr
           pop ctxPtr $ Right ()
         else pop ctxPtr $ Left $ "Nonexistent property of global object: " ++ show oname
