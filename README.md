[![Hackage](https://img.shields.io/hackage/v/hs-duktape.svg?style=flat)](https://hackage.haskell.org/package/hs-duktape)
[![Linux Build Status](https://img.shields.io/travis/myfreeweb/hs-duktape.svg?style=flat)](https://travis-ci.org/myfreeweb/hs-duktape)
[![Windows Build Status](https://img.shields.io/appveyor/ci/myfreeweb/hs-duktape.svg?style=flat)](https://ci.appveyor.com/project/myfreeweb/hs-duktape)
[![MIT License](https://img.shields.io/badge/mit-license-green.svg?style=flat)](https://tldrlegal.com/license/mit-license)

# hs-duktape

Haskell bindings for [duktape], a very compact embedded ECMAScript (JavaScript) engine.

[duktape]: http://duktape.org

## Usage

Here's a simple REPL example:

```haskell
module Main where

import Scripting.Duktape
import Control.Monad (forever)
import Data.ByteString.Char8 (pack)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  dukm <- createDuktapeCtx
  case dukm of
    Nothing -> putStrLn "I can't even (start Duktape)"
    Just duk -> forever $ do
      putStr "duktape> "
      hFlush stdout
      retVal <- evalDuktape duk =<< return . pack =<< getLine
      case retVal of
        Left e -> putStrLn $ "Duktape error: " ++ e
        Right Nothing -> putStrLn "No result"
        Right (Just v) -> print v
```

[Aeson]'s `Value` type is used for exchanging values between Haskell and ECMAScript.  
[lens-aeson] is a good library for working with `Value`, um, values.

You can also call functions that are on the global object (or any object that's on the global object):

```haskell
dukm <- createDuktapeCtx
bresult <- callDuktape (fromJust dukm) Nothing "boolTest" [Bool True, Bool True, Bool False] -- boolTest(true, true, false)
aresult <- callDuktape (fromJust dukm) (Just "NumFuns") "sum" [Number 1, Number 2] -- NumFuns.sum(1, 2)
```

And expose Haskell functions (same as with calls: set on global or a property of global):

```haskell
dukm <- createDuktapeCtx
let dbl (Number x) = return $ Number $ x * 2 ∷ IO Value
    dbl _ = return $ String "wtf"
reD ← exposeFnDuktape (fromJust ctx) Nothing "double" dbl 
```

The functions must be of type `IO ()`, `IO Value`, `Value -> IO Value`, `Value -> Value -> IO Value`... and so on.
(Or with any `ToJSON`/`FromJSON` values instead of `Value`)

[Aeson]: https://hackage.haskell.org/package/aeson
[lens-aeson]: https://hackage.haskell.org/package/lens-aeson

## Development

Building from the repository requires initialization with the Duktape configuration script the first time (these files are included in the cabal distribution on Hackage). 

The duktape configure script requires python with the PyYAML library installed.

```bash
git clone --recurse-submodules https://github.com/myfreeweb/hs-duktape
dukconfig/build_dist.sh
```
Thereafter use [stack] to build.

```bash
$ stack build

$ stack test && rm tests.tix
```

[stack]: https://github.com/commercialhaskell/stack

## Contributing

Please feel free to submit pull requests!

By participating in this project you agree to follow the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/4/).

[The list of contributors is available on GitHub](https://github.com/myfreeweb/hs-duktape/graphs/contributors).

## License

Licensed under the MIT license (see to the `LICENSE` file).  
Haskell bindings: Copyright (c) 2015-2018 Greg V <greg@unrelenting.technology>  
Duktape: Copyright (c) 2013-2016 by Duktape authors (see duktape/AUTHORS.rst)
