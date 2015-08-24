# hs-duktape [![Hackage](https://img.shields.io/hackage/v/hs-duktape.svg?style=flat)](https://hackage.haskell.org/package/hs-duktape) [![Build Status](https://img.shields.io/travis/myfreeweb/hs-duktape.svg?style=flat)](https://travis-ci.org/myfreeweb/hs-duktape) [![MIT License](https://img.shields.io/badge/mit-license-green.svg?style=flat)](https://tldrlegal.com/license/mit-license)

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

[Aeson]: https://hackage.haskell.org/package/aeson
[lens-aeson]: https://hackage.haskell.org/package/lens-aeson

## Development

Use [stack] to build.  

```bash
$ stack build

$ stack test && rm tests.tix
```

[stack]: https://github.com/commercialhaskell/stack

## Contributing

Please feel free to submit pull requests!
Bugfixes and simple non-breaking improvements will be accepted without any questions :-)

By participating in this project you agree to follow the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/2/0/).

[The list of contributors is available on GitHub](https://github.com/myfreeweb/hs-duktape/graphs/contributors).

## License

Licensed under the MIT license (see to the `LICENSE` file).  
Haskell bindings: Copyright (c) 2015 Greg V <greg@unrelenting.technology>  
Duktape: Copyright (c) 2013-2015 by Duktape authors (see duktape/AUTHORS.rst)
