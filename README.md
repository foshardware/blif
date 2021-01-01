# BLIF parser

Parses Berkeley Logic Interchange Format (BLIF) into an Abstract Syntax Tree (AST). Intended for use with LibreSilicon Compiler (lsc).

## Usage

```haskell

import qualified Data.Text as Text

import Language.BLIF.Parser (parseBLIF)

main = do
  file <- Text.readFile "my.blif"
  putStrLn $ show $ parseBLIF file

```

## Building BLIF files from AST

`blif` also allows for efficient construction of BLIF files through operations on the `Builder` type.

```haskell

import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Lazy
import Data.Text.Lazy.Builder

import Language.BLIF.Syntax
import Language.BLIF.Parser (parseBLIF)
import Language.BLIF.Builder

main = do
  file <- Text.readFile "my.blif"
  case parseBLIF file of
    Left err -> putStrLn $ show err
    Right blif -> Lazy.putStr $ toLazyText $ builderBlif blif
  
```
