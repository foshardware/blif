
module Language.BLIF.Builder where

import Data.List (intersperse)
import qualified Data.Text.Lazy.IO as Text
import Data.Text.Lazy.Builder

import Language.BLIF.Syntax


printBLIF :: BLIF -> IO ()
printBLIF = Text.putStr . toLazyText . builderBlif


builderBlif :: BLIF -> Builder
builderBlif (BLIF ms) = foldMap builderModel ms


builderModel :: Model -> Builder
builderModel (Model name inputList outputList clockList commands) = newline
  <> fromString ".model" <> space <> fromText name <> newline
  <> builderWires (fromString ".inputs")  (fromText <$> inputList)
  <> builderWires (fromString ".outputs") (fromText <$> outputList)
  <> builderWires (fromString ".clocks")  (fromText <$> clockList)
  <> foldl (<>) mempty (builderCommand <$> commands)
  <> fromString ".end" <> newline


builderCommand :: Command -> Builder
builderCommand (Subcircuit name assignments) = fromString ".subckt"
  <> space <> fromText name
  <> foldl (<>) space (intersperse space $ var <$> assignments)
  <> newline
  where var (k, v) = fromText k <> fromString "=" <> fromText v
builderCommand _ = mempty


builderWires :: Builder -> [Builder] -> Builder
builderWires _ [] = mempty
builderWires d xs = foldl (<>) (d <> space) (intersperse space xs) <> newline


space :: Builder
space = fromString " "

newline :: Builder
newline = fromString "\n"

