{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.BLIF.Builder where

#if MIN_VERSION_base(4,10,0)
#else
import Data.Semigroup ((<>))
#endif

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
  <> ".model" <> space <> fromText name <> newline
  <> builderWires ".inputs"  (fromText <$> inputList)
  <> builderWires ".outputs" (fromText <$> outputList)
  <> builderWires ".clocks"  (fromText <$> clockList)
  <> foldMap builderCommand commands
  <> ".end" <> newline


builderCommand :: Command -> Builder
builderCommand (Subcircuit name assignments) = ".subckt"
  <> space <> fromText name
  <> space <> foldMap id (intersperse space $ var <$> assignments)
  <> newline
  where var (k, v) = fromText k <> "=" <> fromText v
builderCommand (LogicGate names (SingleOutputCover planes)) = ".names"
  <> foldMap (mappend space . fromText) names
  <> (if null planes then mempty else newline <> foldMap id (intersperse space $ fromText <$> planes))
  <> newline
builderCommand _ = mempty


builderWires :: Builder -> [Builder] -> Builder
builderWires _ [] = mempty
builderWires d xs = foldl (<>) (d <> space) (intersperse space xs) <> newline


space :: Builder
space = " "

newline :: Builder
newline = "\n"

