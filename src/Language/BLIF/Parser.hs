{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.BLIF.Parser where

import Control.Monad
import Control.Monad.Reader
import Data.Text (Text, splitOn, count)
import Data.Vector (fromListN)
import Text.Parsec hiding (optional, count)
import Text.Parsec.String (GenParser)
import Text.Parsec.Pos
import Prelude hiding (null)

import Language.BLIF.Lexer
import Language.BLIF.Syntax hiding (modelName)


type Parser = ParsecT [Lexer Token] () (Reader Int)

parseBLIF :: Text -> Either ParseError BLIF
parseBLIF src = flip runReader n $ runParserT blif () "" $ lexer [] src
  where n = maximum $ count "\n" <$> splitOn ".model" src


blif :: Parser BLIF
blif = BLIF <$> many1 model <?> "blif"

model :: Parser Model
model = Model
  <$> modelName
  <*> inputList
  <*> outputList
  <*> clockList
  <*> (fromListN <$> lift ask <*> many1 command)
  <*  end_
  <?> "model"

modelName :: Parser ModelName
modelName = model_ *> ident <?> "model_name"

inputList :: Parser InputList
inputList = inputs_ *> many1 ident <|> pure [] <?> "decl_input_list"

outputList :: Parser OutputList
outputList = outputs_ *> many1 ident <|> pure [] <?> "decl_output_list"

clockList :: Parser ClockList
clockList = clock_ *> many1 ident <|> pure [] <?> "decl_clock_list"

command :: Parser Command
command
  =   logicGate
  <|> libraryGate
  <|> subcircuit
  <|> attribute
  <|> parameter
  <?> "command"

subcircuit :: Parser Subcircuit
subcircuit = subckt_ >> Subcircuit
  <$> ident
  <*> formalActualList
  <?> "subcircuit"

logicGate :: Parser LogicGate
logicGate = names_ >> LogicGate
  <$> many1 ident
  <*> singleOutputCover
  <?> "logic_gate"

singleOutputCover :: Parser SingleOutputCover
singleOutputCover = SingleOutputCover <$> planes <?> "single_output_cover"
    where planes = many1 (inputPlane <|> outputPlane) <|> pure ["0"]

libraryGate :: Parser LibraryGate
libraryGate = gate_ >> LibraryGate
  <$> ident
  <*> formalActualList
  <?> "library_gate"

formalActualList :: Parser FormalActualList
formalActualList = many1 assignment <?> "formal_actual_list"

assignment :: Parser Assignment
assignment = (,) <$> ident <*> (assign_ *> ident) <?> "assignment"

attribute :: Parser Attribute
attribute = attr_ >> Attribute
  <$> ident
  <*> stringLiteral
  <?> "attribute"

parameter :: Parser Parameter
parameter = param_ >> Parameter
  <$> ident
  <*> (inputPlane <|> outputPlane)
  <?> "parameter"


-----
--
-- think `optional` but for Either
eitherOr :: Parser a -> Parser b -> Parser (Either a b)
eitherOr a b = Left <$> a <|> Right <$> b

maybeToken :: (Token -> Maybe a) -> Parser a
maybeToken test = tokenPrim showT posT testT
  where
  showT (L _ t) = show t
  posT  _ (L x _) _ = pos2sourcePos x
  testT (L _ t) = test t
  pos2sourcePos (l, c) = newPos "" l c

ident :: Parser Ident
ident = maybeToken q
  where q (Tok_Ident t) = Just t
        q _ = Nothing

inputPlane :: Parser InputPlane
inputPlane = maybeToken q
  where q (Tok_InputPlane t) = Just t
        q _ = Nothing

outputPlane :: Parser OutputPlane
outputPlane = maybeToken q
  where q (Tok_InputPlane t) = Just t
        q _ = Nothing

stringLiteral :: Parser StringLiteral
stringLiteral = maybeToken q
  where q (Tok_StringLiteral t) = Just t
        q _ = Nothing

p :: Token -> Parser ()
p t = maybeToken $ \r -> if r == t then Just () else Nothing
model_ = p Tok_Model
inputs_ = p Tok_Inputs
outputs_ = p Tok_Outputs
clock_ = p Tok_Clock
end_ = p Tok_End
names_ = p Tok_Names
gate_ = p Tok_Gate
assign_ = p Tok_Assign
attr_ = p Tok_Attr
param_ = p Tok_Param
subckt_ = p Tok_Subckt

