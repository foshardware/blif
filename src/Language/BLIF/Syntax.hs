
module Language.BLIF.Syntax where

import Data.Text (Text)
import Data.Vector (Vector)


type Ident = Text

newtype BLIF = BLIF { models :: [Model] }
  deriving (Eq, Show)

data Model = Model ModelName InputList OutputList ClockList (Vector Command)
  deriving (Eq, Show)

modelName :: Model -> ModelName
modelName (Model name _ _ _ _) = name

type ModelName = Ident

type InputList = [Ident]

type OutputList = [Ident]

type ClockList = [Ident]

data Command
  = LogicGate [Ident] SingleOutputCover
  | LibraryGate Ident FormalActualList
  | Subcircuit Ident FormalActualList
  | Attribute Ident StringLiteral
  | Parameter Ident Plane
  deriving (Eq, Show)

newtype SingleOutputCover = SingleOutputCover [Plane]
  deriving (Eq, Show)

type Plane = Text
type InputPlane = Plane
type OutputPlane = Plane

type Subcircuit = Command
type LibraryGate = Command
type LogicGate = Command
type Attribute = Command
type Parameter = Command


type FormalActualList = [Assignment]

type Assignment = (Ident, Ident)

type StringLiteral = Text



