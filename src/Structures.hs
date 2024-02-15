module Structures (
  OpStruct (..),
  RelIR,
  Term,
  UOp (..),
  BOp (..),
  Token (..),
  RELParser
) where

import Text.Parsec (Parsec)

-- | Term can be a [ regex ] block or just an English character string
type Term = String

-- | Unary operators Union => +, Kleene => * both refering to the regular expression theory
data UOp = Union | Kleene | Outg | Outr | Pattern | Sep deriving (Show, Eq)

-- | Binary operators
data BOp = Or deriving (Show, Eq)

-- | Token is built to house all match strings and bind operator info to terms
data Token = UToken UOp Term | BToken BOp (Term, Term) deriving (Show, Eq)

-- | convience type for common parser in REL
type RELParser = Parsec String [String] [Token]

-- | This creates a structure where each outside operation is a single object
data OpStruct = Generate [Token] [Token]
              | Replace  [Token] [Token] deriving (Show, Eq)

-- | middleware hands this off to the backend for interp
-- effectivly making this is our IR
type RelIR = [OpStruct]

