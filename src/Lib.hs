module Lib (
  someFunc,
  exprParser,
  Token (..),
  Op (Pattern, Kleene, Union)
) where

someFunc :: IO ()
someFunc = putStrLn "Hello world"

-- TODO: parser for outer words and operators

import Text.Parsec (Parsec, between, many1, many, (<|>), modifyState)
import Text.Parsec.Char (letter, char)

-- | Term can be a [ regex ] block or just an English character string
type Term = String

-- | Union => +, Kleene => * both refering to the regular expression theory
data Op = Union | Kleene | Pattern deriving (Show, Eq)

-- | Token is built to house all match strings and bind operator info to terms
data Token = Token Op Term deriving (Show, Eq)

type RELParser = Parsec String [String] [Token]

-- | parses the body of reg ex
termParse :: Parsec String [String] Token
termParse =
  unionParser <|> kleeneParser <|> noOpParser
  where
  unionParser :: Parsec String [String] Token
  unionParser = do
    o <- char '+'
    c <- letter
    modifyState ([o,c] :)
    return $ Token Union [c]
  kleeneParser :: Parsec String [String] Token
  kleeneParser = do
    o <- char '*'
    c <- letter
    modifyState ([o,c] :)
    return $ Token Kleene [c]
  noOpParser :: Parsec String [String] Token
  noOpParser = do
    c <- many1 letter
    modifyState (c :)
    return $ Token Pattern c

-- | The 'A' referse to my cfg where A is a subset of a proper regex term
-- e.g: '*[ABC]', '[A*BC]', '*[B+CA]'
exprParser :: RELParser
exprParser = between beginTerm endTerm (many1 termParse)
  where
  beginTerm :: RELParser
  beginTerm = do
    x <- char '['
    return $ []
  endTerm :: RELParser
  endTerm = do
    x <- char ']'
    return $ []
