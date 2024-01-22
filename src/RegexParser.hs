module RegexParser (
  exprParser,
  Term,
  Op (..),
  Token (..),
  RELParser,
)
where

import Text.Parsec (Parsec, between, many1, (<|>), modifyState)
import Text.Parsec.Char (letter, char)

-- | Term can be a [ regex ] block or just an English character string
type Term = String

-- | Union => +, Kleene => * both refering to the regular expression theory
data Op = Union | Kleene | Or | Outg | Outr | Pattern | Sep deriving (Show, Eq)

-- | Token is built to house all match strings and bind operator info to terms
data Token = Token Op Term deriving (Show, Eq)

-- | convience type for common parser in REL
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

-- | Parses REL's subset of regex contained within square brackets `[]`
exprParser :: RELParser
exprParser = between beginTerm endTerm (many1 termParse)
  where
  beginTerm :: RELParser
  beginTerm = do
    _ <- char '['
    return $ []
  endTerm :: RELParser
  endTerm = do
    _ <- char ']'
    return $ []
