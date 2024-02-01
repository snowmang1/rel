module RegexParser (
  exprParser,
  relParser
) where

import Text.Parsec (manyTill, eof, Parsec, between, many1, (<|>), modifyState)
import Text.Parsec.Char (letter, char)

import Structures

-- | parses out the operator term *o* in the term *o[r][r]*
opParser :: Parsec String [String] Token
opParser = genParse <|> repParse
  where
  genParse :: Parsec String [String] Token
  genParse = do
    x <- char 'g'
    return $ Token Outg [x]
  repParse :: Parsec String [String] Token
  repParse = do
    x <- char 'r'
    return $ Token Outr [x]

-- | The parser representing the REL language
relParser :: Parsec String [String] [[Token]]
relParser = manyTill f eof
  where
  f :: Parsec String [String] [Token]
  f = do 
    op <- opParser
    r1 <- exprParser
    r2 <- exprParser
    return $ op : (r1 ++ (Token Sep "" : r2))

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
