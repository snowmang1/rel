module Lib (relParser) where

import Text.Parsec (Parsec, manyTill, many, between, eof, (<|>))
import Text.Parsec.Char (char)
import RegexParser

-- TODO: create parser lib for complete REL grammar
-- - exprParser needs to be integrated with the CFG
-- - Op<Regex>

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
