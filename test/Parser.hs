module Parser (parser) where

import Test.Tasty
import Test.Tasty.HUnit

import RegexParser
import Text.Parsec

-- | for individual parser modules only
test :: Parsec String [String] [Token] -> (String -> Either ParseError [Token])
test p = runParser p [] ""

bigtest :: Parsec String [String] [[Token]] -> (String -> Either ParseError [[Token]])
bigtest p = runParser p [] ""

parser :: TestTree
parser = testGroup "Parser Tests"
    [ testCase "Tokenized english term" $
      case (test exprParser "[HjKi]") of
        Right [Token Pattern "HjKi"] -> assertBool "" True
        _ -> assertFailure "",
      testCase "Tokenize single one-or-more" $
      case (test exprParser "[+H]") of
        Right [Token Union "H"] -> assertBool "" True
        Right [Token Union x] -> assertFailure ("UNION RECOGNIZED BUT ONLY FOR" ++ x)
        Right [Token Pattern x] -> assertFailure ("DID RECOGNIZE UNION, ONLY PATTERN" ++ x)
        Right [Token Kleene x] -> assertFailure ("DID RECOGNIZE UNION, ONLY KLEENE" ++ x)
        _ -> assertFailure "",
      testCase "Tokenize single Kleene" $
      case (test exprParser "[*H]") of
        Right [Token Kleene "H"] -> assertBool "" True
        Right [Token Kleene x] -> assertFailure ("KLEENE RECOGNIZED BUT ONLY FOR" ++ x)
        Right [Token Pattern x] -> assertFailure ("DID RECOGNIZE UNION, ONLY PATTERN" ++ x)
        Right [Token Union x] -> assertFailure ("DID RECOGNIZE KLEENE, ONLY UNION" ++ x)
        _ -> assertFailure "",
      testCase "Tokenize multiple union" $
      test exprParser "[+H+S]" @?= Right [Token Union "H", Token Union "S"],
      testCase "Tokenize multiple kleene" $
      test exprParser "[*H*S+A*B]" @?= Right [Token Kleene "H", Token Kleene "S",
        Token Union "A", Token Kleene "B"],
      testCase "Tokenize one op[r][r] term" $
      bigtest relParser "g[H][B]" @?= Right [[Token Outg "g", Token Pattern "H",
        Token Sep "", Token Pattern "B"]],
      testCase "Tokenize one full term" $
      bigtest relParser "g[+H*BAMPM][BY+U]" @?= Right [[Token Outg "g",
        Token Union "H", Token Kleene "B", Token Pattern "AMPM",
        Token Sep "", Token Pattern "BY", Token Union "U"]],
      testCase "Tokenize two full terms" $
      bigtest relParser "g[H][B]r[A][C]" @?= Right [[Token Outg "g",
        Token Pattern "H", Token Sep "", Token Pattern "B"],
        [Token Outr "r", Token Pattern "A", Token Sep "",
        Token Pattern "C"]]
    ]

