module Parser (parser) where

import Test.Tasty
import Test.Tasty.HUnit

import RegexParser
import Text.Parsec

import Structures

-- | for individual parser modules only
test :: Parsec String [String] [Token] -> (String -> Either ParseError [Token])
test p = runParser p [] ""

bigtest :: Parsec String [String] [[Token]] -> (String -> Either ParseError [[Token]])
bigtest p = runParser p [] ""

parser :: TestTree
parser = testGroup "Parser Tests" [v1, binOp]

binOp :: TestTree
binOp = testGroup "binary testing"
  [ 
    testCase "Or simple test" $
      case (test exprParser "[|[B][A]]") of
        Right [BToken Or ("B","A")] -> assertBool "" True
        _ -> assertFailure "",
    testCase "Or string parse test" $
      case (test exprParser "[|[abc][123]]") of
        Right [BToken Or ("abc","123")] -> assertBool "" True
        _ -> assertFailure "",
    testCase "Or expr test" $
      test exprParser "[|[A][B]]" @?= Right [BToken Or ("A","B")],
    testCase "Or complex parse" $
      bigtest relParser "g[|[A][B]+H][|[Y][Z]]" @?= Right [[UToken Outg "g", BToken Or ("A","B"),
      UToken Union "H", UToken Sep "", BToken Or ("Y","Z")]]
  ]

v1 :: TestTree
v1 = testGroup "v1 Parser Tests"
    [ testCase "Tokenized english term" $
      case (test exprParser "[HjKi]") of
        Right [UToken Pattern "HjKi"] -> assertBool "" True
        _ -> assertFailure "",
      testCase "Tokenize single one-or-more" $
      case (test exprParser "[+H]") of
        Right [UToken Union "H"] -> assertBool "" True
        Right [UToken Union x] -> assertFailure ("UNION RECOGNIZED BUT ONLY FOR" ++ x)
        Right [UToken Pattern x] -> assertFailure ("DID RECOGNIZE UNION, ONLY PATTERN" ++ x)
        Right [UToken Kleene x] -> assertFailure ("DID RECOGNIZE UNION, ONLY KLEENE" ++ x)
        _ -> assertFailure "",
      testCase "Tokenize single Kleene" $
      case (test exprParser "[*H]") of
        Right [UToken Kleene "H"] -> assertBool "" True
        Right [UToken Kleene x] -> assertFailure ("KLEENE RECOGNIZED BUT ONLY FOR" ++ x)
        Right [UToken Pattern x] -> assertFailure ("DID RECOGNIZE UNION, ONLY PATTERN" ++ x)
        Right [UToken Union x] -> assertFailure ("DID RECOGNIZE KLEENE, ONLY UNION" ++ x)
        _ -> assertFailure "",
      testCase "Tokenize multiple union" $
      test exprParser "[+H+S]" @?= Right [UToken Union "H", UToken Union "S"],
      testCase "Tokenize multiple kleene" $
      test exprParser "[*H*S+A*B]" @?= Right [UToken Kleene "H", UToken Kleene "S",
        UToken Union "A", UToken Kleene "B"],
      testCase "Tokenize one op[r][r] term" $
      bigtest relParser "g[H][B]" @?= Right [[UToken Outg "g", UToken Pattern "H",
        UToken Sep "", UToken Pattern "B"]],
      testCase "Tokenize one full term" $
      bigtest relParser "g[+H*BAMPM][BY+U]" @?= Right [[UToken Outg "g",
        UToken Union "H", UToken Kleene "B", UToken Pattern "AMPM",
        UToken Sep "", UToken Pattern "BY", UToken Union "U"]],
      testCase "Tokenize two full terms" $
      bigtest relParser "g[H][B]r[A][C]" @?= Right [[UToken Outg "g",
        UToken Pattern "H", UToken Sep "", UToken Pattern "B"],
        [UToken Outr "r", UToken Pattern "A", UToken Sep "",
        UToken Pattern "C"]]
    ]

