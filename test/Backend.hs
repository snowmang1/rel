module Backend (backend) where

import Test.Tasty
import Test.Tasty.HUnit

import Lib
import Structures

backend :: TestTree
backend = testGroup "Backend Tests" [interpTests, findPatTests]

interpTests :: TestTree
interpTests = testGroup "interp Testing"
  [ testCase "interp run with blank IR" $
      interp [] "abc" @?= "abc",
    testCase "interp simple generate" $
      interp [Generate [Token Pattern "abc"] [Token Pattern "123"]] "abc"
      @?= "abc123",
    testCase "interp simple generate" $
      interp [Replace [Token Pattern "abc"] [Token Pattern "123"]] "abc"
      @?= "123"
  ]

findPatTests :: TestTree
findPatTests = testGroup "findPat Testing"
  [
    testCase "findPat single Union, single match" $
      findPat [Token Union "b"] "b" @?= Just (0,1),
    testCase "findPat single Union, no match" $
      findPat [Token Union "b"] "c" @?= Nothing,
    testCase "findPat single Union, multiple match" $
      findPat [Token Union "b"] "bbbb" @?= Just (0,4),
    testCase "findPat single Kleene, single match" $
      findPat [Token Kleene "b"] "b" @?= Just (0,1),
    testCase "findPat single Kleene, multiple match" $
      findPat [Token Kleene "b"] "bbbb" @?= Just (0,4),
    testCase "findPat single Kleene, no match" $
      findPat [Token Kleene "b"] "c" @?= Just (0,1),
    testCase "findPat single Pattern, no match" $
      findPat [Token Pattern "sap"] "sa" @?= Nothing,
    testCase "findPat single Pattern, single match" $
      findPat [Token Pattern "sap"] "sap" @?= Just (0,3),
    testCase "findPat two Pattern, two match" $
      findPat [Token Pattern "hello", Token Pattern " World"] "hello World" @?= Just (0,11)
  ]
