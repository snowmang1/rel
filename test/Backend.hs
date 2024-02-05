module Backend (backend) where

import Test.Tasty
import Test.Tasty.HUnit

import Interp
import Structures

backend :: TestTree
backend = testGroup "Backend Tests" [interpTests, matchPreTests]

interpTests :: TestTree
interpTests = testGroup "interp Testing"
  [ testCase "interp run with blank IR" $
      interp [] "abc" @?= "abc",
    testCase "interp simple generate" $
      interp [Generate [Token Pattern "abc"] [Token Pattern "123"]] "abc"
      @?= "abc123",
    testCase "interp complex generate" $
      interp [Generate [Token Union "h", Token Pattern "abc"] [Token Union "h", Token Kleene "i"]]
        "hhhabc" @?= "hhhabch",
    testCase "interp simple replace" $
      interp [Replace [Token Pattern "abc"] [Token Pattern "123"]] "abc"
      @?= "123",
    testCase "interp complex replace" $
      interp [Replace [Token Union "h", Token Pattern "abc"] [Token Pattern "123", Token Pattern "abc"]]
      "habc" @?= "123abc"
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
      findPat [Token Pattern "hello", Token Pattern " World"] "hello World" @?= Just (0,11),
    testCase "findPat Pattern after Failure, single match" $
      findPat [Token Pattern "a"] "hello a" @?= Just (6,7),
    testCase "findPat Union -> Pattern after Failure" $
      findPat [Token Union "a", Token Pattern "hello"] "baaahello" @?= Just (1, 9),
    testCase "findPat Kleene -> Pattern after Failure, actual match" $
      findPat [Token Kleene "a", Token Pattern "hello"] "bahello" @?= Just (1, 7)
  ]

matchPreTests :: TestTree
matchPreTests = testGroup "matchPre Testing"
  [
    testCase "matchPre single Union, single match" $
      matchPre [Token Union "b"] "b" @?= Just 1,
    testCase "matchPre single Union, no match" $
      matchPre [Token Union "b"] "c" @?= Nothing,
    testCase "matchPre single Union, multiple match" $
      matchPre [Token Union "b"] "bbbb" @?= Just 4,
    testCase "matchPre single Kleene, single match" $
      matchPre [Token Kleene "b"] "b" @?= Just 1,
    testCase "matchPre single Kleene, multiple match" $
      matchPre [Token Kleene "b"] "bbbb" @?= Just 4,
    testCase "matchPre single Kleene, no match" $
      matchPre [Token Kleene "b"] "c" @?= Just 1,
    testCase "matchPre single Pattern, no match" $
      matchPre [Token Pattern "sap"] "sa" @?= Nothing,
    testCase "matchPre single Pattern, single match" $
      matchPre [Token Pattern "sap"] "sap" @?= Just 3,
    testCase "matchPre two Pattern, two match" $
      matchPre [Token Pattern "hello", Token Pattern " World"] "hello World" @?= Just 11,
    testCase "matchPre Union -> Pattern -> Kleene" $
      matchPre [Token Union "h", Token Pattern " hi ", Token Kleene "a"] "hhh hi aaaa" @?= Just 11,
    testCase "matchPre Kleene -> Union -> Union -> Pattern" $
      matchPre [Token Kleene "h", Token Union "i", Token Union "r", Token Pattern "hi"]
        "hhhhhiiiiirrrrrrhi" @?= Just 18
  ]
