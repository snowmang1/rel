module Backend (backend) where

import Test.Tasty
import Test.Tasty.HUnit

import Interp
import Structures

backend :: TestTree
backend = testGroup "Backend Tests" [interpTests, matchPreTests, binaryTests]

binaryTests :: TestTree
binaryTests = testGroup "interp binop testing"
  [
    testCase "simple rep Or" $
      interp [Replace [BToken Or ("A","B")] [UToken Pattern "abc"]] "A" @?= "abc",
    testCase "simple rep Or other" $
      interp [Replace [BToken Or ("A","B")] [UToken Pattern "abc"]] "B" @?= "abc",
    testCase "simple gen Or" $
      interp [Generate [BToken Or ("A","B")] [UToken Pattern "abc"]] "A" @?= "Aabc",
    testCase "simple gen Or other" $
      interp [Generate [BToken Or ("A","B")] [UToken Pattern "abc"]] "B" @?= "Babc",
    testCase "complex rep Or" $
      interp [Replace [UToken Pattern "123", BToken Or ("A","B")] [UToken Pattern "abc"]] "123A"
      @?= "abc",
    testCase "complex gen Or" $
      interp [Replace [UToken Pattern "123", BToken Or ("A","B")] [UToken Pattern "abc"]] "123A"
      @?= "abc",
    testCase "prefix gen Or" $
      interp [Replace [BToken Or ("I", "U"), UToken Pattern "123", BToken Or ("A","B")]
      [UToken Pattern "abc"]] "I123A" @?= "abc",
    testCase "print Or" $
      interp [Replace [UToken Pattern "abc"] [BToken Or ("A", "B")]] "abc" @?= "A"
  ]

interpTests :: TestTree -- Here is where new tests should probably go for data collection
interpTests = testGroup "interp Testing"
  [
    -- this works by utilizing the internal `interp` function which is described in the README
    testCase "interp run with blank IR" $
      interp [] "abc" @?= "abc",
    testCase "interp simple generate" $
      interp [Generate [UToken Pattern "abc"] [UToken Pattern "123"]] "abc"
      @?= "abc123",
    testCase "interp complex generate" $
      interp [Generate [UToken Union "h", UToken Pattern "abc"] [UToken Union "h", UToken Kleene "i"]]
        "hhhabc" @?= "hhhabch",
    testCase "interp simple replace" $
      interp [Replace [UToken Pattern "abc"] [UToken Pattern "123"]] "abc"
      @?= "123",
    testCase "interp complex replace" $
      interp [Replace [UToken Union "h", UToken Pattern "abc"] [UToken Pattern "123", UToken Pattern "abc"]]
      "habc" @?= "123abc"
  ]

matchPreTests :: TestTree
matchPreTests = testGroup "matchPre Testing"
  [
    testCase "matchPre single Union, single match" $
      matchPre [UToken Union "b"] "b" @?= Just 1,
    testCase "matchPre single Union, no match" $
      matchPre [UToken Union "b"] "c" @?= Nothing,
    testCase "matchPre single Union, multiple match" $
      matchPre [UToken Union "b"] "bbbb" @?= Just 4,
    testCase "matchPre single Kleene, single match" $
      matchPre [UToken Kleene "b"] "b" @?= Just 1,
    testCase "matchPre single Kleene, multiple match" $
      matchPre [UToken Kleene "b"] "bbbb" @?= Just 4,
    testCase "matchPre single Kleene, no match" $
      matchPre [UToken Kleene "b"] "c" @?= Just 1,
    testCase "matchPre single Pattern, no match" $
      matchPre [UToken Pattern "sap"] "sa" @?= Nothing,
    testCase "matchPre single Pattern, single match" $
      matchPre [UToken Pattern "sap"] "sap" @?= Just 3,
    testCase "matchPre two Pattern, two match" $
      matchPre [UToken Pattern "hello", UToken Pattern " World"] "hello World" @?= Just 11,
    testCase "matchPre Union -> Pattern -> Kleene" $
      matchPre [UToken Union "h", UToken Pattern " hi ", UToken Kleene "a"] "hhh hi aaaa" @?= Just 11,
    testCase "matchPre Kleene -> Union -> Union -> Pattern" $
      matchPre [UToken Kleene "h", UToken Union "i", UToken Union "r", UToken Pattern "hi"]
        "hhhhhiiiiirrrrrrhi" @?= Just 18
  ]
