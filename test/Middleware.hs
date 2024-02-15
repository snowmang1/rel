module Middleware (middle) where

import Test.Tasty
import Test.Tasty.HUnit

import Phase2
import Structures

middle :: TestTree
middle = testGroup "Middleware Tests"
  [ testCase "OpStruct Eq" $
    Generate [UToken Pattern "H"] [] @?=
      Generate [UToken Pattern "H"] [],
    testCase "pars2IR empty list" $
     pars2IR [] @?= [],
    testCase "pars2IR given simple gen statement" $
      pars2IR [[UToken Outg "g", UToken Pattern "ABC", UToken Sep "", UToken Pattern "XYZ"]] @?=
      [(Generate [UToken Pattern "ABC"] [UToken Pattern "XYZ"])],
    testCase "pars2IR two statements" $
      pars2IR [[UToken Outg "g", UToken Pattern "ABC", UToken Sep "", UToken Pattern "XYZ"],
        [UToken Outr "r", UToken Pattern "AB", UToken Sep "", UToken Pattern "Hi"]] @?=
        [(Generate [UToken Pattern "ABC"] [UToken Pattern "XYZ"]), (Replace [UToken Pattern "AB"]
        [UToken Pattern "Hi"])],
    testCase "pars2IR Or statements" $
      pars2IR [[UToken Outr "r", BToken Or ("A","B"), UToken Sep "", BToken Or ("Y","U")]] @?=
        [(Replace [BToken Or ("A","B")] [BToken Or ("Y","U")])]
  ]
