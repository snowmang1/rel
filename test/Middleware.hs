module Middleware (middle) where

import Test.Tasty
import Test.Tasty.HUnit

import Phase2
import Structures

middle :: TestTree
middle = testGroup "Middleware Tests"
  [ testCase "OpStruct Eq" $
    Generate [Token Pattern "H"] [] @?=
      Generate [Token Pattern "H"] [],
    testCase "pars2IR empty list" $
     pars2IR [] @?= [],
    testCase "pars2IR given simple gen statement" $
      pars2IR [[Token Outg "g", Token Pattern "ABC", Token Sep "", Token Pattern "XYZ"]] @?=
      [(Generate [Token Pattern "ABC"] [Token Pattern "XYZ"])],
    testCase "pars2IR two statements" $
      pars2IR [[Token Outg "g", Token Pattern "ABC", Token Sep "", Token Pattern "XYZ"],
        [Token Outr "r", Token Pattern "AB", Token Sep "", Token Pattern "Hi",
        Token Or "B"]] @?= [(Generate [Token Pattern "ABC"] [Token Pattern "XYZ"]),
        (Replace [Token Pattern "AB"] [Token Pattern "Hi", Token Or "B"])]
  ]
