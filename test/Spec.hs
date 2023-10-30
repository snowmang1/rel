
import Test.Tasty
import Test.Tasty.HUnit

import Lib (gen)

main :: IO()
main = defaultMain unittests

unittests :: TestTree
unittests = testGroup "UnitTests"
    [ testCase "generate function" $
      gen "10101g0" @?= ("10101", "0")
    ]
