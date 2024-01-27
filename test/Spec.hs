import Test.Tasty

-- Middleware.hs
import Middleware
-- Parser.hs
import Parser

main :: IO()
main = defaultMain testing

testing :: TestTree
testing = testGroup "Tests" [parser,middle]
