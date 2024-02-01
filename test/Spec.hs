import Test.Tasty

-- Middleware.hs
import Middleware
-- Parser.hs
import Parser
-- Backend.hs
import Backend

main :: IO()
main = defaultMain testing

testing :: TestTree
testing = testGroup "Tests" [parser,middle,backend]
