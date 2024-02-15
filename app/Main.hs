module Main (main) where

import System.Environment
import RegexParser (forceParse)
import Phase2 (pars2IR)
import Interp (interp)
import Lib

-- | takes list (of order 2) and parsers the first one to operate on the second one
main :: IO ()
main = getArgs >>= sortFiles where
sortFiles :: [String] -> IO ()
sortFiles s = do
  f1 <- readFile (head s)
  f2 <- readFile (s!!1)
  (logs (runInterp f1 f2))

runInterp :: String -> String -> String
runInterp f1 f2 = interp (pars2IR (forceParse f1)) f2

logs :: String -> IO ()
logs o = do
  _ <- logDir
  logIter o 0
