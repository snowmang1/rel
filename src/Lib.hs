module Lib ( someFunc, gen) where

-- type Parser = String -> [Char]
-- type Operator = [Char] -> (String, String)

someFunc :: IO ()
someFunc = putStrLn "Hello world"


genPref :: String -> Char -> String -> (String,String)
genPref [] _ pref = ([], pref) --should not be possible
genPref s 'g' pref = (s, pref)
genPref s x pref = genPref (tail s) (head s) (x:pref)
genSuf :: String -> Char -> String -> String
genSuf [] _ pref = pref --should not be possible
genSuf s x pref = genSuf (tail s) (head s) (x:pref)
gen :: String -> (String, String)
gen []  = ("", "")
gen s   = (pat, rep)
  where (ts, pat) = genPref (tail s) (head s) []
        rep = genSuf ts (head ts) []
