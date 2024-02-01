module Lib (
  interp,
  findPat
) where

-- TODO:
-- - interp library of functions
-- - interp function that outputs log files

import Structures

-- | takes an IR created from the file and file itself in the form of a string
-- then uses the IR to parse the file greedily (naively) and outputs a new
-- interpretation of the file. A file' if you will.
interp :: RelIR -> String -> String
interp [] fs = fs
interp (x:ir) fs = case x of
  Generate _ _ -> interp ir (genTerm x fs)
  Replace _ _  -> interp ir (repTerm x fs)
interp l f = undefined

-- | utility function for Union and Kleene mathcing
eatPat :: Term -> String -> (String, Int)
eatPat t' st' = aux t' st' 1 where
  aux :: Term -> String -> Int -> (String, Int)
  aux t (h:st) i = if t==[h] then aux t st (i+1) else (st, i)
  aux t [] i = ([], i)

-- | Pattern to match -> file stream -> file stream after match
-- returns Nothing if Pattern can not be matched
matchPat :: String -> String -> Maybe String
matchPat [] [] = Just []
matchPat [] fs = Just fs
matchPat _ []  = Nothing
matchPat (patHead:pat) (fileHead:fs) =
  case patHead==fileHead of
  True -> matchPat pat fs
  False -> Nothing

-- | function to find index of some pattern [Token]
findPat :: [Token] -> String -> Maybe (Int, Int)
-- TODO: this function needs to fully reset [Token] upon failed match
findPat p fs = findPat' p fs (0,0)
  where
  findPat' :: [Token] -> String -> (Int, Int) -> Maybe (Int, Int)
  findPat' [] _ (start, end) = if start==end then Nothing else Just (start, end)
  findPat' (Token Union s:pt) (h:fs') (start, end) =
    if [h]==s then
      let (fs'',end') = eatPat s fs' in
      findPat' pt fs'' (start, start+end')
    else
      findPat' pt fs' (start+1,start+1)
  findPat' (Token Kleene s:pt) (h:fs') (start, end) = 
    if [h]==s then
      let (fs'',end') = eatPat s fs' in
      findPat' pt fs'' (start, end+end')
    else
      findPat' pt fs' (start, end+1)
  findPat' (Token Pattern s:pt) fs' (start, end) =
    case matchPat s fs' of
    Nothing -> findPat' pt fs' (start+1, start+1)
    Just fs'' -> findPat' pt fs'' (start, end+(length s))
  findPat' _ _ _ = undefined -- no Or for now, v1 /= Binary Ops

-- | these will consume one OpStruct and preform the operation Generate on the given
-- file string fs: find a pattern and append a string to it
genTerm :: OpStruct -> String -> String
genTerm op fs = fs

-- | these will consume one OpStruct and preform the operation Replace on the given
-- file string fs: find a pattern p and replace it with p'
repTerm :: OpStruct -> String -> String
repTerm op fs = fs
