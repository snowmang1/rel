module Lib (
  interp,
  findPat,
  matchPre,
  p2s
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
  Generate find gen -> interp ir (genTerm find gen fs)
  Replace find rep  -> interp ir (repTerm find rep fs)
interp l f = undefined

-- | utility function for Union and Kleene mathcing
eatPat :: Term -> String -> (String, Int)
eatPat t' st' = aux t' st' 1 where
  aux :: Term -> String -> Int -> (String, Int)
  aux t (h:st) i = if t==[h] then aux t st (i+1) else (h:st, i)
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

-- | match prefix will take a [Token] named a pattern and match a prefix 
-- and return the length of the match or return Nothing
matchPre :: [Token] -> String -> Maybe Int
matchPre p fs = matchPre' p fs 0 where
  matchPre' :: [Token] -> String -> Int -> Maybe Int
  matchPre' [] [] end = if end/=0 then Just end else Nothing
  matchPre' _ [] end  = Nothing
  matchPre' p' (h:fs') end = 
    case p' of
    []  -> Just end
    _   ->
      case head p' of
      Token Union s   -> case [h]==s of
        True  -> let (fs'', end') = eatPat s fs' in matchPre' (tail p') fs'' (end+end')
        False -> Nothing
      Token Kleene s  -> case [h]==s of
        True  -> let (fs'', end') = eatPat s fs' in matchPre' (tail p') fs'' (end+end')
        False -> matchPre' (tail p') fs' (end+1)
      Token Pattern s -> case matchPat s (h:fs') of
        Just fs'' -> matchPre' (tail p') fs'' (end+(length s))
        Nothing   -> Nothing
      _ -> undefined

-- | function to find index of some pattern [Token]
findPat :: [Token] -> String -> Maybe (Int, Int)
-- TODO: this function needs to fully reset [Token] upon failed match
findPat p fs = findPat' [] p fs (0,0)
  where
  findPat' :: [Token] -> [Token] -> String -> (Int, Int) -> Maybe (Int, Int)
  findPat' _ _ [] (start, end) = if start==end then Nothing else Just (start, end)
  findPat' mp (Token Union s:pt) (h:fs') (start, end) =
    if [h]==s then
      let (fs'',end') = eatPat s fs' in
      findPat' (Token Union s:mp) pt fs'' (start, start+end')
    else
      findPat' [] ((reverse mp)++(Token Union s:pt)) fs' (start+1,start+1)
  findPat' mp (Token Kleene s:pt) (h:fs') (start, end) = 
    if [h]==s then
      let (fs'',end') = eatPat s fs' in
      findPat' (Token Kleene s:mp) pt fs'' (start, end+end')
    else
      case start==end of
      True  -> findPat' (Token Kleene s:mp) pt fs' (start, end+1) --prefix non-disruptive
      False -> findPat' [] ((reverse mp)++(Token Kleene s:pt)) fs' (start+1, start+1) -- disreuptive
  findPat' mp (Token Pattern s:pt) fs' (start, end) =
    case matchPat s fs' of
    Nothing -> findPat' [] ((reverse mp)++(Token Pattern s:pt)) (tail fs') (start+1, start+1)
    Just fs'' -> findPat' (Token Pattern s:mp) pt fs'' (start, end+(length s))
  findPat' _ _ _ _ = undefined -- no Or for now, v1 /= Binary Ops

-- | takes a list of tokens and creates an output string
p2s :: [Token] -> String
p2s p = p2s' p "" where
  p2s' p a = case p of
    []                 -> a
    Token Union s:pt   -> p2s' pt (a ++ s)
    Token Kleene s:pt  -> p2s' pt (a ++ "")
    Token Pattern s:pt -> p2s' pt (a ++ s)
    _ ->  p2s' (tail p) (a ++ "")

-- | these will consume one OpStruct and preform the operation Generate on the given
-- file string fs: find a pattern and append a string to it
genTerm :: [Token] -> [Token] -> String -> String
genTerm find gen fs = genTerm' find gen fs "" where
  genTerm' find' gen' fs' x =
    case matchPre find' fs' of
    Just end  -> let (pref, postf) = splitAt end fs' in
      genTerm' find' gen' postf (x ++ pref ++ (p2s gen'))
    Nothing   -> x

-- | these will consume one OpStruct and preform the operation Replace on the given
-- file string fs: find a pattern p and replace it with p'
repTerm :: [Token] -> [Token] -> String -> String
repTerm find rep fs = repTerm' find rep fs "" where
  repTerm' find' rep' fs' x =
    case matchPre find' fs' of
    Just end  -> let (pref, postf) = splitAt end fs' in
      repTerm' find' rep' postf (x ++ (p2s rep'))
    Nothing   -> x
