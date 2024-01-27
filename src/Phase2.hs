module Phase2 (
  OpStruct (..),
  RelIR (..),
  pars2IR
) where

import RegexParser

-- TODO: phase 2 middleware
-- - translate Token list into readable structure list

-- | This creates a structure where each outside operation is a single object
data OpStruct = Generate [Token] [Token]
  | Replace [Token] [Token] deriving (Show, Eq)

-- | middleware hands this off to the backend for interp
-- effectivly making this is our IR
type RelIR = [OpStruct]

-- | read-in the [[Token]] from relParser and interpret it to be an instance of IR
-- where each operation unit is a single structure
pars2IR :: [[Token]] -> RelIR
pars2IR = fmap transTerm
  where
  transTerm :: [Token] -> OpStruct
  transTerm (Token Outg "g":tl) = let (t0,t1) = span (/= Token Sep "") tl in Generate t0 (tail t1)
  transTerm (Token Outr "r":tl) = let (t0,t1) = span (/= Token Sep "") tl in Replace t0 (tail t1)
  transTerm _ = undefined
