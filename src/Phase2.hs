module Phase2 (
  pars2IR
) where

import Structures

-- | read-in the [[Token]] from relParser and interpret it to be an instance of IR
-- where each operation unit is a single structure
pars2IR :: [[Token]] -> RelIR
pars2IR = fmap transTerm
  where
  transTerm :: [Token] -> OpStruct
  transTerm (UToken Outg "g":tl) = let (t0,t1) = span (/= UToken Sep "") tl in Generate t0 (tail t1)
  transTerm (UToken Outr "r":tl) = let (t0,t1) = span (/= UToken Sep "") tl in Replace t0 (tail t1)
  transTerm _ = undefined
