{- | Types and functions for finite automata. -}
module Language.FiniteState
  ( DFA(..)
  , NFA(..)
  , evalDFA
  , evalNFA
  )
where

import           Data.List                      ( intersect
                                                , union
                                                )

-- | Represents a 5-tuple containing a finite set of states, an alphabet, a
-- delta (transition) function, a start state, and a set of accepting end
-- states.
data DFA st sy = DFA [st] [sy] (st -> sy -> st) st [st]

-- | Evaluate a DFA with some word, testing if the end state matches an
-- accepting state.
evalDFA :: Eq st => DFA st sy -> [sy] -> Bool
evalDFA (DFA _ _ delta start fs) word = end `elem` fs
  where end = foldl delta start word

-- | Same as NFA, except the delta function may return many states for a single
-- transition.
data NFA st sy = NFA [st] [sy] (st -> sy -> [st]) st [st]

-- | Evaluate an NFA with some word, testing if any of the end states match an
-- accepting state.
evalNFA :: Eq st => NFA st sy -> [sy] -> Bool
evalNFA (NFA _ _ delta start fs) word = not $ null $ ends `intersect` fs
 where
  ends = foldl delta' [start] word

  delta' []       _   = []
  delta' (x : xs) sym = delta x sym `union` delta' xs sym
