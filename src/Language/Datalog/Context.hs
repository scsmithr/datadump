module Language.Datalog.Context
  ()
where

import           Language.Datalog.Relation      ( Tuple
                                                , Relation
                                                , Variable
                                                , shiftRecent
                                                , emptyRecent
                                                )

-- | A context for repeated evaluation of tracked variables.
data Context a = Context [Variable a]

iterateVariables :: Context a -> Context a
iterateVariables (Context vs) = Context (shiftRecent <$> vs)

shouldStopComputation :: (Eq a) => Context a -> Bool
shouldStopComputation (Context vs) = any emptyRecent vs
