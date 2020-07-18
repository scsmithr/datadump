{-|
Logic for a (simple and slow) Datalog program.
-}

{-# LANGUAGE LambdaCase #-}

module Language.Datalog.Program
  ( Term(..)
  , Atom(..)
  , Rule(..)
  , Program
  , KnowledgeBase
  , Substitution
  , querySubstitutions
  )
where

import           Data.Function                  ( fix )
import           Data.List                      ( nub )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )

data Term = Var String
          | Sym String deriving (Eq, Show)

data Atom = Atom { symbol :: String
                 , atomTerms :: [Term] } deriving Eq

data Rule = Rule { ruleHead :: Atom
                 , ruleBody :: [Atom] }

type Program = [Rule]

type KnowledgeBase = [Atom]

type Substitution = [(Term, Term)]

emptySubstitution :: Substitution
emptySubstitution = []

-- Substitute all variable terms with its associated substitution.
substitute :: Atom -> Substitution -> Atom
substitute atom sub = atom { atomTerms = map subTerm (atomTerms atom) }
 where
  subTerm sym@Sym{} = sym
  subTerm var@Var{} = fromMaybe var (lookup var sub)

-- Unify variables in atom bodies againsts facts.
unify :: Atom -> Atom -> Maybe Substitution
unify (Atom symbol terms) (Atom symbol' terms')
  | symbol == symbol' = unifyTerms $ zip terms terms'
  | otherwise         = Nothing
 where
  unifyTerms :: [(Term, Term)] -> Maybe Substitution
  unifyTerms [] = Just emptySubstitution
  unifyTerms ((s@Sym{}, s'@Sym{}) : rest) | s == s'   = unifyTerms rest
                                          | otherwise = Nothing
  unifyTerms ((v@Var{}, s@Sym{}) : rest) = do
    incompleteSubs <- unifyTerms rest
    case lookup v incompleteSubs of
      Just s' | s /= s' -> Nothing
      _                 -> Just $ (v, s) : incompleteSubs
  unifyTerms ((_, Var{}) : _) = error "Unable to unify against a variable."

-- Extends a list of substitutions by evaluating an atom against a knowledge
-- base.
evalAtom :: KnowledgeBase -> Atom -> [Substitution] -> [Substitution]
evalAtom kb atom subs = do
  sub <- subs
  ext <- mapMaybe (unify $ substitute atom sub) kb
  return $ sub <> ext

walk :: KnowledgeBase -> [Atom] -> [Substitution]
walk kb = foldr (evalAtom kb) [emptySubstitution]

-- Evaluates a rule against some existing knowledge.
evalRule :: KnowledgeBase -> Rule -> KnowledgeBase
evalRule kb (Rule head body) = map (substitute head) (walk kb body)

-- Derives a new knowledge base from an existing knowledge and some set of
-- rules.
immediateConsequence :: Program -> KnowledgeBase -> KnowledgeBase
immediateConsequence rules kb = nub . (kb <>) . concatMap (evalRule kb) $ rules

-- Checks if the rule is range restricted, meaning that every variable in the
-- rule's head exists in the body.
isRangeRestricted :: Rule -> Bool
isRangeRestricted (Rule head body) = isSubsetOf (vars head)
                                                (concatMap vars body)
 where
  isSubsetOf as bs = all (`elem` bs) as
  vars (Atom _ terms) = nub $ filter
    (\case
      Var{} -> True
      _     -> False
    )
    terms

solve :: Program -> KnowledgeBase
solve rules | all isRangeRestricted rules = fix step []
            | otherwise = error "Program not range restricted."
 where
  step :: (KnowledgeBase -> KnowledgeBase) -> (KnowledgeBase -> KnowledgeBase)
  step f curr | next <- immediateConsequence rules curr =
    if curr == next then curr else f next

querySubstitutions :: String -> Program -> [Substitution]
querySubstitutions querySym prog = case atomTerms . ruleHead <$> queryRules of
  [queryVars] -> zip queryVars <$> relevantSyms
  []          -> error "Query symbol does not exist."
  _           -> error "Query symbol has multiple clauses."
 where
  relevantSyms = atomTerms <$> filter ((== querySym) . symbol) (solve prog)
  queryRules   = filter ((== querySym) . symbol . ruleHead) prog
