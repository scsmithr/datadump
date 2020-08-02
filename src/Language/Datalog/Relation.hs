module Language.Datalog.Relation
  ( Tuple(..)
  , Relation
  , Variable
  , fromTuples
  , shiftRecent
  , emptyRecent
  , unionRelation
  , emptyRelation
  , insertTuple
  , joinTuples
  , joinRelations
  , joinVariables
  , variableFromRelations
  , consume
  )
where

import           Control.Monad                  ( liftM2 )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S

-- | A tuple representation for a relation.
newtype Tuple a = Tuple [a] deriving (Show, Eq, Ord)

-- | A relation containing a list of fixed length tuples.
data Relation a = Relation (Set (Tuple a)) deriving (Show, Eq)

emptyRelation :: Relation a
emptyRelation = Relation S.empty

-- | Insert a tuple into the given relation.
-- TODO: Enforce length.
insertTuple :: (Ord a) => Relation a -> Tuple a -> Relation a
insertTuple (Relation tups) tup = Relation $ S.insert tup tups

-- | Helper for creating a stable relation from a list of tuples.
fromTuples :: (Ord a) => [Tuple a] -> Relation a
fromTuples [] = emptyRelation
fromTuples ts = Relation $ S.fromList ts

-- | Union two relations, preferring the values from the first relation.
unionRelation :: (Ord a) => Relation a -> Relation a -> Relation a
unionRelation (Relation t1) (Relation t2) = Relation $ S.union t1 t2

-- | Join together two tuples on key equality. The resulting tuple will be the
-- result of the provided logic function.
joinTuples
  :: (Eq a) => (b -> c -> d) -> Tuple (a, b) -> Tuple (a, c) -> Tuple (a, d)
joinTuples logic (Tuple l1) (Tuple l2) = Tuple $ map flatten filtered
 where
  cart     = cartesianProd l1 l2
  filtered = filter (\(p1, p2) -> fst p1 == fst p2) cart

  flatten (f, s) = (fst f, logic (snd f) (snd s))

-- | Join two relations on key equality, producing a new relation with the value
-- being the result of the logic function.
joinRelations
  :: (Ord a, Ord d)
  => (b -> c -> d)
  -> Relation (a, b)
  -> Relation (a, c)
  -> Relation (a, d)
joinRelations logic (Relation s1) (Relation s2) = fromTuples fts
 where
  cart = cartesianProd (S.elems s1) (S.elems s2)
  ts   = map (uncurry $ joinTuples logic) cart
  fts  = filter (/= Tuple []) ts

data Variable a = Variable
                  { vrStable :: [Relation a]
                  -- ^ List of processed tuples.
                  , vrRecent :: Relation a
                  -- ^ Admitted tuples, needs to be reconsidered by all rules.
                  , vrNext :: [Relation a]
                  -- ^ Relations that have yet to be admitted.
                  } deriving (Show)

-- | Create a new variable relation from a list of stable relations. No
-- relations will be processed.
variableFromRelations :: [Relation a] -> Variable a
variableFromRelations rels =
  Variable {vrStable = [], vrRecent = emptyRelation, vrNext = rels}

shiftRecent :: Variable a -> Variable a
shiftRecent = pullNext . mergeRecent
 where
  mergeRecent v@(Variable s r _) =
    v { vrStable = s <> [r], vrRecent = emptyRelation }

  pullNext v@(Variable _ _ []      ) = v
  pullNext v@(Variable _ _ (x : xs)) = v { vrRecent = x, vrNext = xs }

emptyRecent :: (Eq a) => Variable a -> Bool
emptyRecent (Variable _ r _) = r == emptyRelation

-- | Join two variabled relations together on key equality. The resulting
-- relation variable relation will have a value determined by the logic
-- function.
--
-- The variable relation will have no stable or recent relations.
joinVariables
  :: (Ord a, Ord d)
  => (b -> c -> d)
  -> Variable (a, b)
  -> Variable (a, c)
  -> Variable (a, d)
joinVariables logic (Variable s1 r1 _) (Variable s2 r2 _) = Variable
  { vrStable = []
  , vrRecent = emptyRelation
  , vrNext   = unprocessed
  }
 where
  b1          = map ((flip $ joinRelations logic) r2) s1
  b2          = map (joinRelations logic r1) s2
  r3          = joinRelations logic r1 r2
  unprocessed = b1 <> b2 <> [r3]

-- | Consume the fully processed variable relation, and return a stable
-- relation.
consume :: (Ord a) => Variable a -> Either String (Relation a)
consume v@(Variable st _ next)
  | not (null next)     = Left "unprocessed relations"
  | not (emptyRecent v) = Left "non-empty recent"
  | otherwise           = Right $ foldr unionRelation emptyRelation st

-- | Cartesian product of two lists.
cartesianProd :: [a] -> [b] -> [(a, b)]
cartesianProd = liftM2 (,)
