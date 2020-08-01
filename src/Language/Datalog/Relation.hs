{-# LANGUAGE DeriveGeneric #-}

module Language.Datalog.Relation
  ()
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

-- | Union two relations, preferring the values from the first relation.
unionRelation :: (Ord a) => Relation a -> Relation a -> Relation a
unionRelation (Relation t1) (Relation t2) = Relation $ S.union t1 t2

-- | Join together two tuples on key equality. The resulting tuple will be a
-- combination of values from the first two.
joinTuplesOnKeys :: (Eq a) => Tuple (a, b) -> Tuple (a, c) -> Tuple (a, b, c)
joinTuplesOnKeys (Tuple l1) (Tuple l2) = Tuple $ map flatten filtered
 where
  cart     = cartesianProd l1 l2
  filtered = filter (\(p1, p2) -> fst p1 == fst p2) cart

  flatten :: ((a, b), (a, c)) -> (a, b, c)
  flatten (f, s) = (fst f, snd f, snd s)

-- | Join two relations together on key equality.
joinRelationsOnKeys
  :: (Ord a, Ord b, Ord c)
  => Relation (a, b)
  -> Relation (a, c)
  -> Relation (a, b, c)
joinRelationsOnKeys (Relation s1) (Relation s2) = Relation $ S.fromList ts
 where
  cart = cartesianProd (S.elems s1) (S.elems s2)
  ts   = map (uncurry joinTuplesOnKeys) cart

data VariableRelation a = VariableRelation
                          { vrStable :: [Relation a]
                          -- ^ List of processed tuples.
                          , vrRecent :: Relation a
                          -- ^ Admitted tuples, needs to be reconsidered by all
                          -- rules.
                          , vrNext :: [Relation a]
                          -- ^ Relations that have yet to be admitted.
                          } deriving (Show)

shiftRecent :: VariableRelation a -> VariableRelation a
shiftRecent = pullNext . mergeRecent
 where
  mergeRecent v@(VariableRelation s r _) =
    v { vrStable = s <> [r], vrRecent = emptyRelation }

  pullNext v@(VariableRelation _ _ []      ) = v
  pullNext v@(VariableRelation _ _ (x : xs)) = v { vrRecent = x, vrNext = xs }

emptyRecent :: (Eq a) => VariableRelation a -> Bool
emptyRecent (VariableRelation _ r _) = r == emptyRelation

joinInto
  :: (Ord a, Ord b, Ord c)
  => VariableRelation (a, b)
  -> VariableRelation (a, c)
  -> Relation (a, b, c)
joinInto (VariableRelation s1 r1 _) (VariableRelation s2 r2 _) = res
 where
  b1  = map (`joinRelationsOnKeys` r2) s1
  b2  = map (joinRelationsOnKeys r1) s2
  r3  = joinRelationsOnKeys r1 r2
  res = foldr unionRelation emptyRelation $ b1 <> b2 <> [r3]

-- | Cartesian product of two lists.
cartesianProd :: [a] -> [b] -> [(a, b)]
cartesianProd = liftM2 (,)
