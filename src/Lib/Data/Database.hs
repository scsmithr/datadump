{-# LANGUAGE DeriveGeneric #-}

module Lib.Data.Database
  ( Database(..)
  , RelationName(..)
  , Relation(..)
  , Tuple(..)
  , mkEmptyDatabase
  , insertTuple
  )
where

import           GHC.Generics                   ( Generic(..) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Serialize                 ( Serialize )

-- | A tuple representation for a relation.
newtype Tuple a = Tuple [a] deriving (Show, Generic)

instance Serialize a => Serialize (Tuple a)

data RelationName = RelationName String deriving (Show, Eq, Ord, Generic)

instance Serialize RelationName

-- | A named relation containing a list of fixed length tuples.
data Relation a = Relation { relationName :: !RelationName
                           , relationTuples :: [Tuple a]
                           } deriving (Show, Generic)

instance Serialize a => Serialize (Relation a)

-- | Collection of relations.
-- TODO: Monadify
data Database a = Database (Map RelationName (Relation a)) deriving (Show, Generic)

instance Serialize a => Serialize (Database a)

mkEmptyDatabase :: Database a
mkEmptyDatabase = Database M.empty

-- | Insert a tuple into the database. If a relation with that name already
-- exists, it will be updated.
insertTuple
  :: (Serialize a) => Database a -> RelationName -> Tuple a -> Database a
insertTuple (Database relations) name tuple = Database updated
 where
  updated = M.insert name rel relations
  tuples  = case M.lookup name relations of
    Just rel' -> tuple : relationTuples rel'
    Nothing   -> [tuple]
  rel = Relation name tuples

