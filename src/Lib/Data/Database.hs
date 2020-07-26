{-# LANGUAGE DeriveGeneric #-}

module Lib.Data.Database
  ( Database(..)
  , RelationName(..)
  , Relation(..)
  , mkEmptyDatabase
  , insertTuple
  )
where

import           GHC.Generics                   ( Generic(..) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import qualified Data.Serialize                as C
import           Lib.Capability.Persist         ( Serialize(..) )

data RelationName = RelationName String deriving (Show, Eq, Ord, Generic)

instance C.Serialize RelationName

-- | A named relation containing a list of fixed length tuples.
data Relation a = Relation { relationName :: !RelationName
                           , relationTuples :: [[a]]
                           } deriving (Show, Generic)

instance C.Serialize a => C.Serialize (Relation a)

-- | Collection of relations.
-- TODO: Monadify
data Database a = Database (Map RelationName (Relation a)) deriving (Generic)

instance C.Serialize a => C.Serialize (Database a)

-- TODO: Maybe use cereal's serialize for everything instead of having this
-- separate type class.
instance C.Serialize a => Serialize (Database a) where
  encode = C.encodeLazy
  decode = C.decodeLazy

mkEmptyDatabase :: Database a
mkEmptyDatabase = Database M.empty

-- | Insert a tuple into the database. If a relation with that name already
-- exists, it will be updated.
insertTuple :: Database a -> RelationName -> [a] -> Database a
insertTuple (Database relations) name tuple = Database updated
 where
  updated = M.insert name rel relations
  tuples  = case M.lookup name relations of
    Just rel' -> tuple : relationTuples rel'
    Nothing   -> [tuple]
  rel = Relation name tuples

