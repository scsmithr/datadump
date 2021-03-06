module Lib.Capability.Persist
  ( Serialize(..)
  , Persist(..)
  , PersistId
  )
where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Serialize                 ( Serialize )

-- | An identifier for persistent data.
type PersistId = String

-- | Capability for persisting a serializable object. Right now will just be for
-- persisting/loading a single data structure. This will need to be extended to
-- support multiple serializable objects.
class Monad m => Persist m where
  persist :: Serialize a => a -> PersistId -> m (Either String ())
  load :: Serialize a => PersistId -> m (Either String a)
