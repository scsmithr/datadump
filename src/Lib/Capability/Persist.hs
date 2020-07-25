module Lib.Capability.Persist
  ( Serialize(..)
  , Persist(..)
  )
where

import           Data.ByteString.Lazy           ( ByteString )

class Serialize a where
  encode :: a -> ByteString
  decode :: ByteString -> Either String a

-- | Capability for persisting a serializable object. Right now will just be for
-- persisting/loading a single data structure. This will need to be extended to
-- support multiple serializable objects.
class Monad m => Persist m where
  persist :: Serialize a => a -> m (Either String ())
  load :: Serialize a => m (Either String a)
