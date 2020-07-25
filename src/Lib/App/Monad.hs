{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.App.Monad
  ( AppM(..)
  )
where

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy          as B
import           Lib.App.Env                    ( Env(..) )
import           Lib.Capability.Persist         ( Persist(..)
                                                , Serialize(..)
                                                )
import           System.FilePath.Posix          ( combine )

newtype AppM a = AppM (ReaderT Env IO a)
               deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

instance Persist AppM where
  persist a = do
    env <- ask
    liftIO $ writeToDataDir (dataDir env) a

  load = pure $ Left "unimplemented"

-- | Writes a serializable object to the data directory.
writeToDataDir :: Serialize a => FilePath -> a -> IO (Either String ())
writeToDataDir dir obj = do
  let path = combine dir "test_file" -- TODO
  let bs   = encode obj
  B.writeFile path bs
  pure $ Right ()
