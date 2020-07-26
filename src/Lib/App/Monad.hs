{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.App.Monad
  ( AppM(..)
  , runAppM
  )
where

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy          as B
import           Lib.App.Env                    ( Env(..) )
import           Lib.Capability.Persist         ( Persist(..)
                                                , Serialize(..)
                                                , PersistId
                                                )
import           System.FilePath.Posix          ( combine )

newtype AppM a = AppM (ReaderT Env IO a)
               deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runAppM :: Env -> AppM a -> IO a
runAppM env (AppM m) = runReaderT m env

instance Persist AppM where
  persist a p = do
    env <- ask
    liftIO $ writeToDataDir (envDataDir env) a p

  load p = do
    env <- ask
    liftIO $ readFromDataDir (envDataDir env) p

-- | Writes a serializable object to the data directory.
-- TODO: Create directory if doesn't exist.
-- TODO: Catch error.
writeToDataDir
  :: Serialize a => FilePath -> a -> PersistId -> IO (Either String ())
writeToDataDir dir obj p = do
  let path = combine dir p
  let bs   = encode obj
  B.writeFile path bs
  pure $ Right ()

-- | Reads a deserializable object from the data directory.
-- TODO: Catch error.
readFromDataDir :: Serialize a => FilePath -> PersistId -> IO (Either String a)
readFromDataDir dir p = do
  let path = combine dir p
  bs <- B.readFile path
  pure $ decode bs
