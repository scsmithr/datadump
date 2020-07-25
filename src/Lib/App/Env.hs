{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.App.Env
  ( Env(..)
  )
where

import           Control.Monad.Reader

data Env = Env
  { envLog :: !(String -> IO ())
  , dataDir :: !FilePath
  }

class HasLog a where
  getLog :: a -> (String -> IO ())

instance HasLog (String -> IO ()) where getLog = id
instance HasLog Env where getLog = envLog

class Has field env where
  obtain :: env -> field

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = error "unimplemented"
