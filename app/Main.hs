module Main where

import           Lib.Capability.Persist
import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString.Lazy          as B
import           Data.ByteString.Builder       as BU
import           Lib.App.Env                    ( Env(..) )
import           Lib.App.Monad                  ( AppM(..)
                                                , runAppM
                                                )

instance Serialize B.ByteString where
  encode = id
  decode = Right

main :: IO ()
main = do
  let env = Env {envLog = Prelude.putStrLn, envDataDir = "/tmp/datadump/"}
  runAppM env run

run :: AppM ()
run = do
  let s = BU.toLazyByteString $ BU.stringUtf8 "hello world"
  let p = "test-file123"
  _ <- persist s p

  let loader :: AppM (Either String B.ByteString)
      loader = load p
  v <- loader

  liftIO $ Prelude.print v
