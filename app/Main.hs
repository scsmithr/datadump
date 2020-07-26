module Main where

import           Lib.Capability.Persist
import           Control.Monad.IO.Class         ( liftIO )
import           Data.ByteString.Lazy          as B
import           Data.ByteString.Builder       as BU
import           Lib.Data.Database
import           Lib.App.Env                    ( Env(..) )
import           Lib.App.Monad                  ( AppM(..)
                                                , runAppM
                                                )

main :: IO ()
main = do
  let env = Env {envLog = Prelude.putStrLn, envDataDir = "/tmp/datadump/"}
  runAppM env run

run :: AppM ()
run = do
  let db :: Database Int
      db = insertTuple mkEmptyDatabase (RelationName "test") [0, 1, 2, 3, 4]
  let p = "test-file123"
  _ <- persist db p

  let loader :: AppM (Either String (Database Int))
      loader = load p
  v <- loader

  liftIO $ Prelude.print v
