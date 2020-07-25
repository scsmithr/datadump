module Main where

import           Lib.Capability.Persist
import           Data.ByteString.Lazy          as B
import           Data.ByteString.Builder       as BU

instance Serialize B.ByteString where
  encode = id
  decode = Right

instance Persist IO where
  persist a = do
    B.putStrLn $ encode a
    pure $ Right ()

  load = do
    pure $ Left "error"

main :: IO ()
main = do
  let s = BU.toLazyByteString $ BU.stringUtf8 "hello world"
  persist s
  pure ()
