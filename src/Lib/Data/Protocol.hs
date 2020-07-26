module Lib.Data.Protocol
  ()
where

import           Data.Word                      ( Word32
                                                , Word8
                                                )
import           Data.Serialize                 ( Serialize(..)
                                                , putWord8
                                                , putWord32be
                                                , getWord8
                                                )
import           Data.Serialize.Get             ( Get(..) )
import           Data.Serialize.Put             ( Put(..) )

data ClientCommand = Ready | Query

clientCommandToWord8 :: ClientCommand -> Word8
clientCommandToWord8 Ready = 0
clientCommandToWord8 Query = 1

clientCommandFromWord8 :: Word8 -> Get ClientCommand
clientCommandFromWord8 0 = pure Ready
clientCommandFromWord8 1 = pure Query
clientCommandFromWord8 w = fail $ "Invalid ClientCommand encoding " ++ show w

instance Serialize ClientCommand where
  put = putWord8 . clientCommandToWord8
  get = clientCommandFromWord8 =<< getWord8

data ServerResponse = QueryResponse | Error

data MessageHeader = Client ClientCommand
                   | Server ServerResponse

data Message a = Message MessageHeader Word32 a

