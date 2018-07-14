{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|

Module      : ArmoredBits.Network.Messages
Description : CBOR message encoding/decoding and parsing

|-}
module ArmoredBits.Network.Messages
  ( CMessage(..)
  , SMessage(..)
  , clientSend
  , clientRecv
  , serverSend
  , serverRecv
  ) where
  
--------------------------------------------------------------------------------
import Codec.CBOR.Read
import Codec.Serialise (hPutSerialise)
import Codec.Serialise.Class (Serialise, decode, encode)
import Codec.Serialise.Decoding (Decoder, decodeListLen, decodeWord)
import Codec.Serialise.Encoding (Encoding, encodeListLen, encodeWord)
import Control.Monad.ST.Strict
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import GHC.Generics
import System.IO
--------------------------------------------------------------------------------

-- | Messages which can be sent by a 'Server'.
--
-- Internal 'Server' message ids are odd.
data SMessage
  = Ping
  | Warning
  | SUnknown
  deriving (Eq, Generic, Show)

instance Serialise SMessage where
  encode = encodeServerMessage
  decode = decodeServerMessage

encodeServerMessage :: SMessage -> Encoding
encodeServerMessage Ping =
  encodeListLen 1 <> encodeWord 1
encodeServerMessage Warning =
  encodeListLen 1 <> encodeWord 3
encodeServerMessage SUnknown = mempty

decodeServerMessage :: Decoder s SMessage
decodeServerMessage = do
  len <- decodeListLen
  tag <- decodeWord
  case (len, tag) of
    (1, 1) -> pure Ping
    (1, 3) -> pure Warning
    _      -> pure SUnknown

-- | Messages which can be sent by a 'Client'.
--
-- Internal 'Client' message ids are even.
data CMessage
  = Pong
  | Disconnect
  | CUnknown
  deriving (Eq, Generic, Show)

instance Serialise CMessage where
  encode = encodeClientMessage
  decode = decodeClientMessage

encodeClientMessage :: CMessage -> Encoding
encodeClientMessage Pong =
  encodeListLen 1 <> encodeWord 2
encodeClientMessage Disconnect =
  encodeListLen 1 <> encodeWord 4
encodeClientMessage CUnknown = mempty

decodeClientMessage :: Decoder s CMessage
decodeClientMessage = do
  len <- decodeListLen
  tag <- decodeWord
  case (len, tag) of
    (1, 2) -> pure Pong
    (1, 4) -> pure Disconnect
    _      -> pure CUnknown

-- | Send a 'CMessage' from a client.
clientSend :: Handle -> CMessage -> IO ()
clientSend = writeMessage

-- | Receive a 'SMessage' from a server.
clientRecv :: Handle -> IO (Either String SMessage)
clientRecv = readMessage

-- | Send a 'SMessage' from a server.
serverSend :: Handle -> SMessage -> IO ()
serverSend = writeMessage

-- | Receive a 'CMessage' from a client.
serverRecv :: Handle -> IO (Either String CMessage)
serverRecv = readMessage

--------------------------------------------------------------------------------

-- | Incrementally stream message data from a 'Handle'
--
-- See: https://github.com/well-typed/cborg/issues/156
readMessage :: Serialise a => Handle -> IO (Either String a)
readMessage h = do
  s <- stToIO $ deserialiseIncremental decode
  streamIt h s

streamIt :: Handle -> IDecode RealWorld a -> IO (Either String a)
streamIt h (Partial k) = do
  dec <- BS.hGet h 1 >>= stToIO . k . Just
  streamIt h dec
streamIt _h (Done _ _ a) = return (Right a)
streamIt _h (Fail _ _ e) = return (Left (show e))

-- | Write a 'Message' to the socket 'Handle'
writeMessage :: Serialise a => Handle -> a -> IO ()
writeMessage = hPutSerialise
