{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

{-|

Module      : Messages
Description : CBOR message encoding/decoding and parsing

|-}
module ArmoredBits.Messages
  ( Message(..)
  , stream
  , writeMessage
  ) where
  
--------------------------------------------------------------------------------
import Codec.CBOR.Read
import Codec.Serialise hiding (Done, Fail, deserialiseIncremental)
import Codec.Serialise.Decoding hiding (Done, Fail)
import Codec.Serialise.Encoding
import Control.Monad.ST.Strict
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import GHC.Generics
import System.IO
--------------------------------------------------------------------------------
import ArmoredBits.Types
--------------------------------------------------------------------------------

-- | Messages which get passed between the 'Server' and 'Client'
--
-- Server messages are odd, client messages are even
data Message
  -- Utility: 1-9
  = Ping
  | Pong
  | Warning
  -- Auth: 50-149
  | LoginReq { token :: Token }
  | LoginRes { code :: LoginState }
  -- Game: 150-256
  | States { serverState :: ServerState, worldState :: WorldState }
  -- Unknown
  | Unknown
  deriving (Generic)

instance Serialise Message where
  encode = encodeMessage
  decode = decodeMessage

encodeMessage :: Message -> Encoding
encodeMessage Ping =
  encodeListLen 1 <> encodeWord 1
encodeMessage Pong =
  encodeListLen 1 <> encodeWord 2
encodeMessage Warning =
  encodeListLen 1 <> encodeWord 3
encodeMessage (LoginReq t) =
  encodeListLen 2 <> encodeWord 50 <> encode t
encodeMessage (LoginRes c) =
  encodeListLen 2 <> encodeWord 51 <> encode c
encodeMessage (States ss ws) =
  encodeListLen 3 <> encodeWord 151 <> encode ss <> encode ws
encodeMessage Unknown = mempty

decodeMessage :: Decoder s Message
decodeMessage = do
  len <- decodeListLen
  tag <- decodeWord
  case (len, tag) of
    (1, 1)   -> pure Ping
    (1, 2)   -> pure Pong
    (1, 3)   -> pure Warning
    (2, 50)  -> LoginReq <$> decode
    (2, 51)  -> LoginRes <$> decode
    (3, 151) -> States <$> decode <*> decode
    _        -> pure Unknown

-- | Incrementally stream message data from a handle
--
-- See: https://github.com/well-typed/cborg/issues/156
stream :: Handle -> IO (Either String Message)
stream h = do
  s <- stToIO $ deserialiseIncremental (decode @Message)
  streamIt h s

streamIt :: Handle -> IDecode RealWorld a -> IO (Either String a)
streamIt h (Partial k) = do
  dec <- BS.hGet h 16 >>= stToIO . k . Just
  streamIt h dec
streamIt _h (Done _ _ a) = return (Right a)
streamIt _h (Fail _ _ e) = return (Left (show e))

-- | Write a 'Message' to the socket 'Handle'
writeMessage :: Handle -> Message -> IO ()
writeMessage = hPutSerialise
