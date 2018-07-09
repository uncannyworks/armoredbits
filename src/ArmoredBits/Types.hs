{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|

Module      : Types
Description : General project wide type definitions

|-}
module ArmoredBits.Types where

--------------------------------------------------------------------------------
import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Data.Tagged
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------

-- $messages

-- | 'Token' tag type
data TokenTag
-- | A 'Token' is tied to a user as a validation mechanism
--
-- Only tokens from clients the server is aware of are allowed to join
type Token = Tagged TokenTag Text

-- | 'Token' constructor
mkToken :: Text -> Token
mkToken = Tagged

-- | 'Serialize' instance for 'Token'
instance Serialise Token where
  encode = encode . untag
  decode = mkToken <$> decode

-- | Whether the connecting 'Client' passed a valid 'Token' or not
data LoginState
  = LoginSuccessful
  | LoginFailed
  deriving (Eq, Enum, Generic, Show)

-- | 'Serialize' instance for 'LoginState'
instance Serialise LoginState where
  encode = encodeInt . fromEnum
  decode = toEnum <$> decodeInt

-- | Whether the 'Client' has gone over the message rate or not
data Rate
  = Good
  | Bad
  deriving (Eq, Show)

-- $client

-- | The current state the 'Client' is in
data ClientState
  = ClientConnected
  | ClientAuthenticated
  | ClientNeedsInitialState
  | ClientProcessing Rate
  | ClientDisconnected
  deriving (Eq, Show)

-- $server

-- | The current state the 'Server' is in
data ServerState
  = ServerInitializing
  | ServerReady
  | ServerShuttingDown
  deriving (Eq, Enum, Generic, Show)

-- | 'Serialize' instance for 'ServerState'
instance Serialise ServerState where
  encode = encodeInt . fromEnum
  decode = toEnum <$> decodeInt

-- $world

-- | The current state the 'World' is in
data WorldState
  = WorldInitializing
  | ConfigurationPhase
  | StartupPhase
  | GamePhase
  | GameOverPhase
  | StartNewRound
  deriving (Eq, Enum, Generic, Show)

-- | 'Serialize' instance for 'WorldState'
instance Serialise WorldState where
  encode = encodeInt . fromEnum
  decode = toEnum <$> decodeInt
