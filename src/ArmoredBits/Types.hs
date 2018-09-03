{-
    Copyright (C) 2014-2018 Uncanny Works

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

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
import Data.Monoid ((<>))
import Data.Tagged
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (decodeFloat, encodeFloat)
--------------------------------------------------------------------------------

-- $util

type Timeout = Int
type RateLimit = Int

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

-- | Whether the connecting 'Peer' passed a valid 'Token' or not
data LoginState
  = LoginSuccessful
  | LoginFailed
  deriving (Eq, Enum, Generic, Show)

-- | 'Serialize' instance for 'LoginState'
instance Serialise LoginState where
  encode = encodeInt . fromEnum
  decode = toEnum <$> decodeInt

-- | Whether the 'Peer' has gone over the message rate or not
data Rate
  = Good
  | Bad
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- $client

-- | 'PeerId' tag type
data PeerIdTag
-- | A 'PeerId' is a 'Server' assigned identifier
type PeerId = Tagged PeerIdTag Int

-- | 'PeerId' constructor
mkPeerId :: Int-> PeerId
mkPeerId = Tagged

-- | The current state the 'Peer' is in
data PeerState
  = PeerConnected
  | PeerAuthenticated
  | PeerDisconnected
  deriving (Eq, Show)

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- $game

data V2 = V2 Float Float deriving (Eq, Show)

instance Serialise V2 where
  encode (V2 a b) = encodeFloat a <> encodeFloat b
  decode = V2 <$> decodeFloat <*> decodeFloat

type Position = V2
type Rotation = V2

-- | 'EntityId' tag type
data EntityIdTag
-- | A 'EntityId' is a 'World' assigned identifier
type EntityId = Tagged EntityIdTag Int

-- | 'EntityId' constructor
mkEntityId :: Int-> EntityId
mkEntityId = Tagged

-- | 'Serialize' instance for 'EntityId
instance Serialise EntityId where
  encode = encode . untag
  decode = mkEntityId <$> decode

