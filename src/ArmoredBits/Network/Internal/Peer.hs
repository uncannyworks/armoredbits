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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Module      : ArmoredBits.Network.Internal.Peer

|-}
module ArmoredBits.Network.Internal.Peer where

--------------------------------------------------------------------------------
import Control.Concurrent.STM.TVar
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.STM (STM, atomically)
import Data.List (foldl')
import GHC.Generics
import Lens.Micro.Platform
import System.Clock
import System.IO
--------------------------------------------------------------------------------
import ArmoredBits.Network.Messages
import ArmoredBits.Types
--------------------------------------------------------------------------------
import Debug.Trace

data PeerEnv
  = PeerEnv
  {
  -- | Message rate limit
    _peerEnvRateLimit :: RateLimit
  -- | Message rate rest speed in milliseconds
  , _peerEnvRateReset :: Int
  -- | List of valid tokens to match against
  , _peerEnvTokens :: [Token]
  } deriving (Generic)

makeLenses ''PeerEnv

data Peer
  = Peer
  {
  -- | 'Server' assigned id
    _peerId :: PeerId
  -- | Socket converted to a Handle
  , _peerHandle :: Handle
  -- | Current state of 'Peer'
  , _peerState :: PeerState
  -- | Token user has passed to identify self
  , _peerToken :: Maybe Token
  -- | Time in nanoseconds since last pong from 'Peer'
  , _peerLastPongTime :: Integer
  -- | Time in nanoseconds since last message received
  , _peerLastMsgTime :: Integer
  -- | Peer message rate state
  , _peerMsgRate :: Rate
  -- | Total messages since last rate check
  , _peerMsgCount :: Int
  -- | Incoming peer 'ClientEvent' messages to be read by the 'Server'
  , _peerEventQueue :: [ClientEvent]
  } deriving (Generic)

makeLenses ''Peer

-- | Initialize a new 'Peer' with a unique 'PeerId' and socket handle.
--
-- This occurs in the 'STM' monad to give the caller a chance to atomically
-- wrap other actions in the same transaction.
mkPeer :: PeerId -> Handle -> STM (TVar Peer)
mkPeer pid h = newTVar (Peer pid h PeerConnected Nothing 0 0 Good 0 [])

resetRate' :: TVar Peer -> STM ()
resetRate' p = modifyTVar' p (set peerMsgCount 0 . set peerMsgRate Good)

disconnectPeer :: TVar Peer -> STM ()
disconnectPeer p = modifyTVar' p (set peerState PeerDisconnected)

pingPeer' :: Handle -> IO ()
pingPeer' h = serverSend h (MessageServer Ping)

-- | Processes incoming messages and updates the 'Peer'.
processMessages :: (Monad m, MonadReader PeerEnv m)
  => Integer -> CMessage -> Peer -> m (Either (ServerMessage, Peer) Peer)
processMessages t msg p = do
  r <- asks (view peerEnvRateLimit)
  let pu = updateMsgRate r . checkBadPeer r $ p
  case view peerMsgRate pu of
    Bad  -> return (Left (Warning, pu))
    Good ->
      case msg of
        (MessageClient m) -> processClientMessage t m pu
        (EventClient m) ->
          case view peerToken pu of
            Nothing -> return (Left (InvalidToken, pu))
            Just _  -> return (Right (processClientEvent t m pu))
        _ -> return (Left (MessageUnknown, pu))

processClientMessage :: (Monad m, MonadReader PeerEnv m)
  => Integer
  -> ClientMessage
  -> Peer
  -> m (Either (ServerMessage, Peer) Peer)
processClientMessage t msg p =
  case msg of
    -- Peer is alive and Pong'ed the server
    Pong -> return (Right (set peerLastPongTime t p))
    -- Peer signalled a disconnect
    Disconnect -> return (Right (set peerState PeerDisconnected p))
    -- Peer sent a token
    -- Must be in valid list of server tokens
    (SendToken tk) -> updateToken tk p
    -- Peer sent unknown message
    _ -> return (Left (MessageUnknown, p))

updateToken :: (Monad m, MonadReader PeerEnv m)
  => Token -> Peer -> m (Either (ServerMessage, Peer) Peer)
updateToken t p = do
  ts <- asks (view peerEnvTokens)
  if t `elem` ts
  then return (Right (set peerState PeerAuthenticated . set peerToken (Just t) $ p))
  else return (Left (InvalidToken, p))

-- | 'Peer' received a relevant message, update internal state.
processClientEvent :: Integer -> ClientEvent -> Peer -> Peer
processClientEvent t msg =
  over peerEventQueue (msg :) .
  set peerLastMsgTime t .
  over peerMsgCount (+ 1)

updateMsgRate :: RateLimit -> Peer -> Peer
updateMsgRate r p
  | view peerMsgCount p > r = set peerMsgRate Bad p
  | otherwise = set peerMsgRate Good p

-- | If a 'Peer' sends nothing but CUnknown it is misbehaving and should be disconnected.
checkBadPeer :: RateLimit -> Peer -> Peer
checkBadPeer r p
  | r == chk  = set peerState PeerDisconnected p
  | otherwise = p
  where
    f c ClientEventUnknown = c + 1
    f c _                  = c
    chk = foldl' f 0 (view peerEventQueue p)

runPeer' :: PeerEnv -> Handle -> TVar Peer -> IO ()
runPeer' e h p = do
  serverRecv h >>= \case
    Left err -> do
      traceIO $ show err
      return () -- TODO: Log failed messages?
    Right msg -> do
      traceIO $ show msg
      t <- fmap toNanoSecs (getTime Monotonic)
      out <- atomically $ do
        rp <- readTVar p
        v <- runReaderT (processMessages t msg rp) e
        case v of
          Right wp     -> writeTVar p wp >> return Nothing
          Left (m, wp) -> writeTVar p wp >> return (Just m)
      case out of
        Nothing -> return ()
        Just m  -> serverSend h (MessageServer m)
