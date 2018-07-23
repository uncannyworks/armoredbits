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
  , _peerToken :: Token
  -- | Time in nanoseconds since last pong from 'Peer'
  , _peerLastPongTime :: Integer
  -- | Time in nanoseconds since last message received
  , _peerLastMsgTime :: Integer
  -- | Peer message rate state
  , _peerMsgRate :: Rate
  -- | Total messages since last rate check
  , _peerMsgCount :: Int
  -- | Incoming peer 'CMessage's to be read by the 'Server'
  , _peerMsgQueue :: [CMessage]
  } deriving (Generic)

makeLenses ''Peer

-- | Initialize a new 'Peer' with a unique 'PeerId' and socket handle.
--
-- This occurs in the 'STM' monad to give the caller a chance to atomically
-- wrap other actions in the same transaction.
mkPeer :: PeerId -> Handle -> STM (TVar Peer)
mkPeer pid h = newTVar (Peer pid h PeerConnected "" 0 0 Good 0 [])

resetRate' :: TVar Peer -> STM ()
resetRate' p = modifyTVar' p (set peerMsgCount 0 . set peerMsgRate Good)

disconnectPeer :: TVar Peer -> STM ()
disconnectPeer p = modifyTVar' p (set peerState PeerDisconnected)

pingPeer' :: Handle -> IO ()
pingPeer' h = serverSend h Ping

-- | Processes incoming messages and updates the 'Peer'.
processMessages :: (Monad m, MonadReader PeerEnv m)
  => Integer -> CMessage -> Peer -> m (Either (SMessage, Peer) Peer)
processMessages t msg p = do
  r <- asks (view peerEnvRateLimit)
  let pu = updateMsgRate r . checkBadPeer r $ p
  case view peerMsgRate pu of
    Bad  -> return (Left (Warning, pu))
    Good ->
      case msg of
        -- Peer is alive and Pong'ed the server
        Pong -> return (Right (set peerLastPongTime t pu))
        -- Peer sent a token
        -- Must be in valid list of server tokens
        (SendToken tk) -> updateToken tk pu
        -- Peer sent a relevant message
        _ -> return (Right (updateMessageQueue t msg pu))

updateToken :: (Monad m, MonadReader PeerEnv m)
  => Token -> Peer -> m (Either (SMessage, Peer) Peer)
updateToken t p = do
  ts <- asks (view peerEnvTokens)
  if t `elem` ts
  then return (Right (set peerToken t p))
  else return (Left (InvalidToken, p))

-- | 'Peer' received a relevant message, update internal state.
updateMessageQueue :: Integer -> CMessage -> Peer -> Peer
updateMessageQueue t msg =
  over peerMsgQueue (msg :) .
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
    f c CUnknown = c + 1
    f c _        = c
    chk = foldl' f 0 (view peerMsgQueue p)

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
        Just m  -> serverSend h m
