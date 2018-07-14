{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Module      : ArmoredBits.Network.Internal.Peer

|-}
module ArmoredBits.Network.Internal.Peer where

--------------------------------------------------------------------------------
import Control.Concurrent.STM.TVar
import Control.Monad.STM (STM, atomically)
import GHC.Generics
import Lens.Micro.Platform
import System.Clock
import System.IO
--------------------------------------------------------------------------------
import ArmoredBits.Network.Messages
import ArmoredBits.Types
--------------------------------------------------------------------------------
import Debug.Trace

data Peer
  = Peer
  {
  -- | 'Server' assigned id
    _peerId :: Int
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

-- | Initialize a new 'Peer' with a unique id and socket handle.
--
-- This occurs in the 'STM' monad to give the caller a chance to atomically
-- wrap other actions in the same transaction.
mkPeer :: Int -> Handle -> STM (TVar Peer)
mkPeer cid h =
  newTVar (Peer cid h PeerConnected "" 0 0 Good 0 [])

resetRate' :: TVar Peer -> STM ()
resetRate' p = modifyTVar' p (set peerMsgCount 0)

disconnectPeer :: TVar Peer -> STM ()
disconnectPeer p = modifyTVar' p (set peerState PeerDisconnected)

pingPeer' :: Handle -> IO ()
pingPeer' h = serverSend h Ping

-- | Processes incoming messages and updates the 'Peer'.
processMessages :: RateLimit -> Integer -> CMessage -> Peer -> Peer
processMessages r t msg p =
  case msg of
    -- Peer is alive and Pong'ed the server
    Pong -> set peerLastPongTime t p
    -- Peer sent a relevant message
    _ -> updateMessageQueue r t msg p

-- | 'Peer' received a relevant message, update internal state.
updateMessageQueue :: RateLimit -> Integer -> CMessage -> Peer -> Peer
updateMessageQueue r t msg =
  rate . over peerMsgQueue (msg :) . set peerLastMsgTime t . over peerMsgCount (+ 1)
  where
    rate p =
      if view peerMsgCount p > r
      then set peerMsgRate Bad p
      else set peerMsgRate Good p

runPeer' :: RateLimit -> Handle -> TVar Peer -> IO ()
runPeer' r h p = do
  serverRecv h >>= \case
    Left e    -> do
      traceIO $ show e
      return () -- TODO: Log failed messages?
    Right msg -> do
      traceIO $ show msg
      t <- fmap toNanoSecs (getTime Monotonic)
      s <- atomically $ do
        rp <- readTVar p
        let v = processMessages r t msg rp
        writeTVar p v
        return (view peerMsgRate v)
      case s of
        Good -> return ()
        Bad  -> serverSend h Warning
