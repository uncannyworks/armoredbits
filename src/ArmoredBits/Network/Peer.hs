{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Module      : ArmoredBits.Network.Peer
Description : 'Peer' network handling

|-}
module ArmoredBits.Network.Peer
  ( checkKeepAlive
  , initPeer
  -- * re-exports
  , PeerEnv(..)
  , Peer(..)
  , peerId
  , peerHandle
  , peerState
  , peerToken
  , peerLastPongTime
  , peerLastMsgTime
  , peerMsgRate
  , peerMsgCount
  , peerMsgQueue
  , mkPeer
  ) where

--------------------------------------------------------------------------------
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar
import Control.Monad (forever, when)
import Control.Monad.STM (STM, atomically)
import Lens.Micro.Platform
import System.IO
--------------------------------------------------------------------------------
import ArmoredBits.Control.Tasks
import ArmoredBits.Types
import ArmoredBits.Util
--------------------------------------------------------------------------------
import ArmoredBits.Network.Internal.Peer
--------------------------------------------------------------------------------
import Debug.Trace

-- $peer

-- | Update 'Peer' based on last ping response.
checkKeepAlive :: Timeout -> Integer -> TVar Peer -> STM ()
checkKeepAlive tout t p = do
  rp <- readTVar p
  when (t - view peerLastPongTime rp > toInteger (inSeconds tout)) $
    modifyTVar' p (set peerState PeerDisconnected)

-- | 'Server' side check for aliveness.
pingPeer :: Handle -> IO ()
pingPeer h = forever $ do
  pingPeer' h
  traceIO "PING"
  threadDelay $ inSeconds 5

-- | Resets the 'Peer' message count every 100ms.
--
-- If the 'Peer' goes over the rate they are sent a `Warning`
-- and blocked from sending new messages until the next tick.
resetRate :: TVar Peer -> IO ()
resetRate  p = forever $ do
  atomically $ resetRate' p
  threadDelay $ inMilliseconds 100

-- | The actual network loop which reads messages from the 'Handle'.
runPeer :: PeerEnv -> Handle -> TVar Peer -> IO ()
runPeer e h p = forever $ do
  traceIO "run"
  runPeer' e h p

-- | Initialize and run the 'Peer''s various threads for message handling.
--
-- This includes a ping check, rate check, and actual message parsing.
initPeer :: PeerEnv -> Handle -> TVar Peer -> IO ()
initPeer e h p = runTasks [pingPeer h, runPeer e h p, resetRate p] cleanup
  where
    -- Peer cleanup
    cleanup = do
      atomically $ disconnectPeer p
      hClose h
