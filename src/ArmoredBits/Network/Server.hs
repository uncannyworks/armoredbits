{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Module      : ArmoredBits.Network.Server
Description : 'Server' network handling

|-}
module ArmoredBits.Network.Server where

--------------------------------------------------------------------------------
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar
import Control.Monad (forever, when)
import Control.Monad.STM (atomically)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import GHC.Generics
import Network.Simple.TCP
import Network.Socket
import System.Clock
import System.IO
--------------------------------------------------------------------------------
import ArmoredBits.Config.Parse
import ArmoredBits.Control.Tasks
import ArmoredBits.Network.Peer
import ArmoredBits.Types
import ArmoredBits.Util
--------------------------------------------------------------------------------

-- $server
--
-- 'Server' runs the game network code, handling incoming peer connections.

-- | 'Server' tracks all incoming 'Peer''s
data Server
  = Server
  {
  -- | Whether the 'Server' is ready to start handling connections or not
    _serverState :: ServerState
  -- | A unique id for new connecting peers
  , _serverIdCounter :: TVar Int
  -- | A map of all 'Peer''s by their unique id
  , _serverGamePeers :: TVar (Map Int (TVar Peer))
  } deriving (Generic)

makeLenses ''Server

-- | Initialize a new 'Server'
mkServer :: IO Server
mkServer = do
  co <- newTVarIO 0
  cs <- newTVarIO Map.empty
  return (Server ServerInitializing co cs)

runServer :: Options -> Server -> IO ()
runServer o s =
  -- TODO: Make these values come from a config
  serve (Host (optionHost o)) (optionPort o) $ \(sock, _) -> do

    -- Convert Socket to Handle
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering

    -- Prepare new Peer
    c1 <- atomically $ do
      modifyTVar' (view serverIdCounter s) (+ 1)
      cid <- readTVar (view serverIdCounter s)
      c0 <- mkPeer cid h
      modifyTVar' (view serverGamePeers s) (Map.insert cid c0)
      return c0
    initPeer 10 h c1 -- 10 msg rate limit

-- | Checks if any 'Peer's have timed out after a specified timeout.
--
-- Flags 'Peer's over timeout value as disconnected and closes their socket.
checkPeers :: Timeout -> Server -> IO ()
checkPeers tout s = forever $ do
  t <- fmap toNanoSecs (getTime Monotonic)
  ps <-atomically $ do
      ps <- readTVar (view serverGamePeers s)
      traverse_ (checkKeepAlive tout t) (Map.elems ps)
      return ps
  traverse_ check ps
  threadDelay $ inSeconds 1
  where
    check p = do
      rp <- readTVarIO p
      when (view peerState rp == PeerDisconnected) $ do
        hClose (view peerHandle rp)
        atomically $
          modifyTVar' (view serverGamePeers s) (Map.delete (view peerId rp))

-- | Initialize and run the 'Server''s main network loop.
initServer :: Options -> Server -> IO ()
initServer o s = runTasks [runServer o s, checkPeers 10 s] cleanup
  where
    -- Perform any final resource cleanup.
    cleanup = do
      putStrLn "Server exited."
      return ()
