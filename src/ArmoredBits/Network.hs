{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

Module      : Network
Description : 'Server' and 'Client' network handling

|-}
module ArmoredBits.Network
  (
  -- * Client
  -- $client
    Client(..)
  , clientMsgQueue
  -- * Server
  -- $server
  , Server(..)
  , initServer
  , runServer
  ) where
    
--------------------------------------------------------------------------------
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM.TVar
import Control.Monad (forever, void, when)
import Control.Monad.STM (STM, atomically)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import Network.Simple.TCP
import Network.Socket
import System.Clock
import System.IO
--------------------------------------------------------------------------------
import ArmoredBits.Messages
import ArmoredBits.Types
--------------------------------------------------------------------------------

-- $client
--
-- 'Client' messages get ignored if more than 10 messages every 100ms occurs.
--
-- Once their message queue is cleared by the game loop they will start
-- receiving again.

-- | 'Client' holds the unique server id, network socket handle, state, etc.
data Client
  = Client
  {
  -- | 'Server' assigned id
    _clientId :: Int
  -- | Socket converted to a Handle
  , _clientHandle :: Handle
  -- | Current state of 'Client'
  , _clientState :: ClientState
  -- | Token user has passed to identify self
  , _clientToken :: Token
  -- | Time in nanoseconds since last pong from 'Client'
  , _clientLastPongTime :: Integer
  -- | Time in nanoseconds since last message received
  , _clientLastMsgTime :: Integer
  -- | Client message rate state
  , _clientMsgRate :: Rate
  -- | Total messages since last rate check
  , _clientMsgCount :: Int
  -- | Incoming client messages to be read by server
  , _clientMsgQueue :: [Message]
  }

clientId :: Lens' Client Int
clientId =
  lens _clientId (\c n -> c { _clientId = n })

clientHandle :: Lens' Client Handle
clientHandle =
  lens _clientHandle (\c n -> c { _clientHandle = n })

clientState :: Lens' Client ClientState
clientState =
  lens _clientState (\c n -> c { _clientState = n })

clientToken :: Lens' Client Token
clientToken =
  lens _clientToken (\c n -> c { _clientToken = n })

clientLastPongTime :: Lens' Client Integer
clientLastPongTime =
  lens _clientLastPongTime (\c n -> c { _clientLastPongTime = n })

clientLastMsgTime :: Lens' Client Integer
clientLastMsgTime =
  lens _clientLastMsgTime (\c n -> c { _clientLastMsgTime = n })

clientMsgRate :: Lens' Client Rate
clientMsgRate =
  lens _clientMsgRate (\c n -> c { _clientMsgRate = n })

clientMsgCount :: Lens' Client Int
clientMsgCount =
  lens _clientMsgCount (\c n -> c { _clientMsgCount = n })

-- | The queue of incoming 'Client' 'Message's which the game world reads from
clientMsgQueue :: Lens' Client [Message]
clientMsgQueue =
  lens _clientMsgQueue (\c n -> c { _clientMsgQueue = n })

-- | Initialize a new 'Client' with a unique id and socket handle
initClient :: Int -> Handle -> STM (TVar Client)
initClient cid h =
  newTVar (Client cid h ClientConnected "" 0 0 Good 0 [])

-- | Run the 'Client''s various threads for message handling
--   This includes a ping check, rate check, and actual message parsing.
runClient :: TVar Client -> IO ()
runClient c0 = do
  void $ forkIO ping
  void $ forkIO rate
  void $ forkFinally run cleanup
  where
    -- Ping client to check for aliveness
    ping = do
      rc <- atomically $ readTVar c0
      forever $ do
        writeMessage (view clientHandle rc) Ping
        threadDelay 5000000 -- 5s

    rate = forever $ do
      rc <- atomically $ readTVar c0
      -- Send client a Warning message if they are over 10 messages in 100ms
      when (view clientMsgCount rc > 10) $ do
        writeMessage (view clientHandle rc) Warning
        atomically $ modifyTVar' c0 (set clientMsgRate Bad)
      -- Move client back to Good status if they are behaving
      when (and [view clientMsgCount rc < 10, view clientMsgRate rc == Bad]) $
        atomically $ modifyTVar' c0 (set clientMsgRate Good)
      threadDelay 100000 -- 100ms

    -- Capture incoming messages
    run = forever $ do
      rwc <- atomically $ readTVar c0
      stream (view clientHandle rwc) >>= \case
        Left _    -> return () -- TODO: Log failed messages?
        Right msg ->
          -- Only handle messages if this client is under the proper rate
          when (view clientMsgRate rwc == Good) $ do
            uc <- process rwc msg
            atomically $ writeTVar c0 uc

    -- Process messages and client logic
    process c1 msg = do
      t <- fmap toNanoSecs (getTime Monotonic)
      case msg of
        Unknown -> return c1
        -- Client is alive and Pong'ed the server
        Pong -> return $ set clientLastPongTime t c1
        -- Client sent a relevant message
        _ -> do
          -- Update relevant Client state
          return
            $ over clientMsgQueue (msg :)
            . set clientLastMsgTime t
            . over clientMsgCount (+ 1)
            $ c1

    -- Client cleanup
    cleanup _ = do
      rc <- atomically $ do
        modifyTVar' c0 (set clientState ClientDisconnected)
        readTVar c0
      hClose $ view clientHandle rc

--------------------------------------------------------------------------------

-- $server
--
-- 'Server' runs the game network code, handling incoming client connections.

-- | 'Server' tracks all incoming 'Client''s
data Server
  = Server
  {
  -- | Whether the 'Server' is ready to start handling connections or not
    _serverState :: ServerState
  -- | A unique id for new connecting clients
  , _serverIdCounter :: TVar Int
  -- | A map of all 'Client''s by their unique id
  , _serverGameClients :: TVar (Map Int (TVar Client))
  }

serverState :: Lens' Server ServerState
serverState =
  lens _serverState (\s n -> s { _serverState = n })

serverIdCounter :: Lens' Server (TVar Int)
serverIdCounter =
  lens _serverIdCounter (\s n -> s { _serverIdCounter = n })

serverGameClients :: Lens' Server (TVar (Map Int (TVar Client)))
serverGameClients =
  lens _serverGameClients (\s n -> s { _serverGameClients = n })

-- | Initialize a new 'Server'
initServer :: IO Server
initServer = do
  co <- newTVarIO 0
  cs <- newTVarIO Map.empty
  return (Server ServerInitializing co cs)

-- | Run the 'Server''s main network loop
runServer :: Server -> IO ()
runServer s = do
  void $ forkIO check
  run
  where
    -- TODO: Make these values come from a config
    run = serve (Host "127.0.0.1") "5555" $ \(sock, _) -> do
      h <- socketToHandle sock ReadWriteMode
      c1 <- atomically $ do
        modifyTVar' (view serverIdCounter s) (+ 1)
        cid <- readTVar (view serverIdCounter s)
        c0 <- initClient cid h
        modifyTVar' (view serverGameClients s) (Map.insert cid c0)
        return c0
      runClient c1

    -- Check if any clients have timed out after 10s
    -- Flag as disconnected
    check = forever $ do
      t <- fmap toNanoSecs (getTime Monotonic)
      atomically $ do
          cs <- readTVar (view serverGameClients s)
          traverse_ (update t) (Map.elems cs)
      threadDelay 1000000 -- 1s
      where
        update t c0 = do
          rc <- readTVar c0
          when (t - view clientLastPongTime rc > 10000000) $
            modifyTVar' c0 (set clientState ClientDisconnected)
