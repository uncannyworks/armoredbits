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

{-# LANGUAGE OverloadedStrings #-}

module ArmoredBits.Network.PeerSpec where

--------------------------------------------------------------------------------
import Control.Concurrent.STM.TVar
import Control.Monad.Reader (runReaderT)
import Control.Monad.STM (atomically)
import Lens.Micro.Platform
import System.Directory (removeFile)
import System.IO
--------------------------------------------------------------------------------
import Test.Hspec
--------------------------------------------------------------------------------
import ArmoredBits.Network.Messages
import ArmoredBits.Types
import ArmoredBits.Util
--------------------------------------------------------------------------------
import ArmoredBits.Network.Peer
import ArmoredBits.Network.Internal.Peer
--------------------------------------------------------------------------------

handle :: String
handle = ".test.handle"

validToken :: Token
validToken = mkToken "TOkEN1"

pe :: PeerEnv
pe = PeerEnv 100 10 [validToken]

mkSpecPeer :: IO (TVar Peer)
mkSpecPeer = do
  h <- openFile handle WriteMode
  atomically $ mkPeer 1 h

peerSpec :: IO ()
peerSpec = hspec $ do
  describe "ArmoredBits.Network.Peer" $ do
    it "mkPeer" $ do
      p <- mkSpecPeer
      v <- readTVarIO p

      view peerId v `shouldBe` 1

      removeFile handle

    it "checkKeepAlive" $ do
      p <- mkSpecPeer
      atomically $ checkKeepAlive 10 (toInteger $ inSeconds 11) p
      v <- readTVarIO p

      view peerState v `shouldBe` PeerDisconnected

      removeFile handle

  describe "ArmoredBits.Network.Internal.Peer" $ do
    it "resetRate'" $ do
      p <- mkSpecPeer
      atomically $ modifyTVar' p (set peerMsgCount 100)
      atomically $ resetRate' p
      v <- readTVarIO p

      view peerMsgCount v `shouldBe` 0

      removeFile handle

    it "disconnectPeer" $ do
      p <- mkSpecPeer
      atomically $ disconnectPeer p
      v <- readTVarIO p

      view peerState v `shouldBe` PeerDisconnected

      removeFile handle

    it "pingPeer'" $ do
      fw <- openFile handle WriteMode
      pingPeer' fw
      hClose fw

      fr <- openFile handle ReadMode
      Right ping <- clientRecv fr
      hClose fr

      ping `shouldBe` (MessageServer Ping)

      removeFile handle

    it "processMessages - Pong" $ do
      p <- mkSpecPeer
      atomically $ do
        rp <- readTVar p
        v <- runReaderT (processMessages 123 (MessageClient Pong) rp) pe
        case v of
          Right wp     -> writeTVar p wp
          Left (_, wp) -> writeTVar p wp
      v <- readTVarIO p

      view peerLastPongTime v `shouldBe` 123

      removeFile handle

    it "processMessages - SendToken" $ do
      p <- mkSpecPeer
      atomically $ do
        rp <- readTVar p
        v <- runReaderT (processMessages 123 (MessageClient (SendToken validToken)) rp) pe
        case v of
          Right wp     -> writeTVar p wp
          Left (_, wp) -> writeTVar p wp
      v <- readTVarIO p

      view peerToken v `shouldBe` Just validToken
      view peerState v `shouldBe` PeerAuthenticated

      removeFile handle

    it "processMessages - Any" $ do
      p <- mkSpecPeer
      atomically $ do
        rp <- return . set peerToken (Just validToken) =<< readTVar p
        v <- runReaderT (processMessages 123 (EventClient ClientEventTest) rp) pe
        case v of
          Right wp    -> writeTVar p wp
          Left (_, wp) -> writeTVar p wp
      v <- readTVarIO p

      view peerLastMsgTime v `shouldBe` 123
      view peerMsgCount v `shouldBe` 1
      view peerEventQueue v `shouldBe` [ClientEventTest]
      view peerMsgRate v `shouldBe` Good

      removeFile handle

    it "updateMsgRate" $ do
      p <- mkSpecPeer
      atomically $ modifyTVar' p
        (updateMsgRate 10 . set peerMsgCount 11)
      v <- readTVarIO p

      view peerMsgRate v `shouldBe` Bad

      removeFile handle

    it "checkBadPeer" $ do
      p <- mkSpecPeer
      atomically $ modifyTVar' p
        (checkBadPeer 10 . set peerEventQueue (take 10 $ repeat ClientEventUnknown))
      v <- readTVarIO p

      view peerState v `shouldBe` PeerDisconnected

      removeFile handle

    it "runPeer'" $ do
      fw <- openFile handle WriteMode
      clientSend fw (EventClient ClientEventTest)
      hClose fw

      h <- openFile handle ReadMode
      p <- atomically $ do
        p <- mkPeer 1 h
        modifyTVar' p (set peerToken (Just validToken))
        return p
      runPeer' pe h p
      hClose h
      v <- readTVarIO p

      view peerMsgCount v `shouldBe` 1
      view peerEventQueue v `shouldBe` [ClientEventTest]

      removeFile handle
