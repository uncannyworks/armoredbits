{-# LANGUAGE OverloadedStrings #-}

module ArmoredBits.Network.ServerSpec where

--------------------------------------------------------------------------------
import Control.Concurrent.STM.TVar
import Control.Monad.STM (atomically)
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import System.Directory (removeFile)
import System.IO
--------------------------------------------------------------------------------
import Test.Hspec
--------------------------------------------------------------------------------
import ArmoredBits.Types
--------------------------------------------------------------------------------
import ArmoredBits.Network.Peer
import ArmoredBits.Network.Server
--------------------------------------------------------------------------------

handle :: String
handle = ".test.handle"

serverSpec :: IO ()
serverSpec = hspec $ do
  describe "ArmoredBits.Network.Server" $ do
    it "mkServer" $ do
      s <- mkServer

      view serverState s `shouldBe` ServerInitializing

    it "getPeerId" $ do
      s <- mkServer
      (p1, p2) <- atomically $ do
        p1 <- getPeerId s
        p2 <- readTVar (view serverIdCounter s)
        return (p1, p2)

      p1 `shouldBe` p2

    it "createPeer" $ do
      h <- openFile handle WriteMode
      s <- mkServer
      tp <- atomically $ createPeer h s

      (p, pid, ps) <- atomically $
        (,,) <$> readTVar tp
             <*> readTVar (view serverIdCounter s)
             <*> readTVar (view serverGamePeers s)

      view peerId p `shouldBe` pid
      Map.member pid ps `shouldBe` True

      removeFile handle
