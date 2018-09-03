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
import ArmoredBits.Network.Peer
import ArmoredBits.Network.Server
import ArmoredBits.Types
--------------------------------------------------------------------------------

handle :: String
handle = ".test.handle"

ts :: [Token]
ts = ["TOkEN1"]

serverSpec :: IO ()
serverSpec = hspec $ do
  describe "ArmoredBits.Network.Server" $ do
    it "mkServer" $ do
      s <- mkServer ts

      view serverState s `shouldBe` ServerInitializing

    it "getPeerId" $ do
      s <- mkServer ts
      (p1, p2) <- atomically $ do
        p1 <- getPeerId s
        p2 <- readTVar (view serverIdCounter s)
        return (p1, p2)

      p1 `shouldBe` p2

    it "createPeer" $ do
      h <- openFile handle WriteMode
      s <- mkServer ts
      tp <- atomically $ createPeer h s

      (p, pid, ps) <- atomically $
        (,,) <$> readTVar tp
             <*> readTVar (view serverIdCounter s)
             <*> readTVar (view serverGamePeers s)

      view peerId p `shouldBe` pid
      Map.member pid ps `shouldBe` True

      removeFile handle
