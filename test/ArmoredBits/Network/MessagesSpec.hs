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

module ArmoredBits.Network.MessagesSpec where

--------------------------------------------------------------------------------
import System.Directory (removeFile)
import System.IO
--------------------------------------------------------------------------------
import Test.Hspec
--------------------------------------------------------------------------------
import ArmoredBits.Network.Messages
--------------------------------------------------------------------------------

handle :: String
handle = ".test.handle"

messagesSpec :: IO ()
messagesSpec = hspec $ do
  describe "ArmoredBits.Network.Message" $ do
    it "serverSend/clientRecv Ping" $ do
      fw <- openFile handle WriteMode
      serverSend fw (MessageServer Ping)
      hClose fw

      fr <- openFile handle ReadMode
      Right ping <- clientRecv fr
      hClose fr

      ping `shouldBe` (MessageServer Ping)

      removeFile handle

    it "clientSend/serverRecv Pong" $ do
      fw <- openFile handle WriteMode
      clientSend fw (MessageClient Pong)
      hClose fw

      fr <- openFile handle ReadMode
      Right pong <- serverRecv fr
      hClose fr

      pong `shouldBe` (MessageClient Pong)

      removeFile handle
