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
      serverSend fw Ping
      hClose fw

      fr <- openFile handle ReadMode
      Right ping <- clientRecv fr
      hClose fr

      ping `shouldBe` Ping

      removeFile handle

    it "clientSend/serverRecv Pong" $ do
      fw <- openFile handle WriteMode
      clientSend fw Pong
      hClose fw

      fr <- openFile handle ReadMode
      Right pong <- serverRecv fr
      hClose fr

      pong `shouldBe` Pong

      removeFile handle
