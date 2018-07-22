module Main where

--------------------------------------------------------------------------------
import ArmoredBits.Network.MessagesSpec
import ArmoredBits.Network.PeerSpec
import ArmoredBits.Network.ServerSpec
--------------------------------------------------------------------------------

main :: IO ()
main = do
  messagesSpec
  peerSpec
  serverSpec
