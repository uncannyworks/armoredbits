module Main where

--------------------------------------------------------------------------------
import ArmoredBits.Network.MessagesSpec
import ArmoredBits.Network.PeerSpec
--------------------------------------------------------------------------------

main :: IO ()
main = do
  messagesSpec
  peerSpec
