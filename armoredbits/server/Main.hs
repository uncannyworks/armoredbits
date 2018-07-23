module Main where

--------------------------------------------------------------------------------
import ArmoredBits.Config.Parse
import ArmoredBits.Network.Server
--------------------------------------------------------------------------------

main :: IO ()
main = do
  o <- getOpts
  s <- mkServer (optionTokens o)
  runServer o s
