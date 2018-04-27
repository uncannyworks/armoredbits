module Main where

import ArmoredBits.Network

main :: IO ()
main = initServer >>= runServer
