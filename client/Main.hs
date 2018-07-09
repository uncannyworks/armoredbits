{-# LANGUAGE LambdaCase #-}

module Main where

--------------------------------------------------------------------------------
import Control.Monad (forever)
import Network.Simple.TCP
import qualified Network.Socket as N
import System.IO
--------------------------------------------------------------------------------
import ArmoredBits.Messages
import ArmoredBits.TaskManager
--------------------------------------------------------------------------------
import Debug.Trace

main :: IO ()
main =
  connect "127.0.0.1" "5555" $ \(sock, _) -> do
    h <- N.socketToHandle sock ReadWriteMode
    runTasks [run h] (cleanup h)
  where
    run h = forever $ do
      readMessage h >>= \case
        Left e    -> do
          traceIO $ show e
          return ()
        Right msg -> do
          traceIO $ show msg
          case msg of
            Ping -> do
              traceIO "PONG"
              writeMessage h Pong
            _    -> do
              traceIO "nonsense"
              return ()

    cleanup = hClose
