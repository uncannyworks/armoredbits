{-# LANGUAGE LambdaCase #-}

module Main where

--------------------------------------------------------------------------------
import Control.Monad (forever)
import Network.Simple.TCP
import qualified Network.Socket as N
import System.IO
--------------------------------------------------------------------------------
import ArmoredBits.Config.Parse
import ArmoredBits.Control.Tasks
import ArmoredBits.Network.Messages
--------------------------------------------------------------------------------
import Debug.Trace

main :: IO ()
main = do
  o <- getOpts
  connect (optionHost o) (optionPort o) $ \(sock, _) -> do
    h <- N.socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    runTasks [run h] (cleanup h)
  where
    run h = forever $ do
      clientRecv h >>= \case
        Left e    -> do
          traceIO $ show e
          return ()
        Right msg -> do
          traceIO $ show msg
          case msg of
            Ping -> do
              traceIO "PONG"
              clientSend h Pong
            _    -> do
              traceIO "nonsense"
              return ()

    cleanup = hClose
