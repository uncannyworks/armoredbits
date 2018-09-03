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
