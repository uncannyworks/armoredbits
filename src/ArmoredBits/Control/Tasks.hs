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

{-|

Module      : ArmoredBits.Control.Tasks
Description : A concurrent thread manager which properly handles thread shutdown
              on engine exit.

|-}
module ArmoredBits.Control.Tasks
  ( mkTaskManager
  , runTasks
  , execTask
  , waitTasks
  , cancelTasks
  , handleKill
  ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, modifyTVar')
import Control.Exception
import Data.Map (Map, delete, elems, empty, insert)
import Data.Foldable (traverse_)
--------------------------------------------------------------------------------

-- | A 'TaskManager' tracks several concurrent threads.
newtype TaskManager = TaskManager (TVar (Map ThreadId (Async ())))

-- | Initialize a new 'TaskManager'.
mkTaskManager :: IO TaskManager
mkTaskManager = TaskManager <$> newTVarIO empty

handleKill :: TaskManager -> IO () -> AsyncException -> IO ()
handleKill tm cu _ = uninterruptibleMask_ (cancelTasks tm >> cu)

-- | Wait for tasks to finish.
waitTasks :: TaskManager -> IO ()
waitTasks (TaskManager m) = do
  ts <- readTVarIO m
  traverse_ wait (elems ts)

-- | Cancel and wait for tasks to finish.
cancelTasks :: TaskManager -> IO ()
cancelTasks (TaskManager m) = do
  ts <- readTVarIO m
  cs <- traverse (async . uninterruptibleCancel) (elems ts)
  traverse_ wait cs

-- Execute a single task.
execTask :: TaskManager -> IO () -> IO ()
execTask (TaskManager m) io = mask_ $ do
  t <- asyncWithUnmask (\um -> um go)
  atomically $ modifyTVar' m (insert (asyncThreadId t) t)
  where
    go = io >> myThreadId >>= \tid -> atomically (modifyTVar' m (delete tid))

-- | Run a series of async tasks and then call cleanup on exit.
--
-- > runTasks [task1, task2, task3] cleanup
runTasks :: [IO ()] -> IO () -> IO ()
runTasks ios cu = do
  tm <- mkTaskManager
  catch (traverse_ (execTask tm) ios >> waitTasks tm >> cu) (handleKill tm cu)
