{-|

Module      : ArmoredBits.TaskManager
Description : A concurrent thread manager which properly handles thread shutdown
              on server exit.

|-}
module ArmoredBits.TaskManager
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
