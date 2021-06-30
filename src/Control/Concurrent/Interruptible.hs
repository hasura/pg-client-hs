{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.Interruptible
    ( interruptible
    ) where

import           Control.Concurrent.Async
import           Control.Exception.Safe

-- | interruptible runs the given action in in a separate thread,
-- running the given cancel action before passing on any asynchronous
-- exceptions to that thread. The intent is that
--   `interruptible (pure ()) == id`
-- in all respects (including exception handling), assuming the wrapped
-- action behaves somewhat reasonably (i.e., doesn't swallow asynchronous
-- exceptions). Particularly, we guarantee that the separate thread terminates
-- before we return.
--
-- The point of this is to allow breaking out of blocking actions if they
-- provide some cancelling escape hatch.
interruptible :: IO () -> IO a -> IO a
interruptible interrupt action = mask $ \restore -> do
  a <- async action

  -- By using `try` with `waitCatch`, we can distinguish between asnychronous
  -- exceptions received from the outside, and those thrown by the wrapped action.
  -- (The latter shouldn't occur, but we also want to avoid throwing an exception
  -- back at the thread below.)
  res <- tryAsync $ restore (waitCatch a)
  case res of
    -- Due to the use of `waitCatch` above, the only exceptions that `tryAsync`
    -- might catch are asynchronous exceptions received from the "outside".
    -- Thus, the `Left` case is the only one where the async action has not
    -- necessarily terminated.
    Left (e :: SomeAsyncException) -> do
      -- Cancelling might throw an exception; we save that and re-raise it,
      -- but not before doing or job of passing the asynchronous exception on
      -- to our child and waiting for it to terminate.
      interruptRes <- try $ interrupt
      throwTo (asyncThreadId a) e
      r <- wait a
      case interruptRes of
        Left (cancelEx :: SomeException) -> throwIO cancelEx
        _                                -> pure r
    -- In the non-interrupted case, we "undo" the `try`, collapsing things
    -- effectively to `restore (wait a)`.
    Right (Left e) ->
      throwIO e
    Right (Right r) ->
      pure r


