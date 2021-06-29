{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interruptible (specInterruptible) where

import           Control.Concurrent       (MVar, newEmptyMVar, putMVar, readMVar, threadDelay,
                                           tryReadMVar)
import           Control.Concurrent.Async (async, asyncThreadId, race_, wait, waitCatch)
import           Control.Exception.Safe   (SomeException, catchAsync, finally, isAsyncException,
                                           mask, throwIO, throwString, throwTo, try, tryAsync,
                                           uninterruptibleMask_)
import           Data.Time                (diffUTCTime, getCurrentTime)
import           System.Timeout           (timeout)

import           Test.Hspec

sleep :: Int -> IO Int
sleep n = threadDelay (n * 1000000) >> pure n

data CancellableIO a =
  CancellableIO
  { cancel :: IO ()
  , run    :: IO a
  }

--instance Functor CancellableIO where
--  fmap f (CancellableIO {cancel, run}) = CancellableIO cancel (fmap f run)

mapAction :: (IO a -> IO a) -> CancellableIO a -> CancellableIO a
mapAction f (CancellableIO {cancel, run}) = CancellableIO { cancel = cancel, run = f run }

mapCancel :: (IO () -> IO ()) -> CancellableIO a -> CancellableIO a
mapCancel f (CancellableIO {cancel, run}) = CancellableIO { cancel = f cancel, run = run }

unint :: CancellableIO a -> CancellableIO a
unint = mapAction (\a -> uninterruptibleMask_ a >>= \r -> threadDelay 1000 >> pure r)

cancellableMVar :: IO a -> IO (CancellableIO ())
cancellableMVar x = do
  c :: MVar () <- newEmptyMVar
  return $ CancellableIO (putMVar c ()) (race_ (readMVar c >> putStrLn "got mvar") x)

cancellableMVarNorace :: IO a -> IO (CancellableIO ())
cancellableMVarNorace x = do -- XXXXXXX this doesn't actually call X anymore
  c :: MVar () <- newEmptyMVar
  return $ CancellableIO (putMVar c ()) (loop c x)
  where
    loop c x = tryReadMVar c >>= \case
      Just _ -> return ()
      Nothing -> do
        threadDelay 100
        loop c x

cancellableSleep :: Int -> IO (CancellableIO ())
cancellableSleep n = do -- XXXXXXX this doesn't actually call X anymore
  c :: MVar () <- newEmptyMVar
  return $ CancellableIO (putMVar c ()) (action c)
  where
    action c = do
      t0 <- getCurrentTime
      loop t0 c
    loop t0 c = tryReadMVar c >>= \case
      Just _ -> return ()
      Nothing -> do
        t <- getCurrentTime
        if diffUTCTime t t0 >= fromIntegral n then
          return ()
        else do
          threadDelay 100
          loop t0 c

interruptible :: CancellableIO a -> IO a
interruptible x = mask $ \restore -> do
  a <- async (run x)
  restore (wait a >> pure ()) `catchAsync`
    (\(e::SomeException) -> do
           putStrLn $ "caught exception"
           if isAsyncException e then do
             cancel x -- FIXME what if this throws?
             putStrLn "canceled from outside"
             throwTo (asyncThreadId a) e
           else
             throwIO e)
  wait a

async' x = async $ x `finally` putStrLn "async exited"

interruptible' :: CancellableIO a -> IO a
interruptible' x = mask $ \restore -> do
  a <- async' (run x)
  res <- tryAsync $ restore (waitCatch a)
  case res of
    Left (e :: SomeException) -> do  -- FIXME we could directly go for SomeAsyncException and avoid impossible below
      putStrLn $ "caught exception"
      if isAsyncException e then do
        putStrLn $ "external async exception"
        -- if cancelling failed, we want to pass on that exception, but still collect our spawned thread
        cancelRes <- try $ cancel x -- FIXME what if this throws? / fails?
        putStrLn "canceled from outside"
        throwTo (asyncThreadId a) e
        r <- wait a -- FIXME masked, but what do we want to do if we receive a second async exception?
        case cancelRes of
          Left (e :: SomeException) -> throwIO e
          _                         -> pure r
      else do
        putStrLn "impossible: non-async exception on waitCatch"
        throwIO e
    Right (Left e) ->
      throwIO e
    Right (Right r) ->
      pure r

specInterruptible :: SpecWith ()
specInterruptible = do
  describe "interruptible" $ do
    it "interrupts an uninterruptible sleep via cancel" $ do
      x <- cancellableSleep 2
      let x' = unint x
      t0 <- getCurrentTime
      res <- timeout 500000 $ interruptible x'
      t1 <- getCurrentTime
      res `shouldBe` Nothing
      -- promptly
      diffUTCTime t1 t0 `shouldSatisfy` (\d -> d >= 0.5 && d < 0.75)

  describe "interruptible'" $ do
    it "interrupts an uninterruptible sleep via cancel" $ do
      x <- cancellableSleep 2
      let x' = unint x
      t0 <- getCurrentTime
      res <- timeout 500000 $ interruptible' x'
      t1 <- getCurrentTime
      res `shouldBe` Nothing
      -- promptly
      diffUTCTime t1 t0 `shouldSatisfy` (\d -> d >= 0.5 && d < 0.75)

    it "doesn't interrupt when cancel only throws, but still collects the async" $ do
      x <- cancellableSleep 2
      let x' = mapCancel (const $ throwString "cancel exception") $ unint x
      t0 <- getCurrentTime
      res <- timeout 500000 $ interruptible' x'
      t1 <- getCurrentTime
      res `shouldBe` Nothing
      -- promptly
      diffUTCTime t1 t0 `shouldSatisfy` (\d -> d >= 2 && d < 2.25)
