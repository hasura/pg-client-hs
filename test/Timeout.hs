{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Timeout (specTimeout) where

import           Test.Hspec

import           Database.PG.Query

import           Control.Monad.Except     (runExceptT)
import           Control.Monad.Identity   (Identity (..), runIdentity)

import           Control.Concurrent.Async (async, wait)
import           Data.Int                 (Int32)
import           Data.Time                (diffUTCTime, getCurrentTime)
import           System.Timeout           (timeout)

import qualified Data.ByteString.Char8    as BS
import qualified System.Environment       as Env

specTimeout :: SpecWith ()
specTimeout = before initDB $ do
  describe "slow insert" $ do
    it "inserts a row successfully if not interrupted" $ \pool -> do
      countRows pool `shouldReturn` 0
      t0 <- getCurrentTime
      res <- sleepyInsert pool 1
      t1 <- getCurrentTime
      res `shouldBe` Right ()
      diffUTCTime t1 t0 `shouldSatisfy` (\x -> x >= 1 && x < 2)
      countRows pool `shouldReturn` 1
    it "is interrupted late by timeout" $ \pool -> do
      countRows pool `shouldReturn` 0
      t0 <- getCurrentTime
      res <- timeout 500000 $ sleepyInsert pool 1
      t1 <- getCurrentTime
      -- timed out
      res `shouldBe` Nothing
      -- but still took the full second
      diffUTCTime t1 t0 `shouldSatisfy` (\x -> x >= 1 && x < 2)
      -- insert was rolled back (FIXME this is not guaranteed if timeout hits late)
      countRows pool `shouldReturn` 0
    it "is not rolled back with async" $ \pool -> do
      countRows pool `shouldReturn` 0
      t0 <- getCurrentTime
      res <- timeout 500000 $ do
        a <- async $ sleepyInsert pool 1
        wait a
      t1 <- getCurrentTime
      -- timed out
      res `shouldBe` Nothing
      -- quickly
      diffUTCTime t1 t0 `shouldSatisfy` (\x -> x >= 0.5 && x < 0.75)
      -- but the insert went through
      countRows pool `shouldReturn` 1
    it "is interrupted promptly with cancelling" $ \pool -> do
      cancelablePool <- mkPool True
      countRows pool `shouldReturn` 0
      t0 <- getCurrentTime
      res <- timeout 500000 $ sleepyInsert cancelablePool 1
      t1 <- getCurrentTime
      -- didn't time out, instead returned properly with a cancel error
      res `shouldSatisfy` (\case
        Just (Left err) -> isCancelErr err
        _               -> False)
      -- promptly
      diffUTCTime t1 t0 `shouldSatisfy` (\x -> x >= 0.5 && x < 0.75)
      -- insert was rolled back (FIXME not guaranteed)
      countRows pool `shouldReturn` 0

mkPool :: Bool -> IO PGPool
mkPool cancelable = do
  dbUri <- BS.pack <$> Env.getEnv "DATABASE_URL"
  initPGPool (connInfo dbUri) connParams logger
  where
    ciRetries = 0
    connInfo uri = ConnInfo { ciRetries, ciDetails = CDDatabaseURI uri }
    connParams = ConnParams 1 1 60 True Nothing (Just 3) cancelable
    logger = mempty

mode :: (TxIsolation, Maybe TxAccess)
mode = (Serializable, Just ReadWrite)

initDB :: IO PGPool
initDB = do
  pool <- mkPool False
  let tx = multiQE PGExecErrTx (fromText statements)
  res <- runExceptT $ runTx pool mode tx
  res `shouldBe` Right ()
  return pool
  where
    statements =
      "DROP TABLE IF EXISTS test_timeout;\n\
      \CREATE TABLE test_timeout (x int);\n\
      \CREATE OR REPLACE FUNCTION sleepy(int) RETURNS int\n\
      \  LANGUAGE sql AS\n\
      \$$\n\
      \  select pg_sleep($1);\n\
      \  select $1\n\
      \$$;\n"

countRows :: PGPool -> IO Int
countRows pool = do
  res <- runExceptT $ runIdentity . getRow <$> runTx pool mode tx
  let Right count = res
  return count
  where
    query = "SELECT count(*) FROM test_timeout"
    tx = withQE PGExecErrTx (fromText query) () False

sleepyInsert :: PGPool -> Int32 -> IO (Either PGExecErr ())
sleepyInsert pool sleep =
  runExceptT $ runTx pool mode tx
  where
    query = "INSERT INTO test_timeout VALUES (sleepy($1))"
    tx = withQE PGExecErrTx (fromText query) (Identity sleep) False

isCancelErr :: PGExecErr -> Bool
isCancelErr err = case err of
  PGExecErrTx (PGTxErr _msg _ _ _) -> True -- fixme
  _                                -> False
