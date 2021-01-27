{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.Hspec

import           Database.PG.Query

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Exception      (Exception (fromException))
import           Control.Exception.Base (IOException, SomeException)
import           Control.Monad.Except   (ExceptT, MonadTrans (lift), runExceptT)

import qualified Data.ByteString.Char8  as BS
import qualified System.Environment     as Env

main :: IO ()
main = hspec $ do
  describe "acquiring connections" do
    it "acquire a single available resource" do
      simpleTest `shouldReturn` Nothing
    it "error when no connections available" do
      noConnectionAvailable `shouldReturn` Nothing
    it "release and acquire works correctly" do
      releaseAndAcquire `shouldReturn` Nothing
    it "release and acquire works correctly" do
      releaseAndAcquireWithTimeout `shouldReturn` Nothing
    it "time out works correctly" do
      releaseAndAcquireWithTimeoutNegative `shouldReturn` Nothing

mkPool :: IO PGPool
mkPool = do
  dbUri <- BS.pack <$> Env.getEnv "DATABASE_URL"
  initPGPool (connInfo dbUri) connParams logger
  where
    connInfo uri = ConnInfo { ciRetries, ciDetails = mkDetails uri }
    ciRetries = 0
    mkDetails = CDDatabaseURI
    logger = mempty
    connParams = ConnParams 1 1 60 True Nothing (Just 3)

withFreshPool :: (FromPGTxErr e, FromPGConnErr e) => PGPool -> IO a -> IO (Either e a)
withFreshPool pool action =
  runExceptT
    . withConn pool (ReadCommitted, Just ReadOnly)
    . const $ lift action

err :: Show a => a -> IO (Maybe String)
err = pure . Just . show

nada :: IO ()
nada = mempty

simpleTest :: IO (Maybe String)
simpleTest = do
  pool <- mkPool
  withFreshPool pool nada >>= \case
    Left (e :: PGExecErr) -> err e
    Right _               -> mempty

noConnectionAvailable :: IO (Maybe String)
noConnectionAvailable = do
  pool <- mkPool
  withFreshPool pool (action pool) >>= \case
    Left (e :: PGExecErr) -> err e
    Right _               -> mempty
  where
    action pool = withFreshPool pool nada >>= \case
      Left (e :: PGExecErr) -> mempty
      Right _               -> err "connection acquisition expected to fail"

releaseAndAcquire :: IO (Maybe String)
releaseAndAcquire = do
  pool <- mkPool
  _ <- withFreshPool pool nada >>= \case
    Left (e :: PGExecErr) -> err e
    Right _               -> mempty
  withFreshPool pool nada >>= \case
    Left (e :: PGExecErr) -> err e
    Right _               -> mempty

releaseAndAcquireWithTimeout :: IO (Maybe String)
releaseAndAcquireWithTimeout = do
  pool <- mkPool
  _ <- forkIO $ withFreshPool pool (threadDelay 300_000) >>= \case
    Left (e :: PGExecErr) -> error "unexpected error when acquiring connections"
    Right _               -> mempty
  threadDelay 100_000
  withFreshPool pool nada >>= \case
    Left (e :: PGExecErr) -> err e
    Right _               -> mempty

releaseAndAcquireWithTimeoutNegative :: IO (Maybe String)
releaseAndAcquireWithTimeoutNegative = do
  pool <- mkPool
  _ <- forkIO $ withFreshPool pool (threadDelay 10_000_000) >>= \case
    Left (e :: PGExecErr) -> error "unexpected error when acquiring connections"
    Right _               -> mempty
  threadDelay 1_000_000
  withFreshPool pool nada >>= \case
    Left (e :: PGExecErr) -> mempty
    Right _               -> err "Wat"

