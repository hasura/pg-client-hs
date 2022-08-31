{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans -Wno-name-shadowing #-}

module Jsonb (specJsonb) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isRight)
import Data.Kind (Type)
import Data.String
import Database.PG.Query
import Database.PG.Query.Connection
import Database.PostgreSQL.LibPQ.Internal
import GHC.Generics
import System.Environment qualified as Env
import Test.Hspec
import Prelude

type TestValue :: Type
newtype TestValue = TestValue {hey :: Int}
  deriving stock (Show, Generic)

instance J.FromJSON TestValue

instance Show (AltJ TestValue) where
  show (AltJ tv) = show tv

instance Show (AltJ J.Value) where
  show (AltJ v) = show v

getPgUri :: (MonadIO m) => m BS.ByteString
getPgUri = liftIO $ fromString <$> Env.getEnv "DATABASE_URL"

getPostgresConnect :: (MonadIO m) => m ConnInfo
getPostgresConnect = do
  dbUri <- getPgUri
  pure $
    defaultConnInfo
      { ciDetails = CDDatabaseURI dbUri
      }

specJsonb :: Spec
specJsonb = do
  describe "AltJ encoder works with SOH mid-bytestring" $ do
    it "is great again" $ do
      input <- LBS.readFile "./Test/static/soh-in-response.json"
      fromCol @(AltJ TestValue) (Just (LBS.toStrict input)) `shouldSatisfy` isRight

  describe "Decoding JSON and JSONB" $ do
    it "Querying 'json' from PostgreSQL succeeds" $ do
      pg <- getPostgresConnect
      result <-
        runTxT
          pg
          (rawQE show "select '{\"hey\":42}'::json" [] False)

      result `shouldSatisfy` \case
        (Right (SingleRow (Identity (_ :: BS.ByteString)))) -> True
        Left e -> error e

    it "Querying 'jsonb' from PostgreSQL succeeds" $ do
      pg <- getPostgresConnect
      result <-
        runTxT
          pg
          (rawQE show "select '{\"hey\":42}'::jsonb" [] False)

      result `shouldSatisfy` \case
        Right (SingleRow (Identity (_ :: BS.ByteString))) -> True
        Left e -> error e

    it "Querying 'json' from PostgreSQL into AltJ type succeeds" $ do
      pg <- getPostgresConnect
      result <-
        runTxT
          pg
          (rawQE show "select '{\"hey\":42}'::json" [] False)

      result `shouldSatisfy` \case
        Right (SingleRow (Identity (AltJ (_ :: TestValue)))) -> True
        Left e -> error e

    it "Querying 'jsonb' from PostgreSQL into AltJ type succeeds" $ do
      pg <- getPostgresConnect
      result <-
        runTxT
          pg
          (rawQE show "select '{\"hey\":42}'::jsonb" [] False)

      result `shouldSatisfy` \case
        Right (SingleRow (Identity (AltJ (_ :: TestValue)))) -> True
        Left e -> error e

instance FromPGConnErr String where
  fromPGConnErr = show

runTxT :: forall a. ConnInfo -> TxET String IO a -> IO (Either String a)
runTxT conn q = do
  pool <- initPGPool conn defaultConnParams (const (return ()))
  x <- runExceptT $ runTx' pool q
  destroyPGPool pool
  pure x
