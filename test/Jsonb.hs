{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans -Wno-name-shadowing #-}

module Jsonb (specJsonb) where

import Data.String
import System.Environment qualified as Env
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import Database.PG.Query
import Database.PG.Query.Connection
import Database.PostgreSQL.LibPQ.Internal
import GHC.Generics
import Test.Hspec
import Prelude

newtype TestValue = TestValue {hey :: Int}
  deriving stock (Show, Generic)

instance J.FromJSON TestValue

getPgUri :: (MonadIO m) => m BS.ByteString
getPgUri = liftIO $ fromString <$> Env.getEnv "DATABASE_URL"

getPostgresConnect :: (MonadIO m) => m ConnInfo
getPostgresConnect = do
  dbUri <- getPgUri
  pure $ defaultConnInfo
    { ciDetails = CDDatabaseURI dbUri
    }

specJsonb :: Spec
specJsonb = do
  describe "Feelings" $ do
    it "Querying 'json' from PostgreSQL" $ do
      pg <- getPostgresConnect
      SingleRow (Identity (i :: BS.ByteString)) <- runTxT pg
        (rawQE show "select '{\"hey\":42}'::json" [] False)
      i `shouldSatisfy` const True

    it "Querying 'jsonb' from PostgreSQL (note the difference in formatting and \\SOH prefix)" $ do
      pg <- getPostgresConnect
      SingleRow (Identity (i :: BS.ByteString)) <- runTxT pg
          (rawQE show "select '{\"hey\":42}'::jsonb" [] False)
      i `shouldSatisfy` const True

    it "Querying 'jsonb' from PostgreSQL" $ do
      pg <- getPostgresConnect
      SingleRow (Identity (AltJ (i :: TestValue))) <- runTxT pg
          (rawQE show "select '{\"hey\":42}'::jsonb" [] False)
      i `shouldSatisfy` const True

instance FromPGConnErr String where
  fromPGConnErr = show

runTxT :: forall a. ConnInfo -> TxET String IO a -> IO a
runTxT conn q = do
  pool <- initPGPool conn defaultConnParams (const (return ()))
  x :: Either String a <- runExceptT $ runTx' pool q
  destroyPGPool pool
  case x of
    Left e -> error e
    Right r -> return r
