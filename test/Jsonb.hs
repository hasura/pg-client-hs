{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans -Wno-name-shadowing #-}

module Jsonb (specJsonb) where

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

stringface :: BS.ByteString
stringface = "postgresql://hasura:hasura@127.0.0.1:65002/hasura"

pgDb, cockroachDb :: ConnInfo
pgDb =
  defaultConnInfo
    { ciDetails = CDDatabaseURI stringface
    }
cockroachDb =
  defaultConnInfo
    { ciDetails = CDDatabaseURI stringface
    }

specJsonb :: Spec
specJsonb = do
  describe "Feelings" $ do
    it "Querying 'json' from PostgreSQL" $ do
      SingleRow (Identity (i :: BS.ByteString)) <- runTxT pgDb (rawQE show "select '{\"hey\":42}'::json" [] False)
      print i

    it "Querying 'jsonb' from PostgreSQL (note the difference in formatting and \\SOH prefix)" $ do
      SingleRow (Identity (i :: BS.ByteString)) <- runTxT pgDb (rawQE show "select '{\"hey\":42}'::jsonb" [] False)
      print i

    it "Querying 'json' from CockroachDB" $ do
      SingleRow (Identity (i :: BS.ByteString)) <- runTxT cockroachDb (rawQE show "select '{\"hey\":42}'::json" [] False)
      print i

    it "Querying 'jsonb' from CockroachDB (note the difference in formatting and \\SOH prefix)" $ do
      SingleRow (Identity (i :: BS.ByteString)) <- runTxT cockroachDb (rawQE show "select '{\"hey\":42}'::jsonb" [] False)
      print i

    -- This fails for both
    it "Querying 'jsonb' from PostgreSQL" $ do
      -- Note that 'AltJ' is simply a newtype wrapper that directs 'FromCol' to use 'FromJSON' to produce a Haskell Value of the specified type.
      SingleRow (Identity (AltJ (i :: TestValue))) <- runTxT pgDb (rawQE show "select '{\"hey\":42}'::jsonb" [] False)
      print i

    it "Querying 'jsonb' from CockroachDB" $ do
      SingleRow (Identity (AltJ (i :: TestValue))) <- runTxT cockroachDb (rawQE show "select '{\"hey\":42}'::json" [] False)
      print i

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
