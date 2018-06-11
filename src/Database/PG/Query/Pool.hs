{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Database.PG.Query.Pool
  ( ConnParams (..)
  , PGPool
  , defaultConnParams
  , initPGPool
  , destroyPGPool
  , withConn
  , runTxOnConn'
  , beginTx
  , abortTx
  , commitTx
  , runTx
  , runTxOnConn
  , catchConnErr
  , sql
  , sqlFromFile
  , PGExecErr(..)
  , FromPGConnErr(..)
  , FromPGTxErr(..)
  ) where

import           Database.PG.Query.Connection
import           Database.PG.Query.Transaction

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.IORef
import           Data.Monoid
import           GHC.Exts                    (fromString)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import qualified Data.ByteString.Builder     as BB
import qualified Data.HashTable.IO           as HI
import qualified Data.Pool                   as RP
import qualified Database.PostgreSQL.LibPQ   as PQ

type PGPool = RP.Pool PGConn

data ConnParams
  = ConnParams
    { cpStripes  :: !Int
    , cpConns    :: !Int
    , cpIdleTime :: !Int
    }
  deriving (Show, Eq)

defaultConnParams :: ConnParams
defaultConnParams = ConnParams 1 20 60

initPGPool :: ConnInfo
           -> ConnParams
           -> IO PGPool
initPGPool ci cp =
  RP.createPool creator destroyer nStripes diffTime nConns
  where
    nStripes  = cpStripes cp
    nConns    = cpConns cp
    creator   = do
      pqConn  <- initPQConn ci
      ctr     <- newIORef 0
      table   <- HI.new
      return $ PGConn pqConn ctr table
    destroyer = PQ.finish . pgPQConn
    diffTime  = fromIntegral $ cpIdleTime cp

-- |
-- Release all connections acquired by the pool.
destroyPGPool :: PGPool -> IO ()
destroyPGPool = RP.destroyAllResources

data PGExecErr
  = PGExecErrConn !PGConnErr
  | PGExecErrTx !PGTxErr
  deriving (Eq)

instance ToJSON PGExecErr where
  toJSON (PGExecErrConn pce) = toJSON pce
  toJSON (PGExecErrTx txe)   = toJSON txe

instance FromPGTxErr PGExecErr where
  fromPGTxErr = PGExecErrTx

instance FromPGConnErr PGExecErr where
  fromPGConnErr = PGExecErrConn

instance Show PGExecErr where
  show (PGExecErrConn pce) = show pce
  show (PGExecErrTx txe)   = show txe

beginTx :: TxMode -> Tx ()
beginTx (i, w) =
  unitQ query () True
  where
    query = fromBuilder $ BB.string7
      ("BEGIN " <> show i <> " " <> maybe "" show w)

commitTx :: Tx ()
commitTx =
  unitQ "COMMIT" () True

abortTx :: Tx ()
abortTx =
  unitQ "ABORT" () True

class FromPGTxErr e where
  fromPGTxErr :: PGTxErr -> e

class FromPGConnErr e where
  fromPGConnErr :: PGConnErr -> e

runTxOnConn :: (FromPGTxErr e, MonadError e m, MonadBaseControl IO m)
            => PGConn
            -> TxMode
            -> (PGConn -> m a)
            -> m a
runTxOnConn pgConn txm f = do
  -- Begin the transaction. If there is an error, you shouldn't call abort
  mapExceptIO fromPGTxErr $ execTx pgConn $ beginTx txm
  -- Run the actual transaction and commit. If there is an error, abort
  flip catchError abort $ do
    a <- f pgConn
    mapExceptIO fromPGTxErr $ execTx pgConn commitTx
    return a
  where
    abort e = do
      mapExceptIO fromPGTxErr $ execTx pgConn abortTx
      throwError e

withConn :: (FromPGTxErr e, FromPGConnErr e, MonadError e m, MonadBaseControl IO m)
         => PGPool
         -> TxMode
         -> (PGConn -> m a)
         -> m a
withConn pool txm f =
  catchConnErr action
  where
    action  = RP.withResource pool $
             \connRsrc -> runTxOnConn connRsrc txm f

catchConnErr :: (FromPGConnErr e, MonadError e m, MonadBaseControl IO m)
             => m a
             -> m a
catchConnErr action =
  control $ \runInIO -> (runInIO action) `catch` (runInIO . handler)
  where
    handler = mkConnExHandler action fromPGConnErr

mkConnExHandler :: (MonadError e m)
                => m a
                -> (PGConnErr -> e)
                -> (PGConnErr -> m a)
mkConnExHandler _ ef = throwError . ef

runTx :: (FromPGTxErr e, FromPGConnErr e, MonadError e m, MonadIO m)
      => PGPool
      -> TxMode
      -> TxE e a
      -> m a
runTx pool txm tx = do
  res <- liftIO $ runExceptT $
         withConn pool txm $ \connRsrc -> runTxOnConn' connRsrc tx
  either throwError return res

runTxOnConn' :: (MonadError e m, MonadBaseControl IO m)
             => PGConn
             -> TxE e a
             -> m a
runTxOnConn' = execTx

sql :: QuasiQuoter
sql = QuasiQuoter { quoteExp = \a -> [|fromString a|] }

sqlFromFile :: FilePath -> Q Exp
sqlFromFile fp = do
  contents <- qAddDependentFile fp >> runIO (readFile fp)
  [| fromString contents |]
