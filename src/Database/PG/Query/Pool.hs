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
  , runTx'
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
import           GHC.Exts                      (fromString)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import qualified Data.HashTable.IO             as HI
import qualified Data.Pool                     as RP
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.LibPQ     as PQ

type PGPool = RP.Pool PGConn

data ConnParams
  = ConnParams
    { cpStripes      :: !Int
    , cpConns        :: !Int
    , cpIdleTime     :: !Int
    , cpAllowPrepare :: !Bool
    }
  deriving (Show, Eq)

defaultConnParams :: ConnParams
defaultConnParams = ConnParams 1 20 60 True

initPGPool :: ConnInfo
           -> ConnParams
           -> PGLogger
           -> IO PGPool
initPGPool ci cp logger =
  RP.createPool creator destroyer nStripes diffTime nConns
  where
    nStripes  = cpStripes cp
    nConns    = cpConns cp
    retryP = mkPGRetryPolicy $ ciRetries ci
    creator   = do
      pqConn  <- initPQConn ci logger
      ctr     <- newIORef 0
      table   <- HI.new
      return $ PGConn pqConn (cpAllowPrepare cp) retryP logger ctr table
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
    query = fromText $ T.pack
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

runTxOnConn :: (FromPGTxErr e)
            => PGConn
            -> TxMode
            -> (PGConn -> ExceptT e IO a)
            -> ExceptT e IO a
runTxOnConn pgConn txm f = do
  -- Begin the transaction. If there is an error, you shouldn't call abort
  withExceptT fromPGTxErr $ execTx pgConn $ beginTx txm
  -- Run the actual transaction and commit. If there is an error, abort
  flip catchError abort $ do
    a <- f pgConn
    withExceptT fromPGTxErr $ execTx pgConn commitTx
    return a
  where
    abort e = do
      withExceptT fromPGTxErr $ execTx pgConn abortTx
      throwError e

withConn :: (FromPGTxErr e, FromPGConnErr e)
         => PGPool
         -> TxMode
         -> (PGConn -> ExceptT e IO a)
         -> ExceptT e IO a
withConn pool txm f =
  catchConnErr action
  where
    action  = RP.withResource pool $
             \connRsrc -> runTxOnConn connRsrc txm f

catchConnErr :: (FromPGConnErr e, MonadError e m, MonadBaseControl IO m)
             => m a
             -> m a
catchConnErr action =
  control $ \runInIO -> runInIO action `catch` (runInIO . handler)
  where
    handler = mkConnExHandler action fromPGConnErr

{-# INLINE mkConnExHandler #-}
mkConnExHandler :: (MonadError e m)
                => m a
                -> (PGConnErr -> e)
                -> (PGConnErr -> m a)
mkConnExHandler _ ef = throwError . ef

runTx :: (FromPGTxErr e, FromPGConnErr e)
      => PGPool
      -> TxMode
      -> TxE e a
      -> ExceptT e IO a
runTx pool txm tx = do
  res <- liftIO $ runExceptT $
         withConn pool txm $ \connRsrc -> runTxOnConn' connRsrc tx
  either throwError return res

runTx' :: (FromPGTxErr e, FromPGConnErr e)
       => PGPool
       -> TxE e a
       -> ExceptT e IO a
runTx' pool tx = do
  res <- liftIO $ runExceptT $ catchConnErr $
         RP.withResource pool $ \connRsrc -> execTx connRsrc tx
  either throwError return res

runTxOnConn' :: PGConn
             -> TxE e a
             -> ExceptT e IO a
runTxOnConn' = execTx

sql :: QuasiQuoter
sql = QuasiQuoter { quoteExp = \a -> [|fromString a|] }

sqlFromFile :: FilePath -> Q Exp
sqlFromFile fp = do
  contents <- qAddDependentFile fp >> runIO (readFile fp)
  [| fromString contents |]
