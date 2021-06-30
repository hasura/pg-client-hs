{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Database.PG.Query.Pool
  ( ConnParams (..)
  , PGPool
  , pgPoolStats
  , PGPoolStats(..)
  , getInUseConnections
  , withExpiringPGconn
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
  -- * Forcing destroying of connections
  , PGConnectionStale(..)
  ) where

import qualified Data.ByteString               as BS
import qualified Data.HashTable.IO             as HI
import qualified Data.Pool                     as RP
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Database.PostgreSQL.LibPQ     as PQ
import qualified System.Metrics.Distribution   as EKG.Distribution

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.IORef
import           Data.Time
import           GHC.Exts                      (fromString)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           System.Metrics.Distribution   (Distribution)

import           Database.PG.Query.Connection
import           Database.PG.Query.Transaction

data PGPool = PGPool
  { -- | the underlying connection pool
    _pool  :: !(RP.Pool PGConn),
    -- | EKG stats about how we acquire, release, and manage connections
    _stats :: !PGPoolStats
  }

pgPoolStats :: PGPool -> PGPoolStats
pgPoolStats = _stats

-- | Actual ekg gauges and other metrics are not created here, since those depend on
-- a store and it's much simpler to perform the sampling of the distribution from within graphql-engine.
data PGPoolStats = PGPoolStats
  { -- | time taken to acquire new connections from postgres
    _dbConnAcquireLatency   :: !Distribution
  , _poolConnAcquireLatency :: !Distribution
  }

getInUseConnections :: PGPool -> IO Int
getInUseConnections = RP.getInUseResourceCount . _pool

data ConnParams
  = ConnParams
    { cpStripes      :: !Int
    , cpConns        :: !Int
    , cpIdleTime     :: !Int
    -- ^ Connections that sit idle for longer than cpIdleTime may be destroyed.
    , cpAllowPrepare :: !Bool
    , cpMbLifetime   :: !(Maybe NominalDiffTime)
    -- ^ If passed, 'withExpiringPGconn' will destroy the connection when it is older than lifetime.
    , cpTimeout      :: !(Maybe NominalDiffTime)
    -- ^ If passed, 'withConnection' will throw a 'TimeoutException' after 'timeout' seconds.
    , cpCancel       :: !Bool
    -- ^ Transactions will be canceled if a Timeout is received.
    }
  deriving (Show, Eq)

defaultConnParams :: ConnParams
defaultConnParams = ConnParams 1 20 60 True Nothing Nothing True

initPGPoolStats :: IO PGPoolStats
initPGPoolStats = do
  _dbConnAcquireLatency <- EKG.Distribution.new
  _poolConnAcquireLatency <- EKG.Distribution.new
  pure PGPoolStats {..}

initPGPool :: ConnInfo
           -> ConnParams
           -> PGLogger
           -> IO PGPool
initPGPool ci cp logger = do
  _stats <- initPGPoolStats
  _pool <-  RP.createPool' (creator _stats) destroyer nStripes diffTime nConns nTimeout
  pure PGPool {..}
  where
    nStripes  = cpStripes cp
    nConns    = cpConns cp
    nTimeout  = cpTimeout cp
    retryP = mkPGRetryPolicy $ ciRetries ci
    creator stats = do
      createdAt <- getCurrentTime
      pqConn  <- initPQConn ci logger
      connAcquiredAt <- getCurrentTime
      let connAcquiredMillis = realToFrac (1000000 * diffUTCTime connAcquiredAt createdAt)
      EKG.Distribution.add (_dbConnAcquireLatency stats) connAcquiredMillis
      ctr     <- newIORef 0
      table   <- HI.new
      return $ PGConn pqConn (cpAllowPrepare cp) (cpCancel cp) retryP logger ctr table createdAt (cpMbLifetime cp)
    destroyer = PQ.finish . pgPQConn
    diffTime  = fromIntegral $ cpIdleTime cp

-- | Release all connections acquired by the pool.
destroyPGPool :: PGPool -> IO ()
destroyPGPool = RP.destroyAllResources . _pool

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

beginTx :: (MonadIO m) => TxMode -> TxT m ()
beginTx (i, w) =
  unitQ query () True
  where
    query = fromText $ T.pack
      ("BEGIN " <> show i <> " " <> maybe "" show w)

commitTx :: (MonadIO m) => TxT m ()
commitTx =
  unitQ "COMMIT" () True

abortTx :: (MonadIO m) => TxT m ()
abortTx =
  unitQ "ABORT" () True

class FromPGTxErr e where
  fromPGTxErr :: PGTxErr -> e

class FromPGConnErr e where
  fromPGConnErr :: PGConnErr -> e

runTxOnConn :: (MonadIO m, FromPGTxErr e)
            => PGConn
            -> TxMode
            -> (PGConn -> ExceptT e m a)
            -> ExceptT e m a
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

-- | Run a transaction using the postgres pool.
--
-- Catches postgres exceptions and converts them to 'e', including
-- 'TimeoutException's thrown in case the timeout is set and reached.
withConn :: ( MonadIO m
            , MonadBaseControl IO m
            , FromPGTxErr e
            , FromPGConnErr e
            )
         => PGPool
         -> TxMode
         -> (PGConn -> ExceptT e m a)
         -> ExceptT e m a
withConn pool txm f =
  catchConnErr action
  where
    action  = withExpiringPGconn pool $
             \connRsrc -> runTxOnConn connRsrc txm f

catchConnErr :: forall e m a
              . (FromPGConnErr e, MonadError e m, MonadBaseControl IO m)
             => m a
             -> m a
catchConnErr action =
  control $ \runInIO ->
    runInIO action `catches`
      [ Handler (runInIO . handler)
      , Handler (runInIO . handleTimeout)
      ]
  where
    handler = mkConnExHandler action fromPGConnErr

    handleTimeout :: RP.TimeoutException -> m a
    handleTimeout _ =
      throwError (fromPGConnErr $ PGConnErr "connection acquisition timeout expired")

{-# INLINE mkConnExHandler #-}
mkConnExHandler :: (MonadError e m)
                => m a
                -> (PGConnErr -> e)
                -> (PGConnErr -> m a)
mkConnExHandler _ ef = throwError . ef

runTx :: ( MonadIO m
         , MonadBaseControl IO m
         , FromPGTxErr e
         , FromPGConnErr e
         )
      => PGPool
      -> TxMode
      -> TxET e m a
      -> ExceptT e m a
runTx pool txm tx = do
  withConn pool txm $ \connRsrc -> runTxOnConn' connRsrc tx

runTx' :: ( MonadIO m
          , MonadBaseControl IO m
          , FromPGTxErr e
          , FromPGConnErr e
          )
       => PGPool
       -> TxET e m a
       -> ExceptT e m a
runTx' pool tx = do
  catchConnErr $
    withExpiringPGconn pool $ \connRsrc -> execTx connRsrc tx

runTxOnConn' :: PGConn
             -> TxET e m a
             -> ExceptT e m a
runTxOnConn' = execTx

sql :: QuasiQuoter
sql = QuasiQuoter { quoteExp = \a -> [|fromString a|] }

-- | Construct a 'Query' at compile-time from some given file.
--
-- NOTE: This function assumes that the file is UTF-8 encoded.
--
-- Any incompatible character encodings will be rejected at compile-time with
-- a 'TE.UnicodeException' error.
sqlFromFile :: FilePath -> Q Exp
sqlFromFile fp = do
  bytes <- qAddDependentFile fp >> runIO (BS.readFile fp)
  case TE.decodeUtf8' bytes of
    Left err -> throw $! err
    Right txtContents ->
      -- NOTE: This is (effectively) the same implementation as the 'Lift'
      -- instance for 'Text' from 'th-lift-instances'.
      let strContents = T.unpack txtContents
      in [| fromText . T.pack $ strContents |]

-- | 'RP.withResource' for PGPool but implementing a workaround for #5087,
-- optionally expiring the connection after a configurable amount of time so
-- that memory at least can't accumulate unbounded in long-lived connections.
--
-- See ticket for discussion of more long-term solutions.
--
-- Note that idle connections that aren't actively expired here will be
-- destroyed per the timeout policy in Data.Pool.
withExpiringPGconn
  :: (MonadBaseControl IO m, MonadIO m)=> PGPool -> (PGConn -> m a) -> m a
withExpiringPGconn pool f = do
  -- If the connection was stale, we'll discard it and retry, possibly forcing
  -- creation of new connection:
  old <- liftIO getCurrentTime
  handleLifted (\PGConnectionStale -> withExpiringPGconn pool f) $ do
    RP.withResource (_pool pool) $ \connRsrc@PGConn{..} -> do
      now <- liftIO getCurrentTime
      let millis = realToFrac (1000000 * diffUTCTime now old)
      liftIO (EKG.Distribution.add (_poolConnAcquireLatency (_stats pool)) millis)
      let connectionStale =
            maybe False (\lifetime-> now `diffUTCTime` pgCreatedAt > lifetime) pgMbLifetime
      when connectionStale $ do
        -- Throwing is the only way to signal to resource pool to discard the
        -- connection at this time, so we need to use it for control flow:
        throw PGConnectionStale
      -- else proceed with callback:
      f connRsrc
        -- FIXME this segfaults sometimes... see cbits/libpq-bindings.c
        -- Clean up the connection buffers to prevent memory bloat (See #5087):
        -- <* liftIO (unsafeClampInOutBufferSpace pgPQConn)

-- | Used internally (see 'withExpiringPGconn'), but exported in case we need
-- to allow callback to signal that the connection should be destroyed and we
-- should retry.
data PGConnectionStale = PGConnectionStale
  deriving Show

instance Exception PGConnectionStale

-- cribbed from lifted-base
handleLifted :: (MonadBaseControl IO m, Exception e) => (e -> m a) -> m a -> m a
handleLifted handler ma = control $ \runInIO -> handle (runInIO . handler) (runInIO ma)
