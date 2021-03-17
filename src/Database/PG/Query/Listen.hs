{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Reference:- https://github.com/hasura/skor/blob/master/src/skor.c
--
-- See also Ex 31-2 in: https://www.postgresql.org/docs/9.1/libpq-example.html

module Database.PG.Query.Listen
  ( PGChannel(..)
  , NotifyHandler
  , OnStartHandler
  , listen
  )
where

import           Database.PG.Query.Connection
import           Database.PG.Query.Pool
import           Database.PG.Query.Transaction

import           Control.Exception             (displayException, try)
import           Control.Monad.Except
import           Control.Monad.Trans.Control
import           Data.String
import           GHC.Conc.IO                   (threadWaitRead)

import qualified Data.Text                     as T
import qualified Database.PostgreSQL.LibPQ     as PQ

newtype PGChannel
  = PGChannel {getChannelTxt :: T.Text}
  deriving(Show, Eq, IsString)

-- | A callback to run on each event
type NotifyHandler = PQ.Notify -> IO ()

-- | A callback run at most once per call to 'listen', after @LISTEN@ has been
-- issued and before any 'NotifyHandler' are called
type OnStartHandler = IO ()

-- | LISTEN on given channel. This never exits, except in the case of exceptions.
listen
  :: ( FromPGConnErr e
     , FromPGTxErr e
     , MonadError e m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => PGPool -> PGChannel -> OnStartHandler -> NotifyHandler -> m void
listen pool channel onStartHandler notifyHandler = catchConnErr $
  withExpiringPGconn pool $ \pgConn -> do
    let conn = pgPQConn pgConn

    -- Issue listen command
    eRes <- liftIO $ runExceptT $
            execMulti pgConn (mkTemplate listenCmd) $ const $ return ()
    either throwTxErr return eRes
    -- Now we're subscribed...
    liftIO onStartHandler
    forever $ do
      r <- liftIO $ runExceptT $ waitForData conn
      either (throwError . fromPGConnErr) return r
      -- Read data now waiting on the socket:
      success <- liftIO $ PQ.consumeInput conn
      unless success throwConsumeFailed
      liftIO $ processNotifs conn
  where
    listenCmd = "LISTEN  " <> getChannelTxt channel <> ";"
    throwTxErr =
      throwError . fromPGTxErr . PGTxErr listenCmd [] False
    throwConsumeFailed = throwError $ fromPGConnErr $
      PGConnErr "consuming input failed from postgres connection"

    processNotifs conn = do
      -- Collect notification
      mNotify <- PQ.notifies conn
      onJust mNotify $ \n -> do
        -- Apply notify handler on arrived notification
        notifyHandler n
        -- Process remaining notifications if any
        processNotifs conn

waitForData :: PQ.Connection -> ExceptT PGConnErr IO ()
waitForData conn = do
  -- Get file descriptor of underlying socket of a connection
  mFd <- lift $ PQ.socket conn
  fd <- maybe (throwError $ PGConnErr "connection is not currently open") pure mFd
  -- Block, waiting for there to be data to read on the socket:
  waitResult <- lift . try $ threadWaitRead fd
  either (throwError . ioErrorToPGConnErr) return waitResult
  where
    ioErrorToPGConnErr :: IOError -> PGConnErr
    ioErrorToPGConnErr = PGConnErr . T.pack . displayException

onJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
onJust Nothing _    = return ()
onJust (Just v) act = act v
