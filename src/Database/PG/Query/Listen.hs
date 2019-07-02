{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Reference:- https://github.com/hasura/skor/blob/master/src/skor.c

module Database.PG.Query.Listen
  ( PGChannel(..)
  , NotifyHandler
  , PGNotifyEvent(..)
  , listen
  )
where

import           Database.PG.Query.Connection
import           Database.PG.Query.Pool
import           Database.PG.Query.Transaction

import           Control.Exception             (displayException, try)
import           Control.Monad.Except
import           Control.Monad.Trans.Control
import           Data.Pool                     (withResource)
import           Data.String
import           GHC.Conc.IO                   (threadWaitRead)

import qualified Data.Text                     as T
import qualified Database.PostgreSQL.LibPQ     as PQ

newtype PGChannel
  = PGChannel {getChannelTxt :: T.Text}
  deriving(Show, Eq, IsString)

data PGNotifyEvent
  = PNEOnStart
  | PNEPQNotify !PQ.Notify
  deriving Show

type NotifyHandler = PGNotifyEvent -> IO ()

-- | listen on given channel
listen
  :: ( FromPGConnErr e
     , FromPGTxErr e
     , MonadError e m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => PGPool -> PGChannel -> NotifyHandler -> m ()
listen pool channel handler = catchConnErr $
  withResource pool $ \pgConn -> do
    let conn = pgPQConn pgConn

    -- Issue listen command
    eRes <- liftIO $ runExceptT $
            execMulti pgConn (mkTemplate listenCmd) $ const $ return ()
    either throwTxErr return eRes
    -- Emit onStart event
    liftIO $ handler PNEOnStart
    forever $ do
      -- Make postgres connection ready for reading
      r <- liftIO $ runExceptT $ waitForReadReadiness conn
      either (throwError . fromPGConnErr) return r
      -- Check for input
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
        handler $ PNEPQNotify n
        -- Process remaining notifications if any
        processNotifs conn

waitForReadReadiness :: PQ.Connection -> ExceptT PGConnErr IO ()
waitForReadReadiness conn = do
  -- Get file descriptor of underlying socket of a connection
  mFd <- lift $ PQ.socket conn
  fd <- maybe (throwError $ PGConnErr "connection is not currently open") pure mFd
  -- Wait for the socket to be ready for reading
  waitResult <- lift . try $ threadWaitRead fd
  either (throwError . ioErrorToPGConnErr) return waitResult
  where
    ioErrorToPGConnErr :: IOError -> PGConnErr
    ioErrorToPGConnErr = PGConnErr . T.pack . displayException

onJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
onJust Nothing _    = return ()
onJust (Just v) act = act v
