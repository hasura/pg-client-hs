{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Reference:- https://github.com/hasura/skor/blob/master/src/skor.c

module Database.PG.Query.Listen
  ( PGChannel(..)
  , NotifyHandler
  , listen
  )
where

import           Database.PG.Query.Connection
import           Database.PG.Query.Pool
import           Database.PG.Query.Transaction

import           Control.Monad.Except
import           Control.Monad.Trans.Control
import           Data.Pool                     (withResource)
import           Data.String

import qualified Data.Text                     as T
import qualified Database.PostgreSQL.LibPQ     as PQ
import qualified System.Posix.IO.Select        as PS
import qualified System.Posix.IO.Select.FdSet  as PS
import qualified System.Posix.IO.Select.Types  as PS

newtype PGChannel
  = PGChannel {getChannelTxt :: T.Text}
  deriving(Show, Eq, IsString)

type NotifyHandler = PQ.Notify -> IO ()

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
    -- Issue listen command
    eRes <- liftIO $ runExceptT $
            execMulti pgConn (mkTemplate listenCmd) $ const $ return ()
    either throwTxErr return eRes
    forever $ do
      let conn = pgPQConn pgConn
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
        -- Apply handler on arrived notification
        handler n
        -- Process remaining notifications if any
        processNotifs conn

waitForReadReadiness :: PQ.Connection -> ExceptT PGConnErr IO ()
waitForReadReadiness conn = do
  -- Get file descriptor of underlying socket of a connection
  mFd <- lift $ PQ.socket conn
  onJust mFd withFd
  where
    -- Perform select(2) operation on file descriptor
    -- to check whether it is ready for reading
    withFd fd = do
      selRes <- lift $ do
        -- Make file descriptors set
        r <- PS.fromList [fd] --read
        w <- PS.empty -- write
        e <- PS.empty -- exception
        PS.select r w e PS.Never
      case selRes of
        PS.Error -> throwError $ PGConnErr "select() failed"
        _        -> return ()

onJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
onJust Nothing _    = return ()
onJust (Just v) act = act v
