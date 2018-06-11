{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Database.PG.Query.Connection
    ( initPQConn
    , defaultConnInfo
    , ConnInfo(..)
    , PGQuery(..)
    , PGConn(..)
    , PGConnErr(..)
    , ResultOk(..)
    , getPQRes
    , Template
    , mkTemplate
    , PrepArg
    , prepare
    , execPrepared
    , execMulti
    , execParams
    , execCmd
    , execQuery
    , toByteString
    , lenientDecodeUtf8
    , mapExceptIO
    , PGErrInternal(..)
    , PGStmtErrDetail(..)
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Hashable
import           Data.IORef
import           Data.Monoid
import           Data.Maybe
import           Data.Word
import           GHC.Exts
import           GHC.Generics

import qualified Data.ByteString              as DB
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.HashTable.IO            as HI
import qualified Data.Text                    as DT
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.Encoding.Error     as TE
import qualified Database.PostgreSQL.LibPQ    as PQ

data ConnInfo
  = ConnInfo
    { connHost     :: !String
    , connPort     :: !Int
    , connUser     :: !String
    , connPassword :: !String
    , connDatabase :: !String
    , connOptions  :: !(Maybe String)
    }
  deriving (Eq, Read, Show)

newtype PGConnErr = PGConnErr { getConnErr :: DT.Text }
  deriving (Show, Eq, ToJSON)

instance Exception PGConnErr

newtype PGExecStatus = PGExecStatus PQ.ExecStatus
  deriving (Show, Eq)

instance ToJSON PGExecStatus where
  toJSON (PGExecStatus pqStatus) =
    $(mkToJSON (aesonDrop 0 snakeCase) ''PQ.ExecStatus) pqStatus

mapExceptIO
  :: (MonadBase IO m, MonadError e2 m)
  => (e1 -> e2)
  -> ExceptT e1 IO a
  -> m a
mapExceptIO f action = do
  res <- liftBase $ runExceptT action
  case res of
    Left e  -> throwError $ f e
    Right a -> return a

-- |
-- Establish and initialize a conn.
initPQConn
  :: ConnInfo
  -> IO PQ.Connection
initPQConn ci = do

  -- Initialise the connection
  conn <- PQ.connectdb (pgConnString ci)

  -- Check the status of the connection
  s <- PQ.status conn
  when (s /= PQ.ConnectionOk) $ do
    m <- PQ.errorMessage conn
    let msg = maybe mempty lenientDecodeUtf8 m
    throwIO $ PGConnErr msg

  -- Check the server version
  v <- PQ.serverVersion conn
  when (v < 80200) $
    throwIO $ PGConnErr $ "Unsupported postgres version : " <> fromString (show v)

  -- Set some parameters and check the response
  mRes <- PQ.exec conn $ toByteString $ mconcat
          [ BB.string7 "SET client_encoding = 'UTF8';"
          , BB.string7 "SET client_min_messages TO WARNING;"
          ]
  case mRes of
    Just res -> do
      st <- PQ.resultStatus res
      case st of
        PQ.CommandOk -> return conn
        _            -> throwIO $ PGConnErr "unexpected status after setting params"
    Nothing  -> throwIO $ PGConnErr "unexpected result after setting params"

toByteString :: BB.Builder -> DB.ByteString
toByteString = BL.toStrict . BB.toLazyByteString

defaultConnInfo :: ConnInfo
defaultConnInfo =
  ConnInfo { connHost = "127.0.0.1"
           , connPort = 5432
           , connUser = "postgres"
           , connPassword = ""
           , connDatabase = ""
           , connOptions = Nothing
           }

pgConnString :: ConnInfo -> DB.ByteString
pgConnString connInfo = fromString connstr
  where
    connstr = str "host="     connHost
            $ num "port="     connPort
            $ str "user="     connUser
            $ str "password=" connPassword
            $ str "dbname="   connDatabase
            $ mStr "options=" connOptions
            $ []

    str name field
      | null value = id
      | otherwise  = showString name . quote value . space
        where value = field connInfo

    mStr name field
      | null value = id
      | otherwise  = showString name . quote value . space
        where value = fromMaybe "" (field connInfo)

    num name field
      | value <= 0 = id
      | otherwise  = showString name . shows value . space
        where value = field connInfo

    quote s rest = '\'' : foldr delta ('\'' : rest) s
       where
         delta c cs = case c of
                        '\\' -> '\\' : '\\' : cs
                        '\'' -> '\\' : '\'' : cs
                        _    -> c : cs

    space [] = []
    space xs = ' ':xs

data PGStmtErrDetail
  = PGStmtErrDetail
  { edExecStatus  :: !PGExecStatus
  , edStatusCode  :: !(Maybe DT.Text)
  , edMessage     :: !(Maybe DT.Text)
  , edDescription :: !(Maybe DT.Text)
  , edHint        :: !(Maybe DT.Text)
  } deriving (Show, Eq, Generic)

instance ToJSON PGStmtErrDetail where
   toJSON = genericToJSON $ aesonDrop 2 snakeCase

data ResultOk
  = ResultOkEmpty !PQ.Result
  | ResultOkData !PQ.Result
  deriving (Show, Eq)

getPQRes :: ResultOk -> PQ.Result
getPQRes (ResultOkEmpty res) = res
getPQRes (ResultOkData res)  = res

lenientDecodeUtf8 :: DB.ByteString -> DT.Text
lenientDecodeUtf8 = TE.decodeUtf8With TE.lenientDecode

checkResult
  :: (MonadError PGErrInternal m, MonadBase IO m)
  => PQ.Connection
  -> Maybe PQ.Result
  -> m ResultOk
checkResult conn Nothing = do
  -- This is a fatal error.
  mErr <- liftBase $ PQ.errorMessage conn
  let msg = maybe mempty lenientDecodeUtf8 mErr
  s <- liftBase $ PQ.status conn
  when (s /= PQ.ConnectionOk) $ liftBase $ throwIO $ PGConnErr msg
  throwError $
    PGIUnexpected $ "Fatal, OOM maybe? : " <> msg
checkResult _ (Just res) = do
  st <- liftBase $ PQ.resultStatus res
  -- validate the result status with the given function
  case st of
    PQ.CommandOk     -> return $ ResultOkEmpty res
    PQ.TuplesOk      -> return $ ResultOkData res

    -- Any of these indicate error
    PQ.BadResponse   -> withFullErr st
    PQ.NonfatalError -> withFullErr st
    PQ.FatalError    -> withFullErr st
    PQ.EmptyQuery    -> withFullErr st

    -- Not error, but unexpected status like copy in or copy out
    _                -> throwError $ PGIUnexpected $
                        "Unexpected execStatus : " <> DT.pack (show st)
  where
    errField    = liftBase . PQ.resultErrorField res
    withFullErr st = do
      code <- fmap lenientDecodeUtf8 <$> errField PQ.DiagSqlstate
      msg  <- fmap lenientDecodeUtf8 <$> errField PQ.DiagMessagePrimary
      desc <- fmap lenientDecodeUtf8 <$> errField PQ.DiagMessageDetail
      hint <- fmap lenientDecodeUtf8 <$> errField PQ.DiagMessageHint
      throwError $ PGIStatement $
        PGStmtErrDetail (PGExecStatus st) code msg desc hint

assertResCmd
  :: (MonadError PGErrInternal m, MonadBase IO m)
  => PQ.Connection
  -> Maybe PQ.Result
  -> m ()
assertResCmd conn mRes = do
  resOk <- checkResult conn mRes
  case resOk of
    ResultOkEmpty _ -> return ()
    ResultOkData _  -> throwError $
      PGIUnexpected "cmd expected; tuples found"

-- These are convenient wrappers around LibPQ's similar functions
prepare'
  :: (MonadError PGErrInternal m, MonadBase IO m)
  => PQ.Connection           -- ^ connection
  -> RemoteKey               -- ^ stmtName
  -> Template                -- ^ query
  -> [PQ.Oid]                -- ^ paramTypes
  -> m ()  -- ^ result
prepare' conn rk (Template t) ol = do
  mRes <- liftBase $ PQ.prepare conn rk t $ Just ol
  assertResCmd conn mRes

execPrepared
  :: (MonadError PGErrInternal m, MonadBase IO m)
  => PQ.Connection                -- ^ connection
  -> RemoteKey                    -- ^ stmtName
  -> [Maybe (DB.ByteString, PQ.Format)]           -- ^ parameters
  -> m ResultOk -- ^ result
execPrepared conn n args = do
  mRes <- liftBase $ PQ.execPrepared conn n args PQ.Binary
  checkResult conn mRes

-- Prevents a class of SQL injection attacks
execParams
  :: (MonadError PGErrInternal m, MonadBase IO m)
  => PQ.Connection                 -- ^ connection
  -> Template                      -- ^ statement
  -> [(PQ.Oid, Maybe (DB.ByteString, PQ.Format))]  -- ^ parameters
  -> m ResultOk  -- ^ result
execParams conn (Template t) params = do
  let params' = map (\(ty, v) -> prependToTuple2 ty <$> v) params
  mRes <- liftBase $ PQ.execParams conn t params' PQ.Binary
  checkResult conn mRes
  where
    prependToTuple2 a (b, c) = (a, b, c)

data PGConn
  = PGConn
  { pgPQConn  :: !PQ.Connection
  , pgCounter :: !(IORef Word16)
  , pgTable   :: !RKLookupTable
  }

type RKLookupTable = HI.BasicHashTable LocalKey RemoteKey

-- |
-- Local statement key.
data LocalKey
  = LocalKey !Template ![Word32]
  deriving (Show, Eq)

localKey :: Template -> [PQ.Oid] -> LocalKey
localKey t ol =
  LocalKey t (map oidMapper ol)
  where
    oidMapper (PQ.Oid x) = fromIntegral x

newtype Template
  = Template DB.ByteString
  deriving (Show, Eq, Hashable)

mkTemplate :: DB.ByteString -> Template
mkTemplate = Template

instance Hashable LocalKey where
  hashWithSalt salt (LocalKey template _) =
    hashWithSalt salt template

-- |
-- Remote statement key.
type RemoteKey = DB.ByteString

prepare
  :: (MonadError PGErrInternal m, MonadBase IO m)
  => PGConn
  -> Template
  -> [PQ.Oid]
  -> m RemoteKey
prepare (PGConn conn counter table) t tl = do
  let lk      = localKey t tl
  rkm <- liftBase $ HI.lookup table lk
  case rkm of
    -- Already prepared
    (Just rk) -> return rk
    -- Not found
    Nothing -> do
      w <- liftBase $ readIORef counter
      -- Create a new unique remote key
      let rk = fromString $ show w
      -- prepare the statement
      prepare' conn rk t tl
      -- Insert into table
      liftBase $ HI.insert table lk rk
      -- Increment the counter
      liftBase $ writeIORef counter (succ w)
      return rk

type PrepArg = (PQ.Oid, Maybe (DB.ByteString, PQ.Format))

data PGQuery a
  = PGQuery
  { pgqTemplate   :: !Template
  , pgqArgs       :: [PrepArg]
  , pgqPreparable :: Bool
  , pgqConv       :: ResultOk -> ExceptT DT.Text IO a
  }

data PGErrInternal
  = PGIUnexpected !DT.Text
  | PGIStatement !PGStmtErrDetail
  deriving (Eq)

instance ToJSON PGErrInternal where
  toJSON (PGIUnexpected msg)      = toJSON msg
  toJSON (PGIStatement errDetail) = toJSON errDetail

execQuery
  :: (MonadError PGErrInternal m, MonadBase IO m)
  => PGConn
  -> PGQuery a
  -> m a
execQuery pgConn@(PGConn conn _ _) (PGQuery t params preparable convF) = do
  resOk <- case preparable of
    True -> do
      let (tl, vl) = unzip params
      rk <- prepare pgConn t tl
      execPrepared conn rk vl
    False -> execParams conn t params
  mapExceptIO PGIUnexpected $ convF resOk

execMulti
  :: (MonadError PGErrInternal m, MonadBase IO m)
  => PGConn
  -> Template
  -> (ResultOk -> ExceptT DT.Text IO a)
  -> m a
execMulti (PGConn conn _ _) (Template t) convF = do
  mRes  <- liftBase $ PQ.exec conn t
  resOk <- checkResult conn mRes
  mapExceptIO PGIUnexpected $ convF resOk

execCmd
  :: (MonadError PGErrInternal m, MonadBase IO m)
  => PGConn
  -> Template
  -> m ()
execCmd (PGConn conn _ _) (Template t) = do
  mRes <- liftBase $ PQ.execParams conn t [] PQ.Binary
  assertResCmd conn mRes
