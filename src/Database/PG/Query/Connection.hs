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
    , ConnDetails(..)
    , ConnOptions(..)
    , pgConnString
    , PGQuery(..)
    , PGRetryPolicy
    , mkPGRetryPolicy
    , PGLogEvent(..)
    , PGLogger
    , PGConn(..)
    , resetPGConn
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
    , PGErrInternal(..)
    , PGStmtErrDetail(..)
    ) where

import           Control.Exception
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Bool
import           Data.Hashable
import           Data.IORef
import           Data.Maybe
import           Data.Time
import           Data.Word
import           GHC.Exts
import           GHC.Generics

import qualified Control.Retry             as CR
import qualified Data.ByteString           as DB
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Lazy      as BL
import qualified Data.HashTable.IO         as HI
import qualified Data.Text                 as DT
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Encoding.Error  as TE
import qualified Database.PostgreSQL.LibPQ as PQ

data ConnOptions
  = ConnOptions
    { connHost     :: !String
    , connPort     :: !Int
    , connUser     :: !String
    , connPassword :: !String
    , connDatabase :: !String
    , connOptions  :: !(Maybe String)
    } deriving (Eq, Read, Show)

data ConnDetails
  = CDDatabaseURI !DB.ByteString
  | CDOptions !ConnOptions
  deriving (Show, Read, Eq)

data ConnInfo
  = ConnInfo
  { ciRetries :: !Int
  , ciDetails :: !ConnDetails
  } deriving (Show, Read, Eq)

newtype PGConnErr = PGConnErr { getConnErr :: DT.Text }
  deriving (Show, Eq, ToJSON)

instance Exception PGConnErr

newtype PGExecStatus = PGExecStatus PQ.ExecStatus
  deriving (Show, Eq)

instance ToJSON PGExecStatus where
  toJSON (PGExecStatus pqStatus) =
    $(mkToJSON (aesonDrop 0 snakeCase) ''PQ.ExecStatus) pqStatus

type PGRetryPolicyM m = CR.RetryPolicyM m
type PGRetryPolicy = PGRetryPolicyM (ExceptT PGErrInternal IO)

newtype PGLogEvent
  = PLERetryMsg DT.Text
  deriving (Show, Eq)

type PGLogger = PGLogEvent -> IO ()

type PGError = Either PGErrInternal PGConnErr
type PGExec a = ExceptT PGError IO a

throwPGIntErr
  :: MonadError PGError m => PGErrInternal -> m a
throwPGIntErr = throwError . Left

throwPGConnErr
  :: MonadError PGError m => PGConnErr -> m a
throwPGConnErr = throwError . Right

pgRetrying
  :: (MonadIO m)
  => IO ()
  -> PGRetryPolicyM m
  -> PGLogger
  -> m (Either PGConnErr a)
  -> m a
pgRetrying resetFn retryP logger action = do
  eRes <- CR.retrying retryP shouldRetry $ const action
  either (liftIO . throwIO) return eRes
  where
    shouldRetry rs =
      either (const $ onError rs) (const $ return False)

    onError rs = do
      let retryIterNo = CR.rsIterNumber rs
      liftIO $ do
        logger $ PLERetryMsg $
          "postgres connection failed, retrying("
          <> DT.pack (show retryIterNo) <> ")."
        resetFn
      return True

-- |
-- Establish and initialize a conn.
initPQConn
  :: ConnInfo
  -> PGLogger
  -> IO PQ.Connection
initPQConn ci logger =
  -- Retry if postgres connection error occurs
  pgRetrying resetFn retryP logger $ do

    -- Initialise the connection
    conn <- PQ.connectdb (pgConnString $ ciDetails ci)

    -- Check the status of the connection
    s <- liftIO $ PQ.status conn
    let connOk = s == PQ.ConnectionOk
    bool (whenConnNotOk conn) (whenConnOk conn) connOk
  where
    resetFn = return ()
    retryP = mkPGRetryPolicy $ ciRetries ci

    whenConnNotOk conn = do
      m <- PQ.errorMessage conn
      let msg = maybe mempty lenientDecodeUtf8 m
      return $ Left $ PGConnErr msg

    whenConnOk conn = do
      -- Check the server version
      v <- PQ.serverVersion conn
      let serVerOk = v >= 80200
      bool (whenSerVerNotOk v) (whenSerVerOk conn) serVerOk

    whenSerVerNotOk v =
      throwIO $
        PGConnErr $ "Unsupported postgres version : " <> fromString (show v)

    whenSerVerOk conn = do
      -- Set some parameters and check the response
      mRes <- PQ.exec conn $ toByteString $ mconcat
              [ BB.string7 "SET client_encoding = 'UTF8';"
              , BB.string7 "SET client_min_messages TO WARNING;"
              ]
      case mRes of
        Just res -> do
          st <- PQ.resultStatus res
          case st of
            PQ.CommandOk -> return $ Right conn
            _            -> return $ Left $
                            PGConnErr "unexpected status after setting params"
        Nothing  -> return $ Left $
                    PGConnErr "unexpected result after setting params"

{-# INLINE toByteString #-}
toByteString :: BB.Builder -> DB.ByteString
toByteString = BL.toStrict . BB.toLazyByteString

defaultConnInfo :: ConnInfo
defaultConnInfo = ConnInfo 0 details
  where
    details = CDOptions ConnOptions
              { connHost = "127.0.0.1"
              , connPort = 5432
              , connUser = "postgres"
              , connPassword = ""
              , connDatabase = ""
              , connOptions = Nothing
              }

pgConnString :: ConnDetails -> DB.ByteString
pgConnString (CDDatabaseURI uri) = uri
pgConnString (CDOptions opts)    = fromString connstr
  where
    connstr = str "host="     connHost
            $ num "port="     connPort
            $ str "user="     connUser
            $ str "password=" connPassword
            $ str "dbname="   connDatabase
            $ mStr "options=" connOptions []

    str name field
      | null value = id
      | otherwise  = showString name . quote value . space
        where value = field opts

    mStr name field
      | null value = id
      | otherwise  = showString name . quote value . space
        where value = fromMaybe "" (field opts)

    num name field
      | value <= 0 = id
      | otherwise  = showString name . shows value . space
        where value = field opts

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

{-# INLINE getPQRes #-}
getPQRes :: ResultOk -> PQ.Result
getPQRes (ResultOkEmpty res) = res
getPQRes (ResultOkData res)  = res

{-# INLINE lenientDecodeUtf8 #-}
lenientDecodeUtf8 :: DB.ByteString -> DT.Text
lenientDecodeUtf8 = TE.decodeUtf8With TE.lenientDecode

retryOnConnErr
  :: PGConn
  -> PGExec a
  -> ExceptT PGErrInternal IO a
retryOnConnErr pgConn action =
  pgRetrying resetFn retryP logger $ do
    resE <- lift $ runExceptT action
    case resE of
      Right r                -> return $ Right r
      Left (Left pgIntErr)   -> throwError pgIntErr
      Left (Right pgConnErr) -> return $ Left pgConnErr
  where
    resetFn = resetPGConn pgConn
    PGConn _ _ retryP logger _ _ _ _ = pgConn

checkResult
  :: PQ.Connection
  -> Maybe PQ.Result
  -> PGExec ResultOk
checkResult conn mRes =
  case mRes of
    Nothing -> do
      -- This is a fatal error.
      msg <- getErrMsg
      let whenConnOk = throwPGIntErr $
                   PGIUnexpected $ "Fatal, OOM maybe? : " <> msg
      isConnOk >>= bool (whenConnNotOk msg) whenConnOk

    Just res -> do
      st <- lift $ PQ.resultStatus res
      -- validate the result status with the given function
      case st of
        PQ.CommandOk     -> return $ ResultOkEmpty res
        PQ.TuplesOk      -> return $ ResultOkData res

        -- Any of these indicate error
        PQ.BadResponse   -> withFullErr res st
        PQ.NonfatalError -> withFullErr res st
        PQ.FatalError    -> whenFatalErr res st
        PQ.EmptyQuery    -> withFullErr res st

        -- Not error, but unexpected status like copy in or copy out
        _                -> throwPGIntErr $ PGIUnexpected $
                            "Unexpected execStatus : " <> DT.pack (show st)
  where
    isConnOk = do
      connSt <- lift $ PQ.status conn
      return $ connSt == PQ.ConnectionOk

    getErrMsg = do
      mErr <- lift $ PQ.errorMessage conn
      return $ maybe mempty lenientDecodeUtf8 mErr

    whenConnNotOk = throwPGConnErr . PGConnErr

    whenFatalErr res st = do
      msg <- getErrMsg
      isConnOk >>= bool (whenConnNotOk msg) (withFullErr res st)

    errField res       = lift . PQ.resultErrorField res
    withFullErr res st = do
      code <- fmap lenientDecodeUtf8 <$> errField res PQ.DiagSqlstate
      msg  <- fmap lenientDecodeUtf8 <$> errField res PQ.DiagMessagePrimary
      desc <- fmap lenientDecodeUtf8 <$> errField res PQ.DiagMessageDetail
      hint <- fmap lenientDecodeUtf8 <$> errField res PQ.DiagMessageHint
      throwPGIntErr $ PGIStatement $
        PGStmtErrDetail (PGExecStatus st) code msg desc hint

{-# INLINE assertResCmd #-}
assertResCmd
  :: PQ.Connection
  -> Maybe PQ.Result
  -> PGExec ()
assertResCmd conn mRes = do
  resOk <- checkResult conn mRes
  checkResOk resOk
  where
    checkResOk (ResultOkEmpty _) = return ()
    checkResOk (ResultOkData _) = throwPGIntErr $
      PGIUnexpected "cmd expected; tuples found"

-- These are convenient wrappers around LibPQ's similar functions
{-# INLINE prepare' #-}
prepare'
  :: PQ.Connection  -- ^ connection
  -> RemoteKey      -- ^ stmtName
  -> Template       -- ^ query
  -> [PQ.Oid]       -- ^ paramTypes
  -> PGExec ()      -- ^ result
prepare' conn rk (Template t) ol = do
  mRes <- liftIO $ PQ.prepare conn rk t $ Just ol
  assertResCmd conn mRes

{-# INLINE execPrepared #-}
execPrepared
  :: PQ.Connection                -- ^ connection
  -> [Maybe (DB.ByteString, PQ.Format)]           -- ^ parameters
  -> RemoteKey                    -- ^ stmtName
  -> PGExec ResultOk -- ^ result
execPrepared conn args n = do
  mRes <- lift $ PQ.execPrepared conn n args PQ.Binary
  checkResult conn mRes

-- Prevents a class of SQL injection attacks
execParams
  :: PQ.Connection                 -- ^ connection
  -> Template                      -- ^ statement
  -> [(PQ.Oid, Maybe (DB.ByteString, PQ.Format))]  -- ^ parameters
  -> PGExec ResultOk  -- ^ result
execParams conn (Template t) params = do
  let params' = map (\(ty, v) -> prependToTuple2 ty <$> v) params
  mRes <- lift $ PQ.execParams conn t params' PQ.Binary
  checkResult conn mRes
  where
    prependToTuple2 a (b, c) = (a, b, c)

mkPGRetryPolicy
  :: MonadIO m
  => Int           -- ^ no.of retries
  -> PGRetryPolicyM m
mkPGRetryPolicy noRetries =
    CR.limitRetriesByDelay limitDelay $
    CR.exponentialBackoff baseDelay <> CR.limitRetries noRetries
  where
    -- limitDelay effectively clamps noRetries to <= 6
    limitDelay = 6 * 1000 * 1000 -- 6 seconds
    baseDelay = 100 * 1000 -- 0.1 second

data PGConn
  = PGConn
  { pgPQConn       :: !PQ.Connection
  , pgAllowPrepare :: !Bool
  , pgRetryPolicy  :: !PGRetryPolicy
  , pgLogger       :: !PGLogger
  , pgCounter      :: !(IORef Word16)
  , pgTable        :: !RKLookupTable
  , pgCreatedAt    :: !UTCTime
  , pgMbLifetime   :: !(Maybe NominalDiffTime)
  -- ^ If passed, 'withExpiringPGconn' will destroy the connection when it is older than lifetime.
  }

resetPGConn :: PGConn -> IO ()
resetPGConn (PGConn conn _ _ _ ctr ht _ _) = do
  -- Reset LibPQ connection
  PQ.reset conn
  -- Set counter to 0
  writeIORef ctr 0
  -- Flush all items in hash table
  keys <- map fst <$> HI.toList ht
  forM_ keys $ HI.delete ht

type RKLookupTable = HI.BasicHashTable LocalKey RemoteKey

-- |
-- Local statement key.
data LocalKey
  = LocalKey !Template ![Word32]
  deriving (Show, Eq)

{-# INLINE localKey #-}
localKey :: Template -> [PQ.Oid] -> LocalKey
localKey t ol =
  LocalKey t (map oidMapper ol)
  where
    oidMapper (PQ.Oid x) = fromIntegral x

newtype Template
  = Template DB.ByteString
  deriving (Show, Eq, Hashable)

{-# INLINE mkTemplate #-}
mkTemplate :: DT.Text -> Template
mkTemplate = Template . TE.encodeUtf8

instance Hashable LocalKey where
  hashWithSalt salt (LocalKey template _) =
    hashWithSalt salt template

-- |
-- Remote statement key.
type RemoteKey = DB.ByteString

prepare
  :: PGConn
  -> Template
  -> [PQ.Oid]
  -> PGExec RemoteKey
prepare (PGConn conn _ _ _ counter table _ _) t tl = do
  let lk      = localKey t tl
  rkm <- lift $ HI.lookup table lk
  case rkm of
    -- Already prepared
    (Just rk) -> return rk
    -- Not found
    Nothing -> do
      w <- lift $ readIORef counter
      -- Create a new unique remote key
      let rk = fromString $ show w
      -- prepare the statement
      prepare' conn rk t tl
      lift $ do
        -- Insert into table
        HI.insert table lk rk
        -- Increment the counter
        writeIORef counter (succ w)
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

{-# INLINE execQuery #-}
execQuery
  :: PGConn
  -> PGQuery a
  -> ExceptT PGErrInternal IO a
execQuery pgConn pgQuery = do
  resOk <- retryOnConnErr pgConn $
    bool withoutPrepare withPrepare $ allowPrepare && preparable
  withExceptT PGIUnexpected $ convF resOk
  where
    PGConn conn allowPrepare _ _ _ _ _ _ = pgConn
    PGQuery t params preparable convF = pgQuery
    withoutPrepare = execParams conn t params
    withPrepare = do
      let (tl, vl) = unzip params
      rk <- prepare pgConn t tl
      execPrepared conn vl rk

{-# INLINE execMulti #-}
execMulti
  :: PGConn
  -> Template
  -> (ResultOk -> ExceptT DT.Text IO a)
  -> ExceptT PGErrInternal IO a
execMulti pgConn (Template t) convF = do
  resOk <- retryOnConnErr pgConn $ do
    mRes <- liftIO $ PQ.exec conn t
    checkResult conn mRes
  withExceptT PGIUnexpected $ convF resOk
  where
    conn = pgPQConn pgConn

{-# INLINE execCmd #-}
execCmd
  :: PGConn
  -> Template
  -> ExceptT PGErrInternal IO ()
execCmd pgConn (Template t) =
  retryOnConnErr pgConn $ do
    mRes <- lift $ PQ.execParams conn t [] PQ.Binary
    assertResCmd conn mRes
  where
    conn = pgPQConn pgConn
