{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.PG.Query.Transaction
    ( TxIsolation(..)
    , TxAccess(..)
    , TxMode
    , PGTxErr(..)
    , getPGStmtErr
    , Tx
    , TxE(..)
    , withNotices
    , withQ
    , withQE
    , rawQ
    , rawQE
    , listQ
    , listQE
    , unitQ
    , unitQE
    , multiQE
    , multiQ
    , discardQ
    , discardQE
    , serverVersion
    , execTx
    , fromBuilder
    , catchE
    , Query
    ) where

import           Database.PG.Query.Class
import           Database.PG.Query.Connection

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Text
import           GHC.Exts

import qualified Data.ByteString.Builder      as BB
import qualified Data.Text                    as T
import qualified Database.PostgreSQL.LibPQ    as PQ

data TxIsolation
  = ReadCommitted
  | RepeatableRead
  | Serializable
  deriving (Eq)

instance Show TxIsolation where
  {-# INLINE show #-}
  show ReadCommitted  = "ISOLATION LEVEL READ COMMITTED"
  show RepeatableRead = "ISOLATION LEVEL REPEATABLE READ"
  show Serializable   = "ISOLATION LEVEL SERIALIZABLE"

data TxAccess
  = ReadWrite
  | ReadOnly
  deriving (Eq)

instance Show TxAccess where
  {-# INLINE show #-}
  show ReadWrite = "READ WRITE"
  show ReadOnly  = "READ ONLY"

type TxMode = (TxIsolation, Maybe TxAccess)

type Tx = TxE PGTxErr

newtype TxE e a
  = TxE { txHandler :: ReaderT PGConn (ExceptT e IO) a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadIO, MonadReader PGConn)

{-# INLINE catchE #-}
catchE :: (e -> e') -> TxE e a -> TxE e' a
catchE f action = TxE $ mapReaderT (withExceptT f) $ txHandler action

data PGTxErr
  = PGTxErr !T.Text ![PrepArg] !Bool !PGErrInternal
  -- | PGCustomErr !T.Text
  deriving (Eq)

{-# INLINE getPGStmtErr #-}
getPGStmtErr :: PGTxErr -> Maybe PGStmtErrDetail
getPGStmtErr (PGTxErr _ _ _ ei) = case ei of
  PGIStatement e  -> return e
  PGIUnexpected _ -> Nothing

instance ToJSON PGTxErr where
  toJSON (PGTxErr stmt args isPrep qe) =
    object [ "statement" .= stmt
           , "arguments" .= map show args
           , "prepared"  .= isPrep
           , "error"     .= qe
           ]

instance Show PGTxErr where
  show = show . encodeToLazyText

{-# INLINE execTx #-}
execTx :: PGConn -> TxE e a -> ExceptT e IO a
execTx conn tx = runReaderT (txHandler tx) conn

newtype Query = Query { getQueryBuilder :: BB.Builder }

instance IsString Query where
  fromString = Query . BB.stringUtf8

{-# INLINE fromBuilder #-}
fromBuilder :: BB.Builder -> Query
fromBuilder = Query

withQ :: (FromRes a, ToPrepArgs r)
      => Query
      -> r
      -> Bool
      -> Tx a
withQ = withQE id

withQE :: (FromRes a, ToPrepArgs r)
       => (PGTxErr -> e)
       -> Query
       -> r
       -> Bool
       -> TxE e a
withQE ef q r prep =
  rawQE ef q args prep
  where
    args = toPrepArgs r

rawQ :: (FromRes a)
     => Query
     -> [PrepArg]
     -> Bool
     -> Tx a
rawQ = rawQE id

rawQE :: (FromRes a)
      => (PGTxErr -> e)
      -> Query
      -> [PrepArg]
      -> Bool
      -> TxE e a
rawQE ef q args prep = TxE $ ReaderT $ \pgConn ->
  withExceptT (ef . txErrF) $
  execQuery pgConn $ PGQuery (mkTemplate stmt) args prep fromRes
  where
    txErrF = PGTxErr (lenientDecodeUtf8 stmt) args prep
    stmt = toByteString $ getQueryBuilder q

multiQE :: (FromRes a)
        => (PGTxErr -> e)
        -> Query
        -> TxE e a
multiQE ef q = TxE $ ReaderT $ \pgConn ->
  withExceptT (ef . txErrF) $
  execMulti pgConn (mkTemplate stmt) fromRes
  where
    txErrF = PGTxErr (lenientDecodeUtf8 stmt) [] False
    stmt = toByteString $ getQueryBuilder q

multiQ :: (FromRes a)
       => Query
       -> Tx a
multiQ = multiQE id

withNotices :: Tx a -> Tx (a, [T.Text])
withNotices tx =  do
  conn <- asks pgPQConn
  setToNotice
  liftIO $ PQ.enableNoticeReporting conn
  a <- tx
  notices <- liftIO $ getNotices conn []
  liftIO $ PQ.disableNoticeReporting conn
  setToWarn
  return (a, map lenientDecodeUtf8 notices)
  where
    setToNotice = unitQE id "SET client_min_messages TO NOTICE;" () False
    setToWarn = unitQE id "SET client_min_messages TO WARNING;" () False
    getNotices conn xs = do
      notice <- PQ.getNotice conn
      case notice of
        Nothing -> return xs
        Just bs -> getNotices conn (bs:xs)

unitQ :: (ToPrepArgs r)
      => Query
      -> r
      -> Bool
      -> Tx ()
unitQ = withQ

unitQE :: (ToPrepArgs r)
       => (PGTxErr -> e)
       -> Query
       -> r
       -> Bool
       -> TxE e ()
unitQE = withQE

discardQ :: (ToPrepArgs r)
         => Query
         -> r
         -> Bool
         -> Tx ()
discardQ t r p= do
  Discard () <- withQ t r p
  return ()

discardQE :: (ToPrepArgs r)
          => (PGTxErr -> e)
          -> Query
          -> r
          -> Bool
          -> TxE e ()
discardQE ef t r p= do
  Discard () <- withQE ef t r p
  return ()

listQ :: (FromRow a, ToPrepArgs r)
      => Query
      -> r
      -> Bool
      -> Tx [a]
listQ = withQ

listQE :: (FromRow a, ToPrepArgs r)
       => (PGTxErr -> e)
       -> Query
       -> r
       -> Bool
       -> TxE e [a]
listQE = withQE

serverVersion
  :: TxE e Int
serverVersion = do
  conn <- asks pgPQConn
  liftIO $ PQ.serverVersion conn
