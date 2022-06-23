{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.PG.Query.Class
  ( WithCount (..),
    WithReturning (..),
    FromCol (..),
    fromColHelper,
    FromRow (..),
    FromRes (..),
    ToPrepArg (..),
    toPrepArg,
    ToPrepArgs (..),
    SingleRow (..),
    Discard (..),
    AltJ (..),
    JSON (..),
    JSONB (..),
  )
where

-------------------------------------------------------------------------------

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict, encode)
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Constraint, Type)
import Data.Scientific (Scientific)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Lazy qualified as Lazy (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Word (Word64)
import Database.PG.Query.Connection
import Database.PG.Query.PTI qualified as PTI
import Database.PostgreSQL.LibPQ qualified as PQ
import PostgreSQL.Binary.Decoding qualified as PD
import PostgreSQL.Binary.Encoding qualified as PE
import Prelude

-------------------------------------------------------------------------------

type WithCount :: Type -> Type
data WithCount a = WithCount
  { wcCount :: Word64,
    wcValue :: a
  }
  deriving stock (Eq, Show)

type WithReturning :: Type -> Type
data WithReturning a = WithReturning
  { wrCount :: Word64,
    wrResults :: Maybe a
  }

type SingleRow :: Type -> Type
newtype SingleRow a = SingleRow
  { getRow :: a
  }
  deriving stock (Eq, Show)

type AltJ :: Type -> Type
newtype AltJ a = AltJ {getAltJ :: a}

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither f _ (Left a) = Left $ f a
mapEither _ f (Right b) = Right $ f b

instance (FromJSON a) => FromCol (AltJ a) where
  fromCol = fromColHelper (PD.fn $ mapEither fromString AltJ . eitherDecodeStrict)

type FromCol :: Type -> Constraint
class FromCol a where
  fromCol ::
    Maybe ByteString ->
    Either Text a

fromColHelper :: PD.Value a -> Maybe ByteString -> Either Text a
fromColHelper f = maybe (throwError "encountered null") (PD.valueParser f)

instance FromCol Int where
  fromCol = fromColHelper PD.int

instance FromCol Int32 where
  fromCol = fromColHelper PD.int

instance FromCol Int64 where
  fromCol = fromColHelper PD.int

instance FromCol Int16 where
  fromCol = fromColHelper PD.int

instance FromCol Float where
  fromCol = fromColHelper PD.float4

instance FromCol Double where
  fromCol = fromColHelper PD.float8

instance FromCol Scientific where
  fromCol = fromColHelper PD.numeric

instance FromCol Char where
  fromCol = fromColHelper PD.char

instance FromCol Text where
  fromCol = fromColHelper PD.text_strict

instance FromCol Lazy.Text where
  fromCol = fromColHelper PD.text_lazy

instance FromCol ByteString where
  fromCol = fromColHelper PD.bytea_strict

instance FromCol Lazy.ByteString where
  fromCol = fromColHelper PD.bytea_lazy

instance FromCol Day where
  fromCol = fromColHelper PD.date

instance FromCol TimeOfDay where
  fromCol = fromColHelper PD.time_int

instance FromCol UTCTime where
  fromCol = fromColHelper PD.timestamptz_int

instance FromCol Bool where
  fromCol = fromColHelper PD.bool

instance FromCol UUID where
  fromCol = fromColHelper PD.uuid

instance FromCol a => FromCol (Maybe a) where
  fromCol Nothing = return Nothing
  fromCol bs = Just <$> fromCol bs

type FromRes :: Type -> Constraint
class FromRes a where
  fromRes :: ResultOk -> ExceptT Text IO a

instance FromRes ResultOk where
  fromRes = return

instance FromRes () where
  fromRes (ResultOkEmpty _) =
    return ()
  fromRes (ResultOkData _) =
    throwError "Expecting no data for (). Instead, status is 'TuplesOk'"

instance FromRes Discard where
  fromRes (ResultOkEmpty _) =
    return $ Discard ()
  fromRes (ResultOkData _) =
    return $ Discard ()

type Discard :: Type
newtype Discard = Discard ()
  deriving stock (Eq, Show)

parseWord64 :: ByteString -> Either Text Word64
parseWord64 b = either buildE return parsed
  where
    parsed = Atto.parseOnly (Atto.decimal <* Atto.endOfInput) b
    buildE e = Left $ "Couldn't parse Word64: " <> fromString e

extractCount :: PQ.Result -> ExceptT Text IO Word64
extractCount r = do
  cmd <- liftIO $ PQ.cmdTuples r
  case cmd of
    Just "" -> throwError "Affected rows information not found"
    Nothing -> throwError "Affected rows information not found"
    Just bs -> ExceptT $ return $ parseWord64 bs

instance FromRes a => FromRes (WithReturning a) where
  fromRes (ResultOkEmpty res) = do
    c <- extractCount res
    return $ WithReturning c Nothing
  fromRes rs@(ResultOkData res) = do
    c <- extractCount res
    r <- fromRes rs
    return $ WithReturning c $ Just r

instance FromRes a => FromRes (WithCount a) where
  fromRes resOk = do
    let res = getPQRes resOk
    a <- fromRes resOk
    c <- extractCount res
    return $ WithCount c a

type ResultMatrix :: Type
type ResultMatrix = V.Vector ResultRow

type ResultRow :: Type
type ResultRow = V.Vector (Maybe ByteString)

{-# INLINE colInt #-}
colInt :: PQ.Column -> Int
colInt (PQ.Col n) = fromIntegral n

{-# INLINE rowInt #-}
rowInt :: PQ.Row -> Int
rowInt (PQ.Row n) = fromIntegral n

-- Should be used only after checking
-- the status
buildMat :: PQ.Result -> IO ResultMatrix
buildMat r = do
  nr <- PQ.ntuples r
  nc <- PQ.nfields r
  mvx <- VM.unsafeNew (rowInt nr)
  for_ [0 .. pred nr] $ \ir -> do
    mvy <- VM.unsafeNew (colInt nc)
    for_ [0 .. pred nc] $ \ic ->
      VM.unsafeWrite mvy (colInt ic) =<< PQ.getvalue r ir ic
    vy <- V.unsafeFreeze mvy
    VM.unsafeWrite mvx (rowInt ir) vy
  V.unsafeFreeze mvx

instance FromRow a => FromRes [a] where
  fromRes (ResultOkEmpty _) =
    throwError "Expecting data. Instead, status is 'CommandOk'"
  fromRes (ResultOkData res) = do
    rm <- liftIO $ buildMat res
    ExceptT $ return $ fmap V.toList $ sequence $ V.map fromRow rm

instance FromRow a => FromRes (V.Vector a) where
  fromRes (ResultOkEmpty _) =
    throwError "Expecting data. Instead, status is 'CommandOk'"
  fromRes (ResultOkData res) = do
    rm <- liftIO $ buildMat res
    ExceptT $ return $ sequence $ V.map fromRow rm

instance FromRow a => FromRes (SingleRow a) where
  fromRes (ResultOkEmpty _) =
    throwError "Expecting data. Instead, status is 'CommandOk'"
  fromRes (ResultOkData res) = do
    rm <- liftIO $ buildMat res
    if V.length rm == 1
      then ExceptT $ return $ SingleRow <$> fromRow (rm V.! 0)
      else throwError "Rows returned != 1"

instance FromRow a => FromRes (Maybe a) where
  fromRes (ResultOkEmpty _) =
    throwError "Expecting data. Instead, status is 'CommandOk'"
  fromRes (ResultOkData res) = do
    rm <- liftIO $ buildMat res
    case V.length rm of
      0 -> ExceptT $ return $ Right Nothing
      1 -> ExceptT $ return $ Just <$> fromRow (rm V.! 0)
      _ -> throwError "Rows returned > 1"

colMismatch :: Int -> Int -> Text
colMismatch expected actual =
  fromString $
    mconcat
      [ "Expected ",
        show expected,
        " column(s), but found : ",
        show actual
      ]

instance FromCol a => FromRow (Identity a) where
  fromRow row = case V.length row of
    1 -> fmap Identity $ fromCol $ row V.! 0
    c -> throwError $ colMismatch 1 c

instance (FromCol a, FromCol b) => FromRow (a, b) where
  fromRow row = case V.length row of
    2 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      return (a, b)
    c -> throwError $ colMismatch 2 c

instance (FromCol a, FromCol b, FromCol c) => FromRow (a, b, c) where
  fromRow row = case V.length row of
    3 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      return (a, b, c)
    c -> throwError $ colMismatch 3 c

instance (FromCol a, FromCol b, FromCol c, FromCol d) => FromRow (a, b, c, d) where
  fromRow row = case V.length row of
    4 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      return (a, b, c, d)
    c -> throwError $ colMismatch 4 c

instance (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e) => FromRow (a, b, c, d, e) where
  fromRow row = case V.length row of
    5 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      return (a, b, c, d, e)
    c -> throwError $ colMismatch 5 c

instance (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f) => FromRow (a, b, c, d, e, f) where
  fromRow row = case V.length row of
    6 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      return (a, b, c, d, e, f)
    c -> throwError $ colMismatch 6 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g) =>
  FromRow (a, b, c, d, e, f, g)
  where
  fromRow row = case V.length row of
    7 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      return (a, b, c, d, e, f, g)
    c -> throwError $ colMismatch 7 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h) =>
  FromRow (a, b, c, d, e, f, g, h)
  where
  fromRow row = case V.length row of
    8 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      return (a, b, c, d, e, f, g, h)
    c -> throwError $ colMismatch 8 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h, FromCol i) =>
  FromRow (a, b, c, d, e, f, g, h, i)
  where
  fromRow row = case V.length row of
    9 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      i <- fromCol $ row V.! 8
      return (a, b, c, d, e, f, g, h, i)
    c -> throwError $ colMismatch 9 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h, FromCol i, FromCol j) =>
  FromRow (a, b, c, d, e, f, g, h, i, j)
  where
  fromRow row = case V.length row of
    10 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      i <- fromCol $ row V.! 8
      j <- fromCol $ row V.! 9
      return (a, b, c, d, e, f, g, h, i, j)
    c -> throwError $ colMismatch 10 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h, FromCol i, FromCol j, FromCol k) =>
  FromRow (a, b, c, d, e, f, g, h, i, j, k)
  where
  fromRow row = case V.length row of
    11 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      i <- fromCol $ row V.! 8
      j <- fromCol $ row V.! 9
      k <- fromCol $ row V.! 10
      return (a, b, c, d, e, f, g, h, i, j, k)
    c -> throwError $ colMismatch 11 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h, FromCol i, FromCol j, FromCol k, FromCol l) =>
  FromRow (a, b, c, d, e, f, g, h, i, j, k, l)
  where
  fromRow row = case V.length row of
    12 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      i <- fromCol $ row V.! 8
      j <- fromCol $ row V.! 9
      k <- fromCol $ row V.! 10
      l <- fromCol $ row V.! 11
      return (a, b, c, d, e, f, g, h, i, j, k, l)
    c -> throwError $ colMismatch 12 c

type FromRow :: Type -> Constraint
class FromRow a where
  fromRow :: ResultRow -> Either Text a

type ToPrepArgs :: Type -> Constraint
class ToPrepArgs a where
  toPrepArgs :: a -> [PrepArg]

instance ToPrepArgs () where
  toPrepArgs _ = []

-- | Convert a value into a prepared query argument.
toPrepArg :: forall a. ToPrepArg a => a -> PrepArg
toPrepArg x = (oid @a, fmap go (encoder x))
  where go e = (PE.encodingBytes e, PQ.Binary)

-- | Tools for preparing values to be query parameters. A prepared query
-- parameter (or 'PrepArg') contains an Oid (allowing Postgres to deduce its
-- type), a 'ByteString' (the serialised value) and a format flag (text or
-- binary). In our case, the format is always binary, so it really comes down
-- to the first two: what is its type, and how do we encode it?
type ToPrepArg :: Type -> Constraint
class ToPrepArg a where

  -- | Encode a value with a postgres encoder. If the value is missing (i.e.
  -- 'Nothing'), the encoder should also be missing.
  encoder :: a -> Maybe PE.Encoding

  -- | Get the OID for this type. Because we won't always have a value (e.g.
  -- when we are dealing with an empty list of values), this function must be
  -- called with a visible type parameter. For example, @'oid' @'Double'@.
  oid :: PQ.Oid

instance (ToJSON a) => ToPrepArg (AltJ a) where
  encoder (AltJ x) = pure . PE.bytea_lazy $ encode x
  oid = PTI.JSON

instance ToPrepArg a => ToPrepArg [a] where
  encoder = pure . PE.array_foldable (fromIntegral cuint) encoder
    where PQ.Oid cuint = oid @a

  oid = PTI.arrayOf (oid @a)

instance ToPrepArg Word64 where
  encoder = pure . PE.int8_word64
  oid = PTI.Int8

instance ToPrepArg Int64 where
  encoder = pure . PE.int8_int64
  oid = PTI.Int8

instance ToPrepArg Int32 where
  encoder = pure . PE.int4_int32
  oid = PTI.Int4

instance ToPrepArg Int16 where
  encoder = pure . PE.int2_int16
  oid = PTI.Int2

instance ToPrepArg Float where
  encoder = pure . PE.float4
  oid = PTI.Float4

instance ToPrepArg Double where
  encoder = pure . PE.float8
  oid = PTI.Float8

instance ToPrepArg Scientific where
  encoder = pure . PE.numeric
  oid = PTI.Numeric

instance ToPrepArg Char where
  encoder = pure . PE.char_utf8
  oid = PTI.Text

instance ToPrepArg Text where
  encoder = pure . PE.text_strict
  oid = PTI.Text

instance ToPrepArg Lazy.Text where
  encoder = pure . PE.text_lazy
  oid = PTI.Text

instance ToPrepArg ByteString where
  encoder = pure . PE.bytea_strict
  oid = PTI.Bytea

instance ToPrepArg Lazy.ByteString where
  encoder = pure . PE.bytea_lazy
  oid = PTI.Bytea

instance ToPrepArg LocalTime where
  encoder = pure . PE.timestamp_int
  oid = PTI.Timestamp

instance ToPrepArg UTCTime where
  encoder = pure . PE.timestamptz_int
  oid = PTI.TimestampTZ

instance ToPrepArg Bool where
  encoder = pure . PE.bool
  oid = PTI.Bool

instance ToPrepArg Day where
  encoder = pure . PE.date
  oid = PTI.Date

instance ToPrepArg UUID where
  encoder = pure . PE.uuid
  oid = PTI.UUID

type JSON :: Type
newtype JSON = JSON Value
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

type JSONB :: Type
newtype JSONB = JSONB Value
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

instance ToPrepArg JSON where
  encoder (JSON j) = pure (PE.json_ast j)
  oid = PTI.JSON

instance ToPrepArg JSONB where
  encoder (JSONB j) = pure (PE.jsonb_ast j)
  oid = PTI.JSONB

instance (ToPrepArg a) => ToPrepArg (Maybe a) where
  encoder x = x >>= encoder
  oid = oid @a

instance (ToPrepArg a) => ToPrepArgs [a] where
  toPrepArgs = map toPrepArg

instance (ToPrepArg a) => ToPrepArgs (Identity a) where
  toPrepArgs (Identity a) =
    [toPrepArg a]

instance (ToPrepArg a, ToPrepArg b) => ToPrepArgs (a, b) where
  toPrepArgs (a, b) =
    [ toPrepArg a,
      toPrepArg b
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c) => ToPrepArgs (a, b, c) where
  toPrepArgs (a, b, c) =
    [ toPrepArg a,
      toPrepArg b,
      toPrepArg c
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d) => ToPrepArgs (a, b, c, d) where
  toPrepArgs (a, b, c, d) =
    [ toPrepArg a,
      toPrepArg b,
      toPrepArg c,
      toPrepArg d
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e) => ToPrepArgs (a, b, c, d, e) where
  toPrepArgs (a, b, c, d, e) =
    [ toPrepArg a,
      toPrepArg b,
      toPrepArg c,
      toPrepArg d,
      toPrepArg e
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f) => ToPrepArgs (a, b, c, d, e, f) where
  toPrepArgs (a, b, c, d, e, f) =
    [ toPrepArg a,
      toPrepArg b,
      toPrepArg c,
      toPrepArg d,
      toPrepArg e,
      toPrepArg f
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f, ToPrepArg g) => ToPrepArgs (a, b, c, d, e, f, g) where
  toPrepArgs (a, b, c, d, e, f, g) =
    [ toPrepArg a,
      toPrepArg b,
      toPrepArg c,
      toPrepArg d,
      toPrepArg e,
      toPrepArg f,
      toPrepArg g
    ]

instance
  (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f, ToPrepArg g, ToPrepArg h) =>
  ToPrepArgs (a, b, c, d, e, f, g, h)
  where
  toPrepArgs (a, b, c, d, e, f, g, h) =
    [ toPrepArg a,
      toPrepArg b,
      toPrepArg c,
      toPrepArg d,
      toPrepArg e,
      toPrepArg f,
      toPrepArg g,
      toPrepArg h
    ]

instance
  (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f, ToPrepArg g, ToPrepArg h, ToPrepArg i) =>
  ToPrepArgs (a, b, c, d, e, f, g, h, i)
  where
  toPrepArgs (a, b, c, d, e, f, g, h, i) =
    [ toPrepArg a,
      toPrepArg b,
      toPrepArg c,
      toPrepArg d,
      toPrepArg e,
      toPrepArg f,
      toPrepArg g,
      toPrepArg h,
      toPrepArg i
    ]

instance
  (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f, ToPrepArg g, ToPrepArg h, ToPrepArg i, ToPrepArg j) =>
  ToPrepArgs (a, b, c, d, e, f, g, h, i, j)
  where
  toPrepArgs (a, b, c, d, e, f, g, h, i, j) =
    [ toPrepArg a,
      toPrepArg b,
      toPrepArg c,
      toPrepArg d,
      toPrepArg e,
      toPrepArg f,
      toPrepArg g,
      toPrepArg h,
      toPrepArg i,
      toPrepArg j
    ]
