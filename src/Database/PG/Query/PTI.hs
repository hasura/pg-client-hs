{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

{-
Description: Oid constants.

All these are taken from this declaration file in the postgres repo:
https://github.com/postgres/postgres/blob/master/src/include/catalog/pg_type.dat
-}
module Database.PG.Query.PTI where

import Database.PostgreSQL.LibPQ qualified as PQ

-- * Constants
--
-- We use pattern synonyms for the Oids so that we can pattern-match on them.

pattern Auto :: PQ.Oid
pattern Auto = PQ.Oid 0

pattern AbsTime :: PQ.Oid
pattern AbsTime = PQ.Oid 702

pattern ACLitem :: PQ.Oid
pattern ACLitem = PQ.Oid 1033

pattern Bit :: PQ.Oid
pattern Bit = PQ.Oid 1560

pattern Bool :: PQ.Oid
pattern Bool = PQ.Oid 16

pattern Box :: PQ.Oid
pattern Box = PQ.Oid 603

pattern BPChar :: PQ.Oid
pattern BPChar = PQ.Oid 1042

pattern Bytea :: PQ.Oid
pattern Bytea = PQ.Oid 17

pattern Char :: PQ.Oid
pattern Char = PQ.Oid 18

pattern CID :: PQ.Oid
pattern CID = PQ.Oid 29

pattern CIDR :: PQ.Oid
pattern CIDR = PQ.Oid 650

pattern Circle :: PQ.Oid
pattern Circle = PQ.Oid 718

pattern CString :: PQ.Oid
pattern CString = PQ.Oid 2275

pattern Date :: PQ.Oid
pattern Date = PQ.Oid 1082

pattern DateRange :: PQ.Oid
pattern DateRange = PQ.Oid 3912

pattern Float4 :: PQ.Oid
pattern Float4 = PQ.Oid 700

pattern Float8 :: PQ.Oid
pattern Float8 = PQ.Oid 701

pattern GTSVector :: PQ.Oid
pattern GTSVector = PQ.Oid 3642

pattern INet :: PQ.Oid
pattern INet = PQ.Oid 869

pattern Int2 :: PQ.Oid
pattern Int2 = PQ.Oid 21

pattern Int2Vector :: PQ.Oid
pattern Int2Vector = PQ.Oid 22

pattern Int4 :: PQ.Oid
pattern Int4 = PQ.Oid 23

pattern Int4Range :: PQ.Oid
pattern Int4Range = PQ.Oid 3904

pattern Int8 :: PQ.Oid
pattern Int8 = PQ.Oid 20

pattern Int8Range :: PQ.Oid
pattern Int8Range = PQ.Oid 3926

pattern Interval :: PQ.Oid
pattern Interval = PQ.Oid 1186

pattern JSON :: PQ.Oid
pattern JSON = PQ.Oid 114

pattern JSONB :: PQ.Oid
pattern JSONB = PQ.Oid 3802

pattern Line :: PQ.Oid
pattern Line = PQ.Oid 628

pattern LSeg :: PQ.Oid
pattern LSeg = PQ.Oid 601

pattern MacAddr :: PQ.Oid
pattern MacAddr = PQ.Oid 829

pattern Money :: PQ.Oid
pattern Money = PQ.Oid 790

pattern Name :: PQ.Oid
pattern Name = PQ.Oid 19

pattern Numeric :: PQ.Oid
pattern Numeric = PQ.Oid 1700

pattern NumRange :: PQ.Oid
pattern NumRange = PQ.Oid 3906

pattern OID :: PQ.Oid
pattern OID = PQ.Oid 26

pattern OIDVector :: PQ.Oid
pattern OIDVector = PQ.Oid 30

pattern Path :: PQ.Oid
pattern Path = PQ.Oid 602

pattern Point :: PQ.Oid
pattern Point = PQ.Oid 600

pattern Polygon :: PQ.Oid
pattern Polygon = PQ.Oid 604

pattern Record :: PQ.Oid
pattern Record = PQ.Oid 2249

pattern RefCursor :: PQ.Oid
pattern RefCursor = PQ.Oid 1790

pattern RegClass :: PQ.Oid
pattern RegClass = PQ.Oid 2205

pattern RegConfig :: PQ.Oid
pattern RegConfig = PQ.Oid 3734

pattern RegDictionary :: PQ.Oid
pattern RegDictionary = PQ.Oid 3769

pattern RegOper :: PQ.Oid
pattern RegOper = PQ.Oid 2203

pattern RegOperator :: PQ.Oid
pattern RegOperator = PQ.Oid 2204

pattern RegProc :: PQ.Oid
pattern RegProc = PQ.Oid 24

pattern RegProcedure :: PQ.Oid
pattern RegProcedure = PQ.Oid 2202

pattern RegType :: PQ.Oid
pattern RegType = PQ.Oid 2206

pattern RelTime :: PQ.Oid
pattern RelTime = PQ.Oid 703

pattern Text :: PQ.Oid
pattern Text = PQ.Oid 25

pattern TID :: PQ.Oid
pattern TID = PQ.Oid 27

pattern Time :: PQ.Oid
pattern Time = PQ.Oid 1083

pattern Timestamp :: PQ.Oid
pattern Timestamp = PQ.Oid 1114

pattern TimestampTZ :: PQ.Oid
pattern TimestampTZ = PQ.Oid 1184

pattern TimeTZ :: PQ.Oid
pattern TimeTZ = PQ.Oid 1266

pattern TInterval :: PQ.Oid
pattern TInterval = PQ.Oid 704

pattern TSQuery :: PQ.Oid
pattern TSQuery = PQ.Oid 3615

pattern TSRange :: PQ.Oid
pattern TSRange = PQ.Oid 3908

pattern TSTZRange :: PQ.Oid
pattern TSTZRange = PQ.Oid 3910

pattern TSVector :: PQ.Oid
pattern TSVector = PQ.Oid 3614

pattern TXIDSnapshot :: PQ.Oid
pattern TXIDSnapshot = PQ.Oid 2970

pattern Unknown :: PQ.Oid
pattern Unknown = PQ.Oid 705

pattern UUID :: PQ.Oid
pattern UUID = PQ.Oid 2950

pattern Varbit :: PQ.Oid
pattern Varbit = PQ.Oid 1562

pattern Varchar :: PQ.Oid
pattern Varchar = PQ.Oid 1043

pattern Void :: PQ.Oid
pattern Void = PQ.Oid 2278

pattern XID :: PQ.Oid
pattern XID = PQ.Oid 28

pattern XML :: PQ.Oid
pattern XML = PQ.Oid 142

-- Array Types

-- | Convert an Oid for a single type into the corresponding type's array Oid.
arrayOf :: PQ.Oid -> PQ.Oid
arrayOf = \case
  Auto            -> Auto
  ACLitem         -> PQ.Oid 1034
  Bit             -> PQ.Oid 1561
  Bool            -> PQ.Oid 1000
  Box             -> PQ.Oid 1020
  BPChar          -> PQ.Oid 1014
  Bytea           -> PQ.Oid 1001
  Char            -> PQ.Oid 1002
  CID             -> PQ.Oid 1012
  CIDR            -> PQ.Oid 651
  Circle          -> PQ.Oid 719
  CString         -> PQ.Oid 1263
  Date            -> PQ.Oid 1182
  DateRange       -> PQ.Oid 3913
  Float4          -> PQ.Oid 1021
  Float8          -> PQ.Oid 1022
  GTSVector       -> PQ.Oid 3644
  INet            -> PQ.Oid 1041
  Int2            -> PQ.Oid 1005
  Int2Vector      -> PQ.Oid 1006
  Int4            -> PQ.Oid 1007
  Int4Range       -> PQ.Oid 3905
  Int8            -> PQ.Oid 1016
  Int8Range       -> PQ.Oid 3927
  Interval        -> PQ.Oid 1187
  JSON            -> PQ.Oid 199
  JSONB           -> PQ.Oid 3807
  Line            -> PQ.Oid 629
  LSeg            -> PQ.Oid 1018
  MacAddr         -> PQ.Oid 1040
  Money           -> PQ.Oid 791
  Name            -> PQ.Oid 1003
  Numeric         -> PQ.Oid 1231
  NumRange        -> PQ.Oid 3907
  OID             -> PQ.Oid 1028
  OIDVector       -> PQ.Oid 1013
  Path            -> PQ.Oid 1019
  Point           -> PQ.Oid 1017
  Polygon         -> PQ.Oid 1027
  RefCursor       -> PQ.Oid 2201
  RegClass        -> PQ.Oid 2210
  RegConfig       -> PQ.Oid 3735
  RegDictionary   -> PQ.Oid 3770
  RegOper         -> PQ.Oid 2208
  RegOperator     -> PQ.Oid 2209
  RegProc         -> PQ.Oid 1008
  RegProcedure    -> PQ.Oid 2207
  RegType         -> PQ.Oid 2211
  Text            -> PQ.Oid 1009
  TID             -> PQ.Oid 1010
  Time            -> PQ.Oid 1183
  Timestamp       -> PQ.Oid 1115
  TimestampTZ     -> PQ.Oid 1185
  TimeTZ          -> PQ.Oid 1270
  TSQuery         -> PQ.Oid 3645
  TSRange         -> PQ.Oid 3909
  TSTZRange       -> PQ.Oid 3911
  TSVector        -> PQ.Oid 3643
  UUID            -> PQ.Oid 2951
  Varbit          -> PQ.Oid 1563
  Varchar         -> PQ.Oid 1015
  XID             -> PQ.Oid 1011
  XML             -> PQ.Oid 143
  _               -> Unknown
