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

pattern Abstime :: PQ.Oid
pattern Abstime = PQ.Oid 702

pattern Aclitem :: PQ.Oid
pattern Aclitem = PQ.Oid 1033

pattern Bit :: PQ.Oid
pattern Bit = PQ.Oid 1560

pattern Bool :: PQ.Oid
pattern Bool = PQ.Oid 16

pattern Box :: PQ.Oid
pattern Box = PQ.Oid 603

pattern Bpchar :: PQ.Oid
pattern Bpchar = PQ.Oid 1042

pattern Bytea :: PQ.Oid
pattern Bytea = PQ.Oid 17

pattern Char :: PQ.Oid
pattern Char = PQ.Oid 18

pattern Cid :: PQ.Oid
pattern Cid = PQ.Oid 29

pattern Cidr :: PQ.Oid
pattern Cidr = PQ.Oid 650

pattern Circle :: PQ.Oid
pattern Circle = PQ.Oid 718

pattern Cstring :: PQ.Oid
pattern Cstring = PQ.Oid 2275

pattern Date :: PQ.Oid
pattern Date = PQ.Oid 1082

pattern Daterange :: PQ.Oid
pattern Daterange = PQ.Oid 3912

pattern Float4 :: PQ.Oid
pattern Float4 = PQ.Oid 700

pattern Float8 :: PQ.Oid
pattern Float8 = PQ.Oid 701

pattern Gtsvector :: PQ.Oid
pattern Gtsvector = PQ.Oid 3642

pattern Inet :: PQ.Oid
pattern Inet = PQ.Oid 869

pattern Int2 :: PQ.Oid
pattern Int2 = PQ.Oid 21

pattern Int2vector :: PQ.Oid
pattern Int2vector = PQ.Oid 22

pattern Int4 :: PQ.Oid
pattern Int4 = PQ.Oid 23

pattern Int4range :: PQ.Oid
pattern Int4range = PQ.Oid 3904

pattern Int8 :: PQ.Oid
pattern Int8 = PQ.Oid 20

pattern Int8range :: PQ.Oid
pattern Int8range = PQ.Oid 3926

pattern Interval :: PQ.Oid
pattern Interval = PQ.Oid 1186

pattern Json :: PQ.Oid
pattern Json = PQ.Oid 114

pattern Jsonb :: PQ.Oid
pattern Jsonb = PQ.Oid 3802

pattern Line :: PQ.Oid
pattern Line = PQ.Oid 628

pattern Lseg :: PQ.Oid
pattern Lseg = PQ.Oid 601

pattern Macaddr :: PQ.Oid
pattern Macaddr = PQ.Oid 829

pattern Money :: PQ.Oid
pattern Money = PQ.Oid 790

pattern Name :: PQ.Oid
pattern Name = PQ.Oid 19

pattern Numeric :: PQ.Oid
pattern Numeric = PQ.Oid 1700

pattern Numrange :: PQ.Oid
pattern Numrange = PQ.Oid 3906

pattern Oid :: PQ.Oid
pattern Oid = PQ.Oid 26

pattern Oidvector :: PQ.Oid
pattern Oidvector = PQ.Oid 30

pattern Path :: PQ.Oid
pattern Path = PQ.Oid 602

pattern Point :: PQ.Oid
pattern Point = PQ.Oid 600

pattern Polygon :: PQ.Oid
pattern Polygon = PQ.Oid 604

pattern Record :: PQ.Oid
pattern Record = PQ.Oid 2249

pattern Refcursor :: PQ.Oid
pattern Refcursor = PQ.Oid 1790

pattern Regclass :: PQ.Oid
pattern Regclass = PQ.Oid 2205

pattern Regconfig :: PQ.Oid
pattern Regconfig = PQ.Oid 3734

pattern Regdictionary :: PQ.Oid
pattern Regdictionary = PQ.Oid 3769

pattern Regoper :: PQ.Oid
pattern Regoper = PQ.Oid 2203

pattern Regoperator :: PQ.Oid
pattern Regoperator = PQ.Oid 2204

pattern Regproc :: PQ.Oid
pattern Regproc = PQ.Oid 24

pattern Regprocedure :: PQ.Oid
pattern Regprocedure = PQ.Oid 2202

pattern Regtype :: PQ.Oid
pattern Regtype = PQ.Oid 2206

pattern Reltime :: PQ.Oid
pattern Reltime = PQ.Oid 703

pattern Text :: PQ.Oid
pattern Text = PQ.Oid 25

pattern Tid :: PQ.Oid
pattern Tid = PQ.Oid 27

pattern Time :: PQ.Oid
pattern Time = PQ.Oid 1083

pattern Timestamp :: PQ.Oid
pattern Timestamp = PQ.Oid 1114

pattern Timestamptz :: PQ.Oid
pattern Timestamptz = PQ.Oid 1184

pattern Timetz :: PQ.Oid
pattern Timetz = PQ.Oid 1266

pattern Tinterval :: PQ.Oid
pattern Tinterval = PQ.Oid 704

pattern Tsquery :: PQ.Oid
pattern Tsquery = PQ.Oid 3615

pattern Tsrange :: PQ.Oid
pattern Tsrange = PQ.Oid 3908

pattern Tstzrange :: PQ.Oid
pattern Tstzrange = PQ.Oid 3910

pattern Tsvector :: PQ.Oid
pattern Tsvector = PQ.Oid 3614

pattern Txid_snapshot :: PQ.Oid
pattern Txid_snapshot = PQ.Oid 2970

pattern Unknown :: PQ.Oid
pattern Unknown = PQ.Oid 705

pattern Uuid :: PQ.Oid
pattern Uuid = PQ.Oid 2950

pattern Varbit :: PQ.Oid
pattern Varbit = PQ.Oid 1562

pattern Varchar :: PQ.Oid
pattern Varchar = PQ.Oid 1043

pattern Void :: PQ.Oid
pattern Void = PQ.Oid 2278

pattern Xid :: PQ.Oid
pattern Xid = PQ.Oid 28

pattern Xml :: PQ.Oid
pattern Xml = PQ.Oid 142

-- Array Types

-- | Convert an Oid for a single type into the corresponding type's array Oid.
arrayOf :: PQ.Oid -> PQ.Oid
arrayOf = \case
  Aclitem         -> PQ.Oid 1034
  Auto            -> Auto
  Bit             -> PQ.Oid 1561
  Bool            -> PQ.Oid 1000
  Box             -> PQ.Oid 1020
  Bpchar          -> PQ.Oid 1014
  Bytea           -> PQ.Oid 1001
  Char            -> PQ.Oid 1002
  Cid             -> PQ.Oid 1012
  Cidr            -> PQ.Oid 651
  Circle          -> PQ.Oid 719
  Cstring         -> PQ.Oid 1263
  Date            -> PQ.Oid 1182
  Daterange       -> PQ.Oid 3913
  Float4          -> PQ.Oid 1021
  Float8          -> PQ.Oid 1022
  Gtsvector       -> PQ.Oid 3644
  Inet            -> PQ.Oid 1041
  Int2            -> PQ.Oid 1005
  Int2vector      -> PQ.Oid 1006
  Int4            -> PQ.Oid 1007
  Int4range       -> PQ.Oid 3905
  Int8            -> PQ.Oid 1016
  Int8range       -> PQ.Oid 3927
  Interval        -> PQ.Oid 1187
  Json            -> PQ.Oid 199
  Jsonb           -> PQ.Oid 3807
  Line            -> PQ.Oid 629
  Lseg            -> PQ.Oid 1018
  Macaddr         -> PQ.Oid 1040
  Money           -> PQ.Oid 791
  Name            -> PQ.Oid 1003
  Numeric         -> PQ.Oid 1231
  Numrange        -> PQ.Oid 3907
  Oid             -> PQ.Oid 1028
  Oidvector       -> PQ.Oid 1013
  Path            -> PQ.Oid 1019
  Point           -> PQ.Oid 1017
  Polygon         -> PQ.Oid 1027
  Refcursor       -> PQ.Oid 2201
  Regclass        -> PQ.Oid 2210
  Regconfig       -> PQ.Oid 3735
  Regdictionary   -> PQ.Oid 3770
  Regoper         -> PQ.Oid 2208
  Regoperator     -> PQ.Oid 2209
  Regproc         -> PQ.Oid 1008
  Regprocedure    -> PQ.Oid 2207
  Regtype         -> PQ.Oid 2211
  Text            -> PQ.Oid 1009
  Tid             -> PQ.Oid 1010
  Time            -> PQ.Oid 1183
  Timestamp       -> PQ.Oid 1115
  Timestamptz     -> PQ.Oid 1185
  Timetz          -> PQ.Oid 1270
  Tsquery         -> PQ.Oid 3645
  Tsrange         -> PQ.Oid 3909
  Tstzrange       -> PQ.Oid 3911
  Tsvector        -> PQ.Oid 3643
  Uuid            -> PQ.Oid 2951
  Varbit          -> PQ.Oid 1563
  Varchar         -> PQ.Oid 1015
  Xid             -> PQ.Oid 1011
  Xml             -> PQ.Oid 143
  _               -> Unknown
