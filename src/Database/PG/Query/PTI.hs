{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

{-
Description: Oid constants.

All these are taken from this declaration file in the postgres repo:
https://github.com/postgres/postgres/blob/master/src/include/catalog/pg_type.dat
-}
module Database.PG.Query.PTI where

-------------------------------------------------------------------------------

import Database.PostgreSQL.LibPQ qualified as PQ
import Prelude

-------------------------------------------------------------------------------
-- * Constants
--
-- We use pattern synonyms for the Oids so that we can pattern-match on them.

pattern Auto :: PQ.Oid
pattern Auto = PQ.Oid 0

pattern Bool :: PQ.Oid
pattern Bool = PQ.Oid 16 -- array_type_oid => '1000',

pattern Bytea :: PQ.Oid
pattern Bytea = PQ.Oid 17 --  array_type_oid => '1001',

pattern Char :: PQ.Oid
pattern Char = PQ.Oid 18 -- , array_type_oid => '1002', descr => 'single character',

pattern Name :: PQ.Oid
pattern Name = PQ.Oid 19 -- , array_type_oid => '1003',

pattern Int8 :: PQ.Oid
pattern Int8 = PQ.Oid 20 -- , array_type_oid => '1016',

pattern Int2 :: PQ.Oid
pattern Int2 = PQ.Oid 21 -- , array_type_oid => '1005',

pattern Int2vector :: PQ.Oid
pattern Int2vector = PQ.Oid 22 -- , array_type_oid => '1006',

pattern Int4 :: PQ.Oid
pattern Int4 = PQ.Oid 23 -- , array_type_oid => '1007',

pattern Regproc :: PQ.Oid
pattern Regproc = PQ.Oid 24 -- , array_type_oid => '1008', descr => 'registered procedure',

pattern Text :: PQ.Oid
pattern Text = PQ.Oid 25 -- , array_type_oid => '1009',

pattern Oid :: PQ.Oid
pattern Oid = PQ.Oid 26 -- , array_type_oid => '1028',

pattern Tid :: PQ.Oid
pattern Tid = PQ.Oid 27 -- , array_type_oid => '1010',

pattern Xid :: PQ.Oid
pattern Xid = PQ.Oid 28 -- , array_type_oid => '1011', descr => 'transaction id',

pattern Cid :: PQ.Oid
pattern Cid = PQ.Oid 29 -- , array_type_oid => '1012',

pattern Oidvector :: PQ.Oid
pattern Oidvector = PQ.Oid 30 -- , array_type_oid => '1013',

pattern Json :: PQ.Oid
pattern Json = PQ.Oid 114 -- , array_type_oid => '199', descr => 'JSON stored as text',

pattern Xml :: PQ.Oid
pattern Xml = PQ.Oid 142 -- , array_type_oid => '143', descr => 'XML content',

pattern Xid8 :: PQ.Oid
pattern Xid8 = PQ.Oid 5069 -- , array_type_oid => '271', descr => 'full transaction id',

pattern Point :: PQ.Oid
pattern Point = PQ.Oid 600 -- , array_type_oid => '1017',

pattern Lseg :: PQ.Oid
pattern Lseg = PQ.Oid 601 -- , array_type_oid => '1018',

pattern Path :: PQ.Oid
pattern Path = PQ.Oid 602 -- , array_type_oid => '1019',

pattern Box :: PQ.Oid
pattern Box = PQ.Oid 603 -- , array_type_oid => '1020',

pattern Polygon :: PQ.Oid
pattern Polygon = PQ.Oid 604 -- , array_type_oid => '1027',

pattern Line :: PQ.Oid
pattern Line = PQ.Oid 628 -- , array_type_oid => '629', descr => 'geometric line',

pattern Float4 :: PQ.Oid
pattern Float4 = PQ.Oid 700 -- , array_type_oid => '1021',

pattern Float8 :: PQ.Oid
pattern Float8 = PQ.Oid 701 -- , array_type_oid => '1022',

pattern Unknown :: PQ.Oid
pattern Unknown = PQ.Oid 705 -- , descr => 'pseudo-type representing an undetermined type',

pattern Circle :: PQ.Oid
pattern Circle = PQ.Oid 718 -- , array_type_oid => '719',

pattern Money :: PQ.Oid
pattern Money = PQ.Oid 790 -- , array_type_oid => '791',

pattern Macaddr :: PQ.Oid
pattern Macaddr = PQ.Oid 829 -- , array_type_oid => '1040',

pattern Inet :: PQ.Oid
pattern Inet = PQ.Oid 869 -- , array_type_oid => '1041',

pattern Cidr :: PQ.Oid
pattern Cidr = PQ.Oid 650 -- , array_type_oid => '651',

pattern Macaddr8 :: PQ.Oid
pattern Macaddr8 = PQ.Oid 774 -- , array_type_oid => '775',

pattern Aclitem :: PQ.Oid
pattern Aclitem = PQ.Oid 1033 -- , array_type_oid => '1034', descr => 'access control list',

pattern Bpchar :: PQ.Oid
pattern Bpchar = PQ.Oid 1042 -- , array_type_oid => '1014',

pattern Varchar :: PQ.Oid
pattern Varchar = PQ.Oid 1043 -- , array_type_oid => '1015',

pattern Date :: PQ.Oid
pattern Date = PQ.Oid 1082 -- , array_type_oid => '1182', descr => 'date',

pattern Time :: PQ.Oid
pattern Time = PQ.Oid 1083 -- , array_type_oid => '1183', descr => 'time of day',

pattern Timestamp :: PQ.Oid
pattern Timestamp = PQ.Oid 1114 -- , array_type_oid => '1115', descr => 'date and time',

pattern Timestamptz :: PQ.Oid
pattern Timestamptz = PQ.Oid 1184 -- , array_type_oid => '1185',

pattern Interval :: PQ.Oid
pattern Interval = PQ.Oid 1186 -- , array_type_oid => '1187',

pattern Timetz :: PQ.Oid
pattern Timetz = PQ.Oid 1266 -- , array_type_oid => '1270',

pattern Bit :: PQ.Oid
pattern Bit = PQ.Oid 1560 -- , array_type_oid => '1561', descr => 'fixed-length bit string',

pattern Varbit :: PQ.Oid
pattern Varbit = PQ.Oid 1562 -- , array_type_oid => '1563',

pattern Numeric :: PQ.Oid
pattern Numeric = PQ.Oid 1700 -- , array_type_oid => '1231',

pattern Refcursor :: PQ.Oid
pattern Refcursor = PQ.Oid 1790 -- , array_type_oid => '2201',

pattern Regprocedure :: PQ.Oid
pattern Regprocedure = PQ.Oid 2202 -- , array_type_oid => '2207',

pattern Regoper :: PQ.Oid
pattern Regoper = PQ.Oid 2203 -- , array_type_oid => '2208', descr => 'registered operator',

pattern Regoperator :: PQ.Oid
pattern Regoperator = PQ.Oid 2204 -- , array_type_oid => '2209',

pattern Regclass :: PQ.Oid
pattern Regclass = PQ.Oid 2205 -- , array_type_oid => '2210', descr => 'registered class',

pattern Regcollation :: PQ.Oid
pattern Regcollation = PQ.Oid 4191 -- , array_type_oid => '4192', descr => 'registered collation',

pattern Regtype :: PQ.Oid
pattern Regtype = PQ.Oid 2206 -- , array_type_oid => '2211', descr => 'registered type',

pattern Regrole :: PQ.Oid
pattern Regrole = PQ.Oid 4096 -- , array_type_oid => '4097', descr => 'registered role',

pattern Regnamespace :: PQ.Oid
pattern Regnamespace = PQ.Oid 4089 -- , array_type_oid => '4090', descr => 'registered namespace',

pattern Uuid :: PQ.Oid
pattern Uuid = PQ.Oid 2950 -- , array_type_oid => '2951', descr => 'UUID datatype',

pattern Tsvector :: PQ.Oid
pattern Tsvector = PQ.Oid 3614 -- , array_type_oid => '3643',

pattern Gtsvector :: PQ.Oid
pattern Gtsvector = PQ.Oid 3642 -- , array_type_oid => '3644',

pattern Tsquery :: PQ.Oid
pattern Tsquery = PQ.Oid 3615 -- , array_type_oid => '3645',

pattern Regconfig :: PQ.Oid
pattern Regconfig = PQ.Oid 3734 -- , array_type_oid => '3735',

pattern Regdictionary :: PQ.Oid
pattern Regdictionary = PQ.Oid 3769 -- , array_type_oid => '3770',

pattern Jsonb :: PQ.Oid
pattern Jsonb = PQ.Oid 3802 -- , array_type_oid => '3807', descr => 'Binary JSON',

pattern Jsonpath :: PQ.Oid
pattern Jsonpath = PQ.Oid 4072 -- , array_type_oid => '4073', descr => 'JSON path',

pattern Int4range :: PQ.Oid
pattern Int4range = PQ.Oid 3904 -- , array_type_oid => '3905', descr => 'range of integers',

pattern Numrange :: PQ.Oid
pattern Numrange = PQ.Oid 3906 -- , array_type_oid => '3907', descr => 'range of numerics',

pattern Tsrange :: PQ.Oid
pattern Tsrange = PQ.Oid 3908 -- , array_type_oid => '3909',

pattern Tstzrange :: PQ.Oid
pattern Tstzrange = PQ.Oid 3910 -- , array_type_oid => '3911',

pattern Daterange :: PQ.Oid
pattern Daterange = PQ.Oid 3912 -- , array_type_oid => '3913', descr => 'range of dates',

pattern Int8range :: PQ.Oid
pattern Int8range = PQ.Oid 3926 -- , array_type_oid => '3927', descr => 'range of bigints',

pattern Int4multirange :: PQ.Oid
pattern Int4multirange = PQ.Oid 4451 -- , array_type_oid => '6150', descr => 'multirange of integers',

pattern Nummultirange :: PQ.Oid
pattern Nummultirange = PQ.Oid 4532 -- , array_type_oid => '6151', descr => 'multirange of numerics',

pattern Tsmultirange :: PQ.Oid
pattern Tsmultirange = PQ.Oid 4533 -- , array_type_oid => '6152',

pattern Tstzmultirange :: PQ.Oid
pattern Tstzmultirange = PQ.Oid 4534 -- , array_type_oid => '6153',

pattern Datemultirange :: PQ.Oid
pattern Datemultirange = PQ.Oid 4535 -- , array_type_oid => '6155', descr => 'multirange of dates',

pattern Int8multirange :: PQ.Oid
pattern Int8multirange = PQ.Oid 4536 -- , array_type_oid => '6157', descr => 'multirange of bigints',

pattern Record :: PQ.Oid
pattern Record = PQ.Oid 2249 -- , descr => 'pseudo-type representing any composite type',

pattern Cstring :: PQ.Oid
pattern Cstring = PQ.Oid 2275 -- , array_type_oid => '1263', descr => 'C-style string',

pattern Any :: PQ.Oid
pattern Any = PQ.Oid 2276 -- , descr => 'pseudo-type representing any type',

pattern Anyarray :: PQ.Oid
pattern Anyarray = PQ.Oid 2277 -- , descr => 'pseudo-type representing a polymorphic array type',

pattern Void :: PQ.Oid
pattern Void = PQ.Oid 2278

pattern Trigger :: PQ.Oid
pattern Trigger = PQ.Oid 2279 -- , descr => 'pseudo-type for the result of a trigger function',

pattern Event_trigger :: PQ.Oid
pattern Event_trigger = PQ.Oid 3838

pattern Language_handler :: PQ.Oid
pattern Language_handler = PQ.Oid 2280

pattern Internal :: PQ.Oid
pattern Internal = PQ.Oid 2281

pattern Anyelement :: PQ.Oid
pattern Anyelement = PQ.Oid 2283 -- , descr => 'pseudo-type representing a polymorphic base type',

pattern Anynonarray :: PQ.Oid
pattern Anynonarray = PQ.Oid 2776

pattern Anyenum :: PQ.Oid
pattern Anyenum = PQ.Oid 3500

pattern Fdw_handler :: PQ.Oid
pattern Fdw_handler = PQ.Oid 3115

pattern Index_am_handler :: PQ.Oid
pattern Index_am_handler = PQ.Oid 325

pattern Tsm_handler :: PQ.Oid
pattern Tsm_handler = PQ.Oid 3310

pattern Table_am_handler :: PQ.Oid
pattern Table_am_handler = PQ.Oid 269

pattern Anyrange :: PQ.Oid
pattern Anyrange = PQ.Oid 3831

pattern Anycompatible :: PQ.Oid
pattern Anycompatible = PQ.Oid 5077

pattern Anycompatiblearray :: PQ.Oid
pattern Anycompatiblearray = PQ.Oid 5078

pattern Anycompatiblenonarray :: PQ.Oid
pattern Anycompatiblenonarray = PQ.Oid 5079

pattern Anycompatiblerange :: PQ.Oid
pattern Anycompatiblerange = PQ.Oid 5080

pattern Anymultirange :: PQ.Oid
pattern Anymultirange = PQ.Oid 4537

pattern Anycompatiblemultirange :: PQ.Oid
pattern Anycompatiblemultirange = PQ.Oid 4538

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
  Datemultirange  -> PQ.Oid 6155
  Daterange       -> PQ.Oid 3913
  Float4          -> PQ.Oid 1021
  Float8          -> PQ.Oid 1022
  Gtsvector       -> PQ.Oid 3644
  Inet            -> PQ.Oid 1041
  Int2            -> PQ.Oid 1005
  Int2vector      -> PQ.Oid 1006
  Int4            -> PQ.Oid 1007
  Int4multirange  -> PQ.Oid 6150
  Int4range       -> PQ.Oid 3905
  Int8            -> PQ.Oid 1016
  Int8multirange  -> PQ.Oid 6157
  Int8range       -> PQ.Oid 3927
  Interval        -> PQ.Oid 1187
  Json            -> PQ.Oid 199
  Jsonb           -> PQ.Oid 3807
  Jsonpath        -> PQ.Oid 4073
  Line            -> PQ.Oid 629
  Lseg            -> PQ.Oid 1018
  Macaddr         -> PQ.Oid 1040
  Macaddr8        -> PQ.Oid 775
  Money           -> PQ.Oid 791
  Name            -> PQ.Oid 1003
  Numeric         -> PQ.Oid 1231
  Nummultirange   -> PQ.Oid 6151
  Numrange        -> PQ.Oid 3907
  Oid             -> PQ.Oid 1028
  Oidvector       -> PQ.Oid 1013
  Path            -> PQ.Oid 1019
  Point           -> PQ.Oid 1017
  Polygon         -> PQ.Oid 1027
  Refcursor       -> PQ.Oid 2201
  Regclass        -> PQ.Oid 2210
  Regcollation    -> PQ.Oid 4192
  Regconfig       -> PQ.Oid 3735
  Regdictionary   -> PQ.Oid 3770
  Regnamespace    -> PQ.Oid 4090
  Regoper         -> PQ.Oid 2208
  Regoperator     -> PQ.Oid 2209
  Regproc         -> PQ.Oid 1008
  Regprocedure    -> PQ.Oid 2207
  Regrole         -> PQ.Oid 4097
  Regtype         -> PQ.Oid 2211
  Text            -> PQ.Oid 1009
  Tid             -> PQ.Oid 1010
  Time            -> PQ.Oid 1183
  Timestamp       -> PQ.Oid 1115
  Timestamptz     -> PQ.Oid 1185
  Timetz          -> PQ.Oid 1270
  Tsmultirange    -> PQ.Oid 6152
  Tsquery         -> PQ.Oid 3645
  Tsrange         -> PQ.Oid 3909
  Tstzmultirange  -> PQ.Oid 6153
  Tstzrange       -> PQ.Oid 3911
  Tsvector        -> PQ.Oid 3643
  Uuid            -> PQ.Oid 2951
  Varbit          -> PQ.Oid 1563
  Varchar         -> PQ.Oid 1015
  Xid             -> PQ.Oid 1011
  Xid8            -> PQ.Oid 271
  Xml             -> PQ.Oid 143
  _               -> Unknown
