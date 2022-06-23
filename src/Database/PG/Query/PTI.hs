{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Description: Oid constants.
--
-- All these are taken from this declaration file in the postgres repo:
-- https://github.com/postgres/postgres/blob/master/src/include/catalog/pg_type.dat
module Database.PG.Query.PTI where

import Database.PostgreSQL.LibPQ (Oid (..))

-- | Convert an 'Oid' for a single type into the corresponding array 'Oid'. If
-- the original Oid is 'Auto', then the result will also be 'Auto'. For types
-- without a defined array parallel, the result will be 'Unknown'.
arrayOf :: PQ.Oid -> PQ.Oid
arrayOf = \case
  Auto -> Auto
  ACLItem -> ACLItemArray
  Bit -> BitArray
  Bool -> BoolArray
  Box -> BoxArray
  BPChar -> BPCharArray
  Bytea -> ByteaArray
  Char -> CharArray
  CID -> CIDArray
  CIDR -> CIDRArray
  Circle -> CircleArray
  CString -> CStringArray
  Date -> DateArray
  DateRange -> DateRangeArray
  Float4 -> Float4Array
  Float8 -> Float8Array
  GTSVector -> GTSVectorArray
  INet -> INetArray
  Int2 -> Int2Array
  Int2Vector -> Int2VectorArray
  Int4 -> Int4Array
  Int4Range -> Int4RangeArray
  Int8 -> Int8Array
  Int8Range -> Int8RangeArray
  Interval -> IntervalArray
  JSON -> JSONArray
  JSONB -> JSONBArray
  Line -> LineArray
  LSeg -> LSegArray
  MacAddr -> MacAddrArray
  Money -> MoneyArray
  Name -> NameArray
  Numeric -> NumericArray
  NumRange -> NumRangeArray
  OID -> OIDArray
  OIDVector -> OIDVectorArray
  Path -> PathArray
  Point -> PointArray
  Polygon -> PolygonArray
  RefCursor -> RefCursorArray
  RegClass -> RegClassArray
  RegConfig -> RegConfigArray
  RegDictionary -> RegDictionaryArray
  RegOper -> RegOperArray
  RegOperator -> RegOperatorArray
  RegProc -> RegProcArray
  RegProcedure -> RegProcedureArray
  RegType -> RegTypeArray
  Text -> TextArray
  TID -> TIDArray
  Time -> TimeArray
  Timestamp -> TimestampArray
  TimestampTZ -> TimestampTZArray
  TimeTZ -> TimeTZArray
  TSQuery -> TSQueryArray
  TSRange -> TSRangeArray
  TSTZRange -> TSTZRangeArray
  TSVector -> TSVectorArray
  UUID -> UUIDArray
  Varbit -> VarbitArray
  Varchar -> VarcharArray
  XID -> XIDArray
  XML -> XMLArray
  _ -> Unknown

-- * Constants

--
-- We use pattern synonyms for the Oids so that we can pattern-match on them.

pattern Auto :: PQ.Oid
pattern Auto = PQ.Oid 0

pattern AbsTime :: PQ.Oid
pattern AbsTime = PQ.Oid 702

pattern ACLItem :: PQ.Oid
pattern ACLItem = PQ.Oid 1033

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

pattern ACLItemArray :: PQ.Oid
pattern ACLItemArray = PQ.Oid 1034

pattern BitArray :: PQ.Oid
pattern BitArray = PQ.Oid 1561

pattern BoolArray :: PQ.Oid
pattern BoolArray = PQ.Oid 1000

pattern BoxArray :: PQ.Oid
pattern BoxArray = PQ.Oid 1020

pattern BPCharArray :: PQ.Oid
pattern BPCharArray = PQ.Oid 1014

pattern ByteaArray :: PQ.Oid
pattern ByteaArray = PQ.Oid 1001

pattern CharArray :: PQ.Oid
pattern CharArray = PQ.Oid 1002

pattern CIDArray :: PQ.Oid
pattern CIDArray = PQ.Oid 1012

pattern CIDRArray :: PQ.Oid
pattern CIDRArray = PQ.Oid 651

pattern CircleArray :: PQ.Oid
pattern CircleArray = PQ.Oid 719

pattern CStringArray :: PQ.Oid
pattern CStringArray = PQ.Oid 1263

pattern DateArray :: PQ.Oid
pattern DateArray = PQ.Oid 1182

pattern DateRangeArray :: PQ.Oid
pattern DateRangeArray = PQ.Oid 3913

pattern Float4Array :: PQ.Oid
pattern Float4Array = PQ.Oid 1021

pattern Float8Array :: PQ.Oid
pattern Float8Array = PQ.Oid 1022

pattern GTSVectorArray :: PQ.Oid
pattern GTSVectorArray = PQ.Oid 3644

pattern INetArray :: PQ.Oid
pattern INetArray = PQ.Oid 1041

pattern Int2Array :: PQ.Oid
pattern Int2Array = PQ.Oid 1005

pattern Int2VectorArray :: PQ.Oid
pattern Int2VectorArray = PQ.Oid 1006

pattern Int4Array :: PQ.Oid
pattern Int4Array = PQ.Oid 1007

pattern Int4RangeArray :: PQ.Oid
pattern Int4RangeArray = PQ.Oid 3905

pattern Int8Array :: PQ.Oid
pattern Int8Array = PQ.Oid 1016

pattern Int8RangeArray :: PQ.Oid
pattern Int8RangeArray = PQ.Oid 3927

pattern IntervalArray :: PQ.Oid
pattern IntervalArray = PQ.Oid 1187

pattern JSONArray :: PQ.Oid
pattern JSONArray = PQ.Oid 199

pattern JSONBArray :: PQ.Oid
pattern JSONBArray = PQ.Oid 3807

pattern LineArray :: PQ.Oid
pattern LineArray = PQ.Oid 629

pattern LSegArray :: PQ.Oid
pattern LSegArray = PQ.Oid 1018

pattern MacAddrArray :: PQ.Oid
pattern MacAddrArray = PQ.Oid 1040

pattern MoneyArray :: PQ.Oid
pattern MoneyArray = PQ.Oid 791

pattern NameArray :: PQ.Oid
pattern NameArray = PQ.Oid 1003

pattern NumericArray :: PQ.Oid
pattern NumericArray = PQ.Oid 1231

pattern NumRangeArray :: PQ.Oid
pattern NumRangeArray = PQ.Oid 3907

pattern OIDArray :: PQ.Oid
pattern OIDArray = PQ.Oid 1028

pattern OIDVectorArray :: PQ.Oid
pattern OIDVectorArray = PQ.Oid 1013

pattern PathArray :: PQ.Oid
pattern PathArray = PQ.Oid 1019

pattern PointArray :: PQ.Oid
pattern PointArray = PQ.Oid 1017

pattern PolygonArray :: PQ.Oid
pattern PolygonArray = PQ.Oid 1027

pattern RefCursorArray :: PQ.Oid
pattern RefCursorArray = PQ.Oid 2201

pattern RegClassArray :: PQ.Oid
pattern RegClassArray = PQ.Oid 2210

pattern RegConfigArray :: PQ.Oid
pattern RegConfigArray = PQ.Oid 3735

pattern RegDictionaryArray :: PQ.Oid
pattern RegDictionaryArray = PQ.Oid 3770

pattern RegOperArray :: PQ.Oid
pattern RegOperArray = PQ.Oid 2208

pattern RegOperatorArray :: PQ.Oid
pattern RegOperatorArray = PQ.Oid 2209

pattern RegProcArray :: PQ.Oid
pattern RegProcArray = PQ.Oid 1008

pattern RegProcedureArray :: PQ.Oid
pattern RegProcedureArray = PQ.Oid 2207

pattern RegTypeArray :: PQ.Oid
pattern RegTypeArray = PQ.Oid 2211

pattern TextArray :: PQ.Oid
pattern TextArray = PQ.Oid 1009

pattern TIDArray :: PQ.Oid
pattern TIDArray = PQ.Oid 1010

pattern TimeArray :: PQ.Oid
pattern TimeArray = PQ.Oid 1183

pattern TimestampArray :: PQ.Oid
pattern TimestampArray = PQ.Oid 1115

pattern TimestampTZArray :: PQ.Oid
pattern TimestampTZArray = PQ.Oid 1185

pattern TimeTZArray :: PQ.Oid
pattern TimeTZArray = PQ.Oid 1270

pattern TSQueryArray :: PQ.Oid
pattern TSQueryArray = PQ.Oid 3645

pattern TSRangeArray :: PQ.Oid
pattern TSRangeArray = PQ.Oid 3909

pattern TSTZRangeArray :: PQ.Oid
pattern TSTZRangeArray = PQ.Oid 3911

pattern TSVectorArray :: PQ.Oid
pattern TSVectorArray = PQ.Oid 3643

pattern UUIDArray :: PQ.Oid
pattern UUIDArray = PQ.Oid 2951

pattern VarbitArray :: PQ.Oid
pattern VarbitArray = PQ.Oid 1563

pattern VarcharArray :: PQ.Oid
pattern VarcharArray = PQ.Oid 1015

pattern XIDArray :: PQ.Oid
pattern XIDArray = PQ.Oid 1011

pattern XMLArray :: PQ.Oid
pattern XMLArray = PQ.Oid 143
