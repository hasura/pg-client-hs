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
arrayOf :: Oid -> Oid
arrayOf = \case
  Auto -> Auto
  ACLItem -> ACLItemArray
  ACLItemArray -> ACLItemArray
  Bit -> BitArray
  BitArray -> BitArray
  Bool -> BoolArray
  BoolArray -> BoolArray
  Box -> BoxArray
  BoxArray -> BoxArray
  BPChar -> BPCharArray
  BPCharArray -> BPCharArray
  Bytea -> ByteaArray
  ByteaArray -> ByteaArray
  Char -> CharArray
  CharArray -> CharArray
  CID -> CIDArray
  CIDArray -> CIDArray
  CIDR -> CIDRArray
  CIDRArray -> CIDRArray
  Circle -> CircleArray
  CircleArray -> CircleArray
  CString -> CStringArray
  CStringArray -> CStringArray
  Date -> DateArray
  DateArray -> DateArray
  DateRange -> DateRangeArray
  DateRangeArray -> DateRangeArray
  Float4 -> Float4Array
  Float4Array -> Float4Array
  Float8 -> Float8Array
  Float8Array -> Float8Array
  GTSVector -> GTSVectorArray
  GTSVectorArray -> GTSVectorArray
  INet -> INetArray
  INetArray -> INetArray
  Int2 -> Int2Array
  Int2Array -> Int2Array
  Int2Vector -> Int2VectorArray
  Int2VectorArray -> Int2VectorArray
  Int4 -> Int4Array
  Int4Array -> Int4Array
  Int4Range -> Int4RangeArray
  Int4RangeArray -> Int4RangeArray
  Int8 -> Int8Array
  Int8Array -> Int8Array
  Int8Range -> Int8RangeArray
  Int8RangeArray -> Int8RangeArray
  Interval -> IntervalArray
  IntervalArray -> IntervalArray
  JSON -> JSONArray
  JSONArray -> JSONArray
  JSONB -> JSONBArray
  JSONBArray -> JSONBArray
  Line -> LineArray
  LineArray -> LineArray
  LSeg -> LSegArray
  LSegArray -> LSegArray
  MacAddr -> MacAddrArray
  MacAddrArray -> MacAddrArray
  Money -> MoneyArray
  MoneyArray -> MoneyArray
  Name -> NameArray
  NameArray -> NameArray
  Numeric -> NumericArray
  NumericArray -> NumericArray
  NumRange -> NumRangeArray
  NumRangeArray -> NumRangeArray
  OID -> OIDArray
  OIDArray -> OIDArray
  OIDVector -> OIDVectorArray
  OIDVectorArray -> OIDVectorArray
  Path -> PathArray
  PathArray -> PathArray
  Point -> PointArray
  PointArray -> PointArray
  Polygon -> PolygonArray
  PolygonArray -> PolygonArray
  RefCursor -> RefCursorArray
  RefCursorArray -> RefCursorArray
  RegClass -> RegClassArray
  RegClassArray -> RegClassArray
  RegConfig -> RegConfigArray
  RegConfigArray -> RegConfigArray
  RegDictionary -> RegDictionaryArray
  RegDictionaryArray -> RegDictionaryArray
  RegOper -> RegOperArray
  RegOperArray -> RegOperArray
  RegOperator -> RegOperatorArray
  RegOperatorArray -> RegOperatorArray
  RegProc -> RegProcArray
  RegProcArray -> RegProcArray
  RegProcedure -> RegProcedureArray
  RegProcedureArray -> RegProcedureArray
  RegType -> RegTypeArray
  RegTypeArray -> RegTypeArray
  Text -> TextArray
  TextArray -> TextArray
  TID -> TIDArray
  TIDArray -> TIDArray
  Time -> TimeArray
  TimeArray -> TimeArray
  Timestamp -> TimestampArray
  TimestampArray -> TimestampArray
  TimestampTZ -> TimestampTZArray
  TimestampTZArray -> TimestampTZArray
  TimeTZ -> TimeTZArray
  TimeTZArray -> TimeTZArray
  TSQuery -> TSQueryArray
  TSQueryArray -> TSQueryArray
  TSRange -> TSRangeArray
  TSRangeArray -> TSRangeArray
  TSTZRange -> TSTZRangeArray
  TSTZRangeArray -> TSTZRangeArray
  TSVector -> TSVectorArray
  TSVectorArray -> TSVectorArray
  UUID -> UUIDArray
  UUIDArray -> UUIDArray
  Varbit -> VarbitArray
  VarbitArray -> VarbitArray
  Varchar -> VarcharArray
  VarcharArray -> VarcharArray
  XID -> XIDArray
  XIDArray -> XIDArray
  XML -> XMLArray
  XMLArray -> XMLArray
  _ -> Unknown

-- * Constants

--
-- We use pattern synonyms for the Oids so that we can pattern-match on them.

pattern Auto :: Oid
pattern Auto = Oid 0

pattern AbsTime :: Oid
pattern AbsTime = Oid 702

pattern ACLItem :: Oid
pattern ACLItem = Oid 1033

pattern Bit :: Oid
pattern Bit = Oid 1560

pattern Bool :: Oid
pattern Bool = Oid 16

pattern Box :: Oid
pattern Box = Oid 603

pattern BPChar :: Oid
pattern BPChar = Oid 1042

pattern Bytea :: Oid
pattern Bytea = Oid 17

pattern Char :: Oid
pattern Char = Oid 18

pattern CID :: Oid
pattern CID = Oid 29

pattern CIDR :: Oid
pattern CIDR = Oid 650

pattern Circle :: Oid
pattern Circle = Oid 718

pattern CString :: Oid
pattern CString = Oid 2275

pattern Date :: Oid
pattern Date = Oid 1082

pattern DateRange :: Oid
pattern DateRange = Oid 3912

pattern Float4 :: Oid
pattern Float4 = Oid 700

pattern Float8 :: Oid
pattern Float8 = Oid 701

pattern GTSVector :: Oid
pattern GTSVector = Oid 3642

pattern INet :: Oid
pattern INet = Oid 869

pattern Int2 :: Oid
pattern Int2 = Oid 21

pattern Int2Vector :: Oid
pattern Int2Vector = Oid 22

pattern Int4 :: Oid
pattern Int4 = Oid 23

pattern Int4Range :: Oid
pattern Int4Range = Oid 3904

pattern Int8 :: Oid
pattern Int8 = Oid 20

pattern Int8Range :: Oid
pattern Int8Range = Oid 3926

pattern Interval :: Oid
pattern Interval = Oid 1186

pattern JSON :: Oid
pattern JSON = Oid 114

pattern JSONB :: Oid
pattern JSONB = Oid 3802

pattern Line :: Oid
pattern Line = Oid 628

pattern LSeg :: Oid
pattern LSeg = Oid 601

pattern MacAddr :: Oid
pattern MacAddr = Oid 829

pattern Money :: Oid
pattern Money = Oid 790

pattern Name :: Oid
pattern Name = Oid 19

pattern Numeric :: Oid
pattern Numeric = Oid 1700

pattern NumRange :: Oid
pattern NumRange = Oid 3906

pattern OID :: Oid
pattern OID = Oid 26

pattern OIDVector :: Oid
pattern OIDVector = Oid 30

pattern Path :: Oid
pattern Path = Oid 602

pattern Point :: Oid
pattern Point = Oid 600

pattern Polygon :: Oid
pattern Polygon = Oid 604

pattern Record :: Oid
pattern Record = Oid 2249

pattern RefCursor :: Oid
pattern RefCursor = Oid 1790

pattern RegClass :: Oid
pattern RegClass = Oid 2205

pattern RegConfig :: Oid
pattern RegConfig = Oid 3734

pattern RegDictionary :: Oid
pattern RegDictionary = Oid 3769

pattern RegOper :: Oid
pattern RegOper = Oid 2203

pattern RegOperator :: Oid
pattern RegOperator = Oid 2204

pattern RegProc :: Oid
pattern RegProc = Oid 24

pattern RegProcedure :: Oid
pattern RegProcedure = Oid 2202

pattern RegType :: Oid
pattern RegType = Oid 2206

pattern RelTime :: Oid
pattern RelTime = Oid 703

pattern Text :: Oid
pattern Text = Oid 25

pattern TID :: Oid
pattern TID = Oid 27

pattern Time :: Oid
pattern Time = Oid 1083

pattern Timestamp :: Oid
pattern Timestamp = Oid 1114

pattern TimestampTZ :: Oid
pattern TimestampTZ = Oid 1184

pattern TimeTZ :: Oid
pattern TimeTZ = Oid 1266

pattern TInterval :: Oid
pattern TInterval = Oid 704

pattern TSQuery :: Oid
pattern TSQuery = Oid 3615

pattern TSRange :: Oid
pattern TSRange = Oid 3908

pattern TSTZRange :: Oid
pattern TSTZRange = Oid 3910

pattern TSVector :: Oid
pattern TSVector = Oid 3614

pattern TXIDSnapshot :: Oid
pattern TXIDSnapshot = Oid 2970

pattern Unknown :: Oid
pattern Unknown = Oid 705

pattern UUID :: Oid
pattern UUID = Oid 2950

pattern Varbit :: Oid
pattern Varbit = Oid 1562

pattern Varchar :: Oid
pattern Varchar = Oid 1043

pattern Void :: Oid
pattern Void = Oid 2278

pattern XID :: Oid
pattern XID = Oid 28

pattern XML :: Oid
pattern XML = Oid 142

-- Array Types

pattern ACLItemArray :: Oid
pattern ACLItemArray = Oid 1034

pattern BitArray :: Oid
pattern BitArray = Oid 1561

pattern BoolArray :: Oid
pattern BoolArray = Oid 1000

pattern BoxArray :: Oid
pattern BoxArray = Oid 1020

pattern BPCharArray :: Oid
pattern BPCharArray = Oid 1014

pattern ByteaArray :: Oid
pattern ByteaArray = Oid 1001

pattern CharArray :: Oid
pattern CharArray = Oid 1002

pattern CIDArray :: Oid
pattern CIDArray = Oid 1012

pattern CIDRArray :: Oid
pattern CIDRArray = Oid 651

pattern CircleArray :: Oid
pattern CircleArray = Oid 719

pattern CStringArray :: Oid
pattern CStringArray = Oid 1263

pattern DateArray :: Oid
pattern DateArray = Oid 1182

pattern DateRangeArray :: Oid
pattern DateRangeArray = Oid 3913

pattern Float4Array :: Oid
pattern Float4Array = Oid 1021

pattern Float8Array :: Oid
pattern Float8Array = Oid 1022

pattern GTSVectorArray :: Oid
pattern GTSVectorArray = Oid 3644

pattern INetArray :: Oid
pattern INetArray = Oid 1041

pattern Int2Array :: Oid
pattern Int2Array = Oid 1005

pattern Int2VectorArray :: Oid
pattern Int2VectorArray = Oid 1006

pattern Int4Array :: Oid
pattern Int4Array = Oid 1007

pattern Int4RangeArray :: Oid
pattern Int4RangeArray = Oid 3905

pattern Int8Array :: Oid
pattern Int8Array = Oid 1016

pattern Int8RangeArray :: Oid
pattern Int8RangeArray = Oid 3927

pattern IntervalArray :: Oid
pattern IntervalArray = Oid 1187

pattern JSONArray :: Oid
pattern JSONArray = Oid 199

pattern JSONBArray :: Oid
pattern JSONBArray = Oid 3807

pattern LineArray :: Oid
pattern LineArray = Oid 629

pattern LSegArray :: Oid
pattern LSegArray = Oid 1018

pattern MacAddrArray :: Oid
pattern MacAddrArray = Oid 1040

pattern MoneyArray :: Oid
pattern MoneyArray = Oid 791

pattern NameArray :: Oid
pattern NameArray = Oid 1003

pattern NumericArray :: Oid
pattern NumericArray = Oid 1231

pattern NumRangeArray :: Oid
pattern NumRangeArray = Oid 3907

pattern OIDArray :: Oid
pattern OIDArray = Oid 1028

pattern OIDVectorArray :: Oid
pattern OIDVectorArray = Oid 1013

pattern PathArray :: Oid
pattern PathArray = Oid 1019

pattern PointArray :: Oid
pattern PointArray = Oid 1017

pattern PolygonArray :: Oid
pattern PolygonArray = Oid 1027

pattern RefCursorArray :: Oid
pattern RefCursorArray = Oid 2201

pattern RegClassArray :: Oid
pattern RegClassArray = Oid 2210

pattern RegConfigArray :: Oid
pattern RegConfigArray = Oid 3735

pattern RegDictionaryArray :: Oid
pattern RegDictionaryArray = Oid 3770

pattern RegOperArray :: Oid
pattern RegOperArray = Oid 2208

pattern RegOperatorArray :: Oid
pattern RegOperatorArray = Oid 2209

pattern RegProcArray :: Oid
pattern RegProcArray = Oid 1008

pattern RegProcedureArray :: Oid
pattern RegProcedureArray = Oid 2207

pattern RegTypeArray :: Oid
pattern RegTypeArray = Oid 2211

pattern TextArray :: Oid
pattern TextArray = Oid 1009

pattern TIDArray :: Oid
pattern TIDArray = Oid 1010

pattern TimeArray :: Oid
pattern TimeArray = Oid 1183

pattern TimestampArray :: Oid
pattern TimestampArray = Oid 1115

pattern TimestampTZArray :: Oid
pattern TimestampTZArray = Oid 1185

pattern TimeTZArray :: Oid
pattern TimeTZArray = Oid 1270

pattern TSQueryArray :: Oid
pattern TSQueryArray = Oid 3645

pattern TSRangeArray :: Oid
pattern TSRangeArray = Oid 3909

pattern TSTZRangeArray :: Oid
pattern TSTZRangeArray = Oid 3911

pattern TSVectorArray :: Oid
pattern TSVectorArray = Oid 3643

pattern UUIDArray :: Oid
pattern UUIDArray = Oid 2951

pattern VarbitArray :: Oid
pattern VarbitArray = Oid 1563

pattern VarcharArray :: Oid
pattern VarcharArray = Oid 1015

pattern XIDArray :: Oid
pattern XIDArray = Oid 1011

pattern XMLArray :: Oid
pattern XMLArray = Oid 143
