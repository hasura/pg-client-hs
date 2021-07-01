{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.PG.Query.PTI where

import           Data.Word

import           Data.Hashable
import qualified Data.HashMap.Strict        as Map
import           Data.Tuple
import qualified Database.PostgreSQL.LibPQ  as PQ
import           Foreign.C.Types
import qualified Language.Haskell.TH.Syntax as TH

mkOid :: Word32 -> PQ.Oid
mkOid = PQ.Oid . fromIntegral

-- * Constants
-------------------------
auto            = mkOid 0

abstime         = mkOid 702
aclitem         = mkOid 1033
bit             = mkOid 1560
bool            = mkOid 16
box             = mkOid 603
bpchar          = mkOid 1042
bytea           = mkOid 17
char            = mkOid 18
cid             = mkOid 29
cidr            = mkOid 650
circle          = mkOid 718
cstring         = mkOid 2275
date            = mkOid 1082
daterange       = mkOid 3912
float4          = mkOid 700
float8          = mkOid 701
gtsvector       = mkOid 3642
inet            = mkOid 869
int2            = mkOid 21
int2vector      = mkOid 22
int4            = mkOid 23
int4range       = mkOid 3904
int8            = mkOid 20
int8range       = mkOid 3926
interval        = mkOid 1186
json            = mkOid 114
jsonb           = mkOid 3802
line            = mkOid 628
lseg            = mkOid 601
macaddr         = mkOid 829
money           = mkOid 790
name            = mkOid 19
numeric         = mkOid 1700
numrange        = mkOid 3906
oid             = mkOid 26
oidvector       = mkOid 30
path            = mkOid 602
point           = mkOid 600
polygon         = mkOid 604
record          = mkOid 2249
refcursor       = mkOid 1790
regclass        = mkOid 2205
regconfig       = mkOid 3734
regdictionary   = mkOid 3769
regoper         = mkOid 2203
regoperator     = mkOid 2204
regproc         = mkOid 24
regprocedure    = mkOid 2202
regtype         = mkOid 2206
reltime         = mkOid 703
text            = mkOid 25
tid             = mkOid 27
time            = mkOid 1083
timestamp       = mkOid 1114
timestamptz     = mkOid 1184
timetz          = mkOid 1266
tinterval       = mkOid 704
tsquery         = mkOid 3615
tsrange         = mkOid 3908
tstzrange       = mkOid 3910
tsvector        = mkOid 3614
txid_snapshot   = mkOid 2970
unknown         = mkOid 705
uuid            = mkOid 2950
varbit          = mkOid 1562
varchar         = mkOid 1043
void            = mkOid 2278
xid             = mkOid 28
xml             = mkOid 142

-- Array Types
abstime_arr       = mkOid 1023
aclitem_arr       = mkOid 1034
bit_arr           = mkOid 1561
bool_arr          = mkOid 1000
box_arr           = mkOid 1020
bpchar_arr        = mkOid 1014
bytea_arr         = mkOid 1001
char_arr          = mkOid 1002
cid_arr           = mkOid 1012
cidr_arr          = mkOid 651
circle_arr        = mkOid 719
cstring_arr       = mkOid 1263
date_arr          = mkOid 1182
daterange_arr     = mkOid 3913
float4_arr        = mkOid 1021
float8_arr        = mkOid 1022
gtsvector_arr     = mkOid 3644
inet_arr          = mkOid 1041
int2_arr          = mkOid 1005
int2vector_arr    = mkOid 1006
int4_arr          = mkOid 1007
int4range_arr     = mkOid 3905
int8_arr          = mkOid 1016
int8range_arr     = mkOid 3927
interval_arr      = mkOid 1187
json_arr          = mkOid 199
jsonb_arr         = mkOid 3807
line_arr          = mkOid 629
lseg_arr          = mkOid 1018
macaddr_arr       = mkOid 1040
money_arr         = mkOid 791
name_arr          = mkOid 1003
numeric_arr       = mkOid 1231
numrange_arr      = mkOid 3907
oid_arr           = mkOid 1028
oidvector_arr     = mkOid 1013
path_arr          = mkOid 1019
point_arr         = mkOid 1017
polygon_arr       = mkOid 1027
record_arr        = mkOid 2287
refcursor_arr     = mkOid 2201
regclass_arr      = mkOid 2210
regconfig_arr     = mkOid 3735
regdictionary_arr = mkOid 3770
regoper_arr       = mkOid 2208
regoperator_arr   = mkOid 2209
regproc_arr       = mkOid 1008
regprocedure_arr  = mkOid 2207
regtype_arr       = mkOid 2211
reltime_arr       = mkOid 1024
text_arr          = mkOid 1009
tid_arr           = mkOid 1010
time_arr          = mkOid 1183
timestamp_arr     = mkOid 1115
timestamptz_arr   = mkOid 1185
timetz_arr        = mkOid 1270
tinterval_arr     = mkOid 1025
tsquery_arr       = mkOid 3645
tsrange_arr       = mkOid 3909
tstzrange_arr     = mkOid 3911
tsvector_arr      = mkOid 3643
txid_snapshot_arr = mkOid 2949
uuid_arr          = mkOid 2951
varbit_arr        = mkOid 1563
varchar_arr       = mkOid 1015
xid_arr           = mkOid 1011
xml_arr           = mkOid 143

newtype ElemOid = ElemOid { getElemOid :: PQ.Oid} deriving (Show, Eq)

instance TH.Lift ElemOid where
  lift (ElemOid (PQ.Oid (CUInt w)))
    = [| ElemOid (PQ.Oid ( CUInt $(TH.lift w))) |]

instance Hashable ElemOid where
  hashWithSalt i (ElemOid (PQ.Oid (CUInt z))) = hashWithSalt i z

newtype ArrOid  = ArrOid { getArrOid :: PQ.Oid} deriving (Show, Eq)

instance TH.Lift ArrOid where
  lift (ArrOid (PQ.Oid (CUInt w)))
    = [| ArrOid (PQ.Oid ( CUInt $(TH.lift w))) |]

instance Hashable ArrOid where
  hashWithSalt i (ArrOid (PQ.Oid (CUInt z))) = hashWithSalt i z

arrOidsQ :: PQ.Oid -> TH.Q TH.Exp
arrOidsQ e = case getArrOidOfElem (ElemOid e) of
  Nothing -> fail $ "Could not find array oid for " <> show e
  Just x  -> TH.lift (ElemOid e, x)

getArrOidOfElem :: ElemOid -> Maybe ArrOid
getArrOidOfElem e = Map.lookup e elemArrOidMap

elemArrOidMap :: Map.HashMap ElemOid ArrOid
elemArrOidMap = Map.fromList elemArrOidPairs

arrElemOidMap :: Map.HashMap ArrOid ElemOid
arrElemOidMap = Map.fromList $ map swap elemArrOidPairs

elemArrOidPairs :: [(ElemOid,ArrOid)]
elemArrOidPairs =
  [ (ElemOid abstime        , ArrOid abstime_arr      )
  , (ElemOid aclitem        , ArrOid aclitem_arr      )
  , (ElemOid bit            , ArrOid bit_arr          )
  , (ElemOid bool           , ArrOid bool_arr         )
  , (ElemOid box            , ArrOid box_arr          )
  , (ElemOid bpchar         , ArrOid bpchar_arr       )
  , (ElemOid bytea          , ArrOid bytea_arr        )
  , (ElemOid char           , ArrOid char_arr         )
  , (ElemOid cid            , ArrOid cid_arr          )
  , (ElemOid cidr           , ArrOid cidr_arr         )
  , (ElemOid circle         , ArrOid circle_arr       )
  , (ElemOid cstring        , ArrOid cstring_arr      )
  , (ElemOid date           , ArrOid date_arr         )
  , (ElemOid daterange      , ArrOid daterange_arr    )
  , (ElemOid float4         , ArrOid float4_arr       )
  , (ElemOid float8         , ArrOid float8_arr       )
  , (ElemOid gtsvector      , ArrOid gtsvector_arr    )
  , (ElemOid inet           , ArrOid inet_arr         )
  , (ElemOid int2           , ArrOid int2_arr         )
  , (ElemOid int2vector     , ArrOid int2vector_arr   )
  , (ElemOid int4           , ArrOid int4_arr         )
  , (ElemOid int4range      , ArrOid int4range_arr    )
  , (ElemOid int8           , ArrOid int8_arr         )
  , (ElemOid int8range      , ArrOid int8range_arr    )
  , (ElemOid interval       , ArrOid interval_arr     )
  , (ElemOid json           , ArrOid json_arr         )
  , (ElemOid jsonb          , ArrOid jsonb_arr        )
  , (ElemOid line           , ArrOid line_arr         )
  , (ElemOid lseg           , ArrOid lseg_arr         )
  , (ElemOid macaddr        , ArrOid macaddr_arr      )
  , (ElemOid money          , ArrOid money_arr        )
  , (ElemOid name           , ArrOid name_arr         )
  , (ElemOid numeric        , ArrOid numeric_arr      )
  , (ElemOid numrange       , ArrOid numrange_arr     )
  , (ElemOid oid            , ArrOid oid_arr          )
  , (ElemOid oidvector      , ArrOid oidvector_arr    )
  , (ElemOid path           , ArrOid path_arr         )
  , (ElemOid point          , ArrOid point_arr        )
  , (ElemOid polygon        , ArrOid polygon_arr      )
  , (ElemOid record         , ArrOid record_arr       )
  , (ElemOid refcursor      , ArrOid refcursor_arr    )
  , (ElemOid regclass       , ArrOid regclass_arr     )
  , (ElemOid regconfig      , ArrOid regconfig_arr    )
  , (ElemOid regdictionary  , ArrOid regdictionary_arr)
  , (ElemOid regoper        , ArrOid regoper_arr      )
  , (ElemOid regoperator    , ArrOid regoperator_arr  )
  , (ElemOid regproc        , ArrOid regproc_arr      )
  , (ElemOid regprocedure   , ArrOid regprocedure_arr )
  , (ElemOid regtype        , ArrOid regtype_arr      )
  , (ElemOid reltime        , ArrOid reltime_arr      )
  , (ElemOid text           , ArrOid text_arr         )
  , (ElemOid tid            , ArrOid tid_arr          )
  , (ElemOid time           , ArrOid time_arr         )
  , (ElemOid timestamp      , ArrOid timestamp_arr    )
  , (ElemOid timestamptz    , ArrOid timestamptz_arr  )
  , (ElemOid timetz         , ArrOid timetz_arr       )
  , (ElemOid tinterval      , ArrOid tinterval_arr    )
  , (ElemOid tsquery        , ArrOid tsquery_arr      )
  , (ElemOid tsrange        , ArrOid tsrange_arr      )
  , (ElemOid tstzrange      , ArrOid tstzrange_arr    )
  , (ElemOid tsvector       , ArrOid tsvector_arr     )
  , (ElemOid txid_snapshot  , ArrOid txid_snapshot_arr)
  , (ElemOid uuid           , ArrOid uuid_arr         )
  , (ElemOid varbit         , ArrOid varbit_arr       )
  , (ElemOid varchar        , ArrOid varchar_arr      )
  , (ElemOid xid            , ArrOid xid_arr          )
  , (ElemOid xml            , ArrOid xml_arr          )
  ]
