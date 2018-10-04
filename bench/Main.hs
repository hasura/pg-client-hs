{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Monad.Except
import           Data.Functor.Identity

import qualified Control.Exception            as E
import qualified Criterion.Main               as C
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import qualified Data.FileEmbed               as FE
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
-- import qualified Data.Hashable                as Hash
import           Data.Int
import qualified Database.PG.Query            as Q
import qualified Database.PG.Query.Connection as Q
import qualified Hasql.Decoders               as HD
import qualified Hasql.Encoders               as HE
import qualified Hasql.Pool                   as HP
import qualified Hasql.Statement              as HS
import qualified Hasql.Transaction            as HT
import qualified Hasql.Transaction.Sessions   as HT
import qualified System.IO.Error              as E

withEx :: (Show e) => IO (Either e a) -> IO a
withEx action =
  action >>= either (E.throwIO . E.userError . show) return

runCTx :: Q.PGPool -> Q.TxE Q.PGExecErr a -> IO a
runCTx pool tx =
  withEx $ runExceptT $ Q.runTx pool (Q.Serializable, Just Q.ReadWrite) tx

runHTx :: HP.Pool -> HT.Transaction a -> IO a
runHTx pool tx =
  withEx $ HP.use pool $ HT.transaction HT.Serializable HT.Write tx

type CTx = Q.TxE Q.PGExecErr

type HTx = HT.Transaction

benchQ
  :: (Q.PGPool, HP.Pool) -> String
  -> (Bool -> CTx B.ByteString, Bool -> HTx B.ByteString) -> IO C.Benchmark
benchQ (poolC, poolH) n (txC, txH) = do
  resC  <- runCTx poolC $ txC False
  resCP <- runCTx poolC $ txC True
  resH  <- runHTx poolH $ txH False
  resHP <- runHTx poolH $ txH True

  unless ( resC == resCP && resCP == resH && resH == resHP) $ do
    BC.putStrLn $ "pg-client: " <> resC
    BC.putStrLn $ "pg-client-prepared: " <> resCP
    BC.putStrLn $ "hasql" <> resH
    BC.putStrLn $ "hasql-prepared" <> resHP
    E.throwIO $ E.userError $ "results are not the same for: " <> n

  return $ C.bgroup n
    [ C.bench "pg-client" $ C.whnfIO $ runCTx poolC $ txC False
    , C.bench "pg-client-prepared" $ C.whnfIO $ runCTx poolC $ txC True
    , C.bench "hasql" $ C.whnfIO $ runHTx poolH $ txH False
    , C.bench "hasql-prepared" $ C.whnfIO $ runHTx poolH $ txH True
    ]

getPoolC :: IO Q.PGPool
getPoolC = do
  let connInfo = Q.defaultConnInfo
                 { Q.connHost = "127.0.0.1"
                 , Q.connPort  = 7432
                 , Q.connUser = "admin"
                 , Q.connDatabase = "chinook"
                 }
      connParams = Q.ConnParams 1 1 180
  Q.initPGPool connInfo connParams

q1 :: T.Text
q1 = $(FE.embedStringFile "bench/queries/artistByArtistId.sql")

mkTx1C :: Bool -> CTx B.ByteString
mkTx1C isPrepared =
  runIdentity . Q.getRow <$>
  Q.withQE Q.PGExecErrTx
  (Q.fromText q1) (Identity (3 :: Int64)) isPrepared

mkTx1H :: Bool -> HTx B.ByteString
mkTx1H isPrepared =
  HT.statement 3 $ HS.Statement (TE.encodeUtf8 q1) encoder decoder isPrepared
  where
    encoder = HE.param HE.int8
    decoder = HD.singleRow $ HD.column $ HD.custom $ \_ bs -> return bs

q2 :: T.Text
q2 = $(FE.embedStringFile "bench/queries/allArtists.sql")

mkTx2C :: Bool -> CTx B.ByteString
mkTx2C isPrepared =
  runIdentity . Q.getRow <$>
  Q.withQE Q.PGExecErrTx
  (Q.fromText q2) () isPrepared

mkTx2H :: Bool -> HTx B.ByteString
mkTx2H isPrepared =
  HT.statement () $ HS.Statement (TE.encodeUtf8 q2) encoder decoder isPrepared
  where
    encoder = HE.unit
    decoder = HD.singleRow $ HD.column $ HD.custom $ \_ bs -> return bs

main :: IO ()
main = do
  poolC <- getPoolC
  poolH <- HP.acquire (1, 180, "postgresql://admin@127.0.0.1:7432/chinook")

  benchmarks <- sequence [
    benchQ (poolC, poolH) "artistByArtistId" (mkTx1C, mkTx1H),
    benchQ (poolC, poolH) "allArtists" (mkTx2C, mkTx2H)
    ]

  C.defaultMain benchmarks

  Q.destroyPGPool poolC
  HP.release poolH
