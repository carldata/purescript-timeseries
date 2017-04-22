module Test.PerfTests (perfTests) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Array as A
import Data.Maybe (fromMaybe)
import Node.FS.Sync (readTextFile)
import Node.FS (FS)
import Node.Encoding (Encoding(..))
import Control.Monad.Eff.Exception (EXCEPTION)

import Test.Assert (assert, ASSERT)
import Test.Helpers(NOW, now)

import Data.TimeSeries as TS
import Data.TimeSeries.IO as IO


perfTests :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
perfTests = do

    log "\n# Performance tests"   
    log "* 10K points"
    test10K
    -- zipWith takes around 3080 ms to complete. With DateTime index 200ms longer
    -- log "* 30k points"
    -- test30K
    log "* 60K points"
    test60K
    log "* 1M points"
    test1M
    log "* 10M points"
    test1M
    

test10K :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test10K = do
    csv <- readTextFile UTF8 "testdata/test10k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = fromMaybe TS.empty $ A.index xs 1
    t1 <- now
    let s3 = TS.zipWith (+) s1 s2
    t2 <- now
    log $ "zipWith " <> show (TS.length s3) <> " points in " <> show (t2-t1) <> " milliseconds."
    assert $ t1-t2 < 1e4

test30K :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test30K = do
    csv <- readTextFile UTF8 "testdata/test30k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = fromMaybe TS.empty $ A.index xs 1
    t1 <- now
    let s3 = TS.zipWith (+) s1 s2
    t2 <- now
    log $ "zipWith " <> show (TS.length s3) <> " points in " <> show (t2-t1) <> " milliseconds."
    assert $ t1-t2 < 1e4


test60K :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test60K = do
    t1 <- now
    csv <- readTextFile UTF8 "testdata/test60k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = fromMaybe TS.empty $ A.index xs 1
    let s3 = A.filter ((==) false) $ A.zipWith (==) s1.values s2.values             
    t2 <- now
    log $ "(filter <<< zip <<< fromCsv) " <> show (t2-t1) <> " milliseconds."
    assert $ t2-t1 < 1e3


test1M :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test1M = do
    let s1 = TS.fromValues $ A.replicate 1000000 2.72
    t1 <- now
    let s3 = map (_ * 3.0) s1.values
    t2 <- now
    log $ "map " <> show (t2-t1) <> " milliseconds."
    assert $ t2-t1 < 1e3


test10M :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test10M = do
    let s1 = TS.fromValues $ A.replicate 10000000 3.14
    t1 <- now
    let s3 = map (_ * 3.0) s1.values
    t2 <- now
    log $ "map " <> show (t2-t1) <> " milliseconds."
    assert $ t2-t1 < 1e4
