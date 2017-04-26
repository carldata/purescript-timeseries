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
import LinearAlgebra.Vector (sum)

import Test.Assert (assert, ASSERT)
import Test.Helpers(NOW, now)

import Data.TimeSeries as TS
import Data.TimeSeries.IO as IO


perfTests :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
perfTests = do

    log "\n# Performance tests"   
    zipWith10K
    -- Skipped due to too long processing. ~3080ms. Doesn't scala lineary
    -- zipWith30K
    testRolling
    testFilter
    test1M
    test1M
    

zipWith10K :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
zipWith10K = do
    log "* zipWith 10K points"
    csv <- readTextFile UTF8 "testdata/test10k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = fromMaybe TS.empty $ A.index xs 1
    t1 <- now
    let s3 = TS.zipWith (+) s1 s2
    t2 <- now
    log $ "Processed " <> show (TS.length s3) <> " points in " <> show (t2-t1) <> " milliseconds."
    assert $ t1-t2 < 5e3

zipWith30K :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
zipWith30K = do
    log "* zipWith 30K points"
    csv <- readTextFile UTF8 "testdata/test30k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = fromMaybe TS.empty $ A.index xs 1
    t1 <- now
    let s3 = TS.zipWith (+) s1 s2
    t2 <- now
    log $ "Processed " <> show (TS.length s3) <> " points in " <> show (t2-t1) <> " milliseconds."
    assert $ t1-t2 < 1e4


testRolling :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
testRolling = do
    log "* Test rolling window 30K, window size=3"
    t1 <- now
    csv <- readTextFile UTF8 "testdata/test30k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = TS.rollingWindow 3 sum s1
    t2 <- now
    log $ "Time: " <> show (t2-t1) <> " milliseconds."
    assert $ t2-t1 < 2e4


testFilter :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
testFilter = do
    log "* Filter, zip and fromCsv. 60K points"
    t1 <- now
    csv <- readTextFile UTF8 "testdata/test60k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = fromMaybe TS.empty $ A.index xs 1
    let s3 = A.filter ((==) false) $ A.zipWith (==) (TS.values s1) (TS.values s2)
    t2 <- now
    log $ "Time " <> show (t2-t1) <> " milliseconds."


test1M :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test1M = do
    log "* 1M points"
    let s1 = TS.fromValues $ A.replicate 1000000 2.72
    t1 <- now
    let s3 = map (_ * 3.0) (TS.values s1)
    t2 <- now
    log $ "map " <> show (t2-t1) <> " milliseconds."
    assert $ t2-t1 < 1e3


test10M :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test10M = do
    log "* 10M points"
    let s1 = TS.fromValues $ A.replicate 10000000 3.14
    t1 <- now
    let s3 = map (_ * 3.0) (TS.values s1)
    t2 <- now
    log $ "map " <> show (t2-t1) <> " milliseconds."
    assert $ t2-t1 < 1e4
