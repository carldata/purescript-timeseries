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
    log "10k points"
    test10k
    -- 30k test is too slow. Time ~= 3286 ms
    log "30k points"
    test30k
    log "60k points"
    test60k
    

test10k :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test10k = do
    csv <- readTextFile UTF8 "testdata/test10k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = fromMaybe TS.empty $ A.index xs 1
    t1 <- now
    let s3 = TS.zipWith (+) s1 s2
    t2 <- now
    log $ "zipWith " <> show (TS.length s3) <> " points in " <> show (t2-t1) <> " milliseconds."
    assert $ t1-t2 < 1e4

-- While using DateTime as a index this function takes around 3280 ms to complete.
test30k :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test30k = do
    csv <- readTextFile UTF8 "testdata/test30k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = fromMaybe TS.empty $ A.index xs 1
    t1 <- now
    let s3 = TS.zipWith (+) s1 s2
    t2 <- now
    log $ "zipWith " <> show (TS.length s3) <> " points in " <> show (t2-t1) <> " milliseconds."
    assert $ t1-t2 < 1e4


test60k :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW  | eff) Unit
test60k = do
    csv <- readTextFile UTF8 "testdata/test60k.csv"
    let xs = IO.fromCsv csv
    let s1 = fromMaybe TS.empty $ A.index xs 0
    let s2 = fromMaybe TS.empty $ A.index xs 1
    t1 <- now
    let s3 = A.filter ((==) false) $ A.zipWith (==) s1.values s2.values             
    t2 <- now
    log $ "* (filter <<< zip) " <> show (t2-t1) <> " milliseconds."
    assert $ t2-t1 < 1e3
