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
    
    log "Performance of 60k file"
    csv60k <- readTextFile UTF8 "testdata/test60k.csv"
    let xs1 = IO.fromCsv csv60k
    let s1 = fromMaybe TS.empty $ A.index xs1 0
    let s2 = fromMaybe TS.empty $ A.index xs1 1
    t1 <- now
    let s3 = A.filter ((==) false) $ A.zipWith (==) s1.values s2.values             
    t2 <- now
    log $ "(filter <<< zip) 60k points in " <> show (t2-t1) <> " milliseconds."
    assert $ (A.length s1.values) == 59042
    
    log "Performance of 10k file"
    csv10k <- readTextFile UTF8 "testdata/test10k.csv"
    let xs2 = IO.fromCsv csv10k
    let s11 = fromMaybe TS.empty $ A.index xs2 0
    let s12 = fromMaybe TS.empty $ A.index xs2 1
    t11 <- now
    let s13 = TS.zipWith (+) s11 s12
    t12 <- now
    log $ "zipWith " <> show (TS.length s13) <> " points in " <> show (t12-t11) <> " milliseconds."
    assert $ (A.length s13.values) == 10000
