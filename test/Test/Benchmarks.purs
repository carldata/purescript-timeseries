module Test.Benchmarks (benchmarks) where

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

import Data.TimeSeries as TS
import Data.TimeSeries.IO as IO


benchmarks :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS | eff) Unit
benchmarks = do

    log "\n# Benchmark tests"
    log "Load 60k Time Series from the file"
    csv <- readTextFile UTF8 "testdata/test60k.csv"
    let s1 = fromMaybe TS.empty $ A.head $ IO.fromCsv csv
    log $ show (A.length s1.values)
    assert $ (A.length s1.values) == 59042
