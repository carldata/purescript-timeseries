module Test.Data.TimeSeries.IO (testIO) where

import Prelude
import Data.Array as A
import Data.Maybe (fromMaybe)
import Data.TimeSeries.IO as IO
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)

import Data.TimeSeries as TS


testIO :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testIO = do

    log "\n# IO tests"

    log "Init test data"
    let csv1 = "ts,v\n2015-01-02T12:34:56,123.45\n2015-01-02T12:34:57,124\n2015-01-02T12:34:58,125\n"
    let csv2 = "ts,v1,v2\n2015-01-02T12:34:56,123.45,0.5\n2015-01-02T12:34:57,124,34\n2015-01-02T12:34:58,125,12\n"

    log "Load single column from CSV"
    let s1 = fromMaybe TS.empty $ A.head $ IO.fromCsv csv1
    assert $ s1.values == [123.45, 124.0, 125.0]

    log "Load 2 Time Series from CSV"
    let xs = IO.fromCsv csv2
    assert $ A.length xs == 2
