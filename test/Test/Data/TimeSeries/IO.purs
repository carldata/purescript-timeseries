module Test.Data.TimeSeries.IO (testIO) where

import Prelude
import Data.TimeSeries.IO as IO
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)


testIO :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testIO = do

    log "Init test data"
    let csv = "ts,v\n2015-01-02T12:34:56,123.45\n2015-01-02T12:34:57,124\n2015-01-02T12:34:58,125\n"

    log "Load from CSV"
    let s1 = IO.fromCsv csv
    assert $ s1.values == [123.45, 124.0, 125.0]

