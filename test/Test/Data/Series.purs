module Test.Data.Series (testSeries) where

import Prelude
import Data.TimeSeries as TS
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)


testSeries :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSeries = do

    log "empty series has length 0"
    let emptySeries = TS.empty
    assert $ TS.length emptySeries == 0
