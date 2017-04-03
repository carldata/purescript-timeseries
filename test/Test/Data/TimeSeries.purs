module Test.Data.Series (testSeries) where

import Prelude
import Data.TimeSeries as TS
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)


testSeries :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSeries = do

    log "Init test series"
    let emptySeries = TS.empty
    let s1 = TS.fromValues [10.0, 1.2, 32.4, 0.65, 11.0]

    log "empty series has length 0"
    assert $ TS.length emptySeries == 0

    log "return series size" 
    assert $ TS.length s1 == 5
