module Test.Data.TimeSeries (testSeries) where

import Prelude
import Data.TimeSeries as TS
import Data.TimeSeries.Time as T
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)


testSeries :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSeries = do

    log "\n# time Series base tests"

    log "Init test series"
    let emptySeries = TS.empty
    let s1 = TS.fromValues [1.3, 2.6, 3.4, 4.6, 5.0]
    let s2 = TS.fromValues [4.0, 2.6, 3.4, 4.6, 1.5]
    let start2 = T.fromSeconds 2
    let start20 = T.fromSeconds 20
    let end4 = T.fromSeconds 4

    log "Empty series has length 0"
    assert $ TS.length emptySeries == 0

    log "Return series size" 
    assert $ TS.length s1 == 5

    log "Return values" 
    assert $ s1.values == [1.3, 2.6, 3.4, 4.6, 5.0]

    log "Slicing"
    let slicing1 = TS.slice start2 end4 s1
    assert $ slicing1.values == [3.4, 4.6]

    log "Slicing - empty series if wrong indexes"
    let slicing2 = TS.slice start20 end4 s1
    assert $ slicing2.values == []

    log "Filtering"
    let filtered1 = TS.filter (_ < 3.0) s2
    assert $ filtered1.values == [2.6, 1.5]

    log "zipWith"
    let z1 = TS.zipWith (+) s1 s2
    assert $ z1.values == [5.3, 5.2, 6.8, 9.2, 6.5]
