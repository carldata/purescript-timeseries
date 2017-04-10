module Test.Data.TimeSeries.Stats (testStats) where

import Prelude
import Data.TimeSeries as TS
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)
import Data.TimeSeries.Stats as S
import Math (sqrt)


testStats :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testStats = do

    let s1 = TS.fromValues [1.0, 2.0, 3.0]
    let s2 = TS.fromValues [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]

    log "mean"
    assert $ S.mean s1 == 2.0

    -- log "variance"
    -- assert $ S.variance s2 == 35.0/12.0

    -- log "standard deviation"
    -- assert $ S.stddev s2 == sqrt (35.0/12.0)
