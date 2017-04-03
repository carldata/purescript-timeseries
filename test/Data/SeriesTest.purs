module Data.SeriesTest (testSeries) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)

import Data.TimeSeries as TS


testSeries :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSeries = do

    log "empty series has length 0"
    assert $ TS.length TS.empty == 0
