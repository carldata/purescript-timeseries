module Test.Data.TimeSeries.Model.Exponential (testExponential) where 

import Prelude
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)

import Data.TimeSeries as TS
import Data.TimeSeries.Model.Exponential as ME


testExponential :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
testExponential = do

  log "\n# Test exponential model"
  udateTest
  fromSeriesTest
  predictTest

    
udateTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
udateTest = do
  log "* Update model"
  let model = ME.ses 0.5 1.0
  assert $ ME.update model 20.0 == ME.ses 0.5 10.5

    
fromSeriesTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
fromSeriesTest = do
  log "* Build model from series"
  let xs = TS.fromValues [1.0, 1.0, 1.0]
  assert $ ME.fromSeries 0.5 xs == ME.ses 0.5 0.875

    
predictTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
predictTest = do
  log "* Predict next value"
  let model = ME.ses 0.5 120.4
  assert $ ME.predictNext model == 120.4
