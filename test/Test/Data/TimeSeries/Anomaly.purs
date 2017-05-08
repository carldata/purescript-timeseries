module Test.Data.TimeSeries.Anomaly (testAnomalies) where

import Prelude
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)

import Data.TimeSeries as TS
import Data.TimeSeries.Anomaly as TA


testAnomalies :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
testAnomalies = do

  log "\n# Anomalies tests"
  removeOutliersTest

    
removeOutliersTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
removeOutliersTest = do
  log "* Remove outliers"
  let xs1 = TS.fromValues [1.0, 1.1, 0.9, 1.2, 1.0]
  let xs2 = TS.fromValues [1.0, 0.8, 9.0, 1.2]
  let model = TA.train xs1
  let ys = TA.removeOutliers model xs2
  assert $ TS.values ys == [1.0, 0.8, 1.04, 1.2]
