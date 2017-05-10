module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.FS (FS)
import Control.Monad.Eff.Exception (EXCEPTION)

import Test.Assert (ASSERT)
import Test.Helpers(NOW)

import Test.Data.TimeSeries (testSeries)
import Test.Data.TimeSeries.IO (testIO)
import Test.Data.TimeSeries.Time.Convert (testTimeConverter)
import Test.Data.TimeSeries.Anomaly (testAnomalies)
import Test.Data.TimeSeries.Analyze (testAnalyze)
import Test.Data.TimeSeries.Session (testSessions)
import Test.PerfTests (perfTests)


main :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW | eff) Unit
main = do
  testTimeConverter
  testSeries
  testIO
  testAnomalies
  testAnalyze
  testSessions
  perfTests
