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
import Test.Data.TimeSeries.Time.Parser (testTimeParser)
import Test.PerfTests (perfTests)


main :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS, now :: NOW | eff) Unit
main = do
  testTimeParser
  testSeries
  testIO
  perfTests