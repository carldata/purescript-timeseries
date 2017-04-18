module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.FS (FS)
import Control.Monad.Eff.Exception (EXCEPTION)

import Test.Assert (ASSERT)

import Test.Data.TimeSeries (testSeries)
import Test.Data.TimeSeries.IO (testIO)
import Test.Data.TimeSeries.Stats (testStats)
import Test.Data.TimeSeries.Time.Parser (testTimeParser)
import Test.Benchmarks (benchmarks)


main :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS | eff) Unit
main = do
  testTimeParser
  testSeries
  testIO
  testStats
  benchmarks