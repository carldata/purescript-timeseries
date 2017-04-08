module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Assert (ASSERT)

import Test.Data.TimeSeries (testSeries)
import Test.Data.TimeSeries.IO (testIO)
import Test.Data.TimeSeries.Time.Parser (testTimeParser)


main :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  testTimeParser
  testSeries
  testIO
