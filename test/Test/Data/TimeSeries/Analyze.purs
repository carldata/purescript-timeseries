module Test.Data.TimeSeries.Analyze (testAnalyze) where

import Prelude
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)

import Data.TimeSeries as TS
import Data.TimeSeries.Analyze as TA


testAnalyze :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
testAnalyze = do

  log "\n# Analyze points in Time Series"
  findMissingTest
  findDifferenceTest

    
findMissingTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
findMissingTest = do
  log "* Find missing points"
  let xs = TS.series [1.0, 2.0, 4.0, 7.0, 8.0] [1.0, 1.1, 0.9, 1.2, 1.0]
  assert $ TA.findMissing xs 1.0 == [3.0, 5.0, 6.0]


findDifferenceTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
findDifferenceTest = do
  log "* Find difference between Time Series"
  let xs = TS.series [1.0, 2.0, 4.0, 7.0, 8.0] [1, 2, 4, 7, 8]
  let ys = TS.series [1.0, 2.0, 3.0, 7.0, 8.0] [1, 2, 0, 0, 8]
  assert $ TA.findDifference xs ys == [7.0]
