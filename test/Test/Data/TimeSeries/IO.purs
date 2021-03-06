module Test.Data.TimeSeries.IO (testIO) where

import Prelude
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Array as A
import Data.Maybe (fromMaybe)
import Data.Tuple(fst, snd)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Test.Assert (assert, ASSERT)

import Data.TimeSeries as TS
import Data.TimeSeries.IO as IO


testIO :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS  | eff) Unit
testIO = do

  log "\n# IO tests"
  singleColumnTest
  multiColumnTest
  fromFileTest
  toCsvTest

    
singleColumnTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS  | eff) Unit
singleColumnTest = do
  log "Load single column from CSV"
  let csv1 = "ts,v\n2015-01-02T12:34:56,123.45\n2015-01-02T12:34:57,124\n2015-01-02T12:34:58,125\n"
  let s1 = fromMaybe TS.empty $ A.head $ IO.fromCsv csv1
  assert $ TS.values s1 == [123.45, 124.0, 125.0]


multiColumnTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS  | eff) Unit
multiColumnTest = do
  log "Load 2 Time Series from CSV"
  let csv2 = "ts,v1,v2\n2015-01-02T12:34:56,123.45,0.5\n2015-01-02T12:34:57,124,34\n2015-01-02T12:34:58,125,12\n"
  let xs = IO.fromCsv csv2
  assert $ A.length xs == 2


fromFileTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS  | eff) Unit
fromFileTest = do
  log "Test load 60K from file"
  csv <- readTextFile UTF8 "testdata/test60k.csv"
  let s1 = fromMaybe TS.empty (A.index (IO.fromCsv csv) 0)
  assert $ TS.length s1 == 59041
  -- Index should increase
  let idx1 = TS.index s1
  let idx2 = A.zip idx1 (fromMaybe [] (A.tail idx1))
  let idx3 = A.filter (\tu -> (fst tu) > (snd tu)) idx2
  assert $ A.length idx3 == 0


toCsvTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION, fs :: FS  | eff) Unit
toCsvTest = do
  log "Save to CSV"
  let csv1 = "time,value\n2015-01-02T12:34:56.000Z,123.45\n2015-01-02T12:34:57.000Z,124.0\n2015-01-02T12:34:58.000Z,125.0\n"
  let s1 = fromMaybe TS.empty $ A.head $ IO.fromCsv csv1
  assert $ IO.toCsv s1 == csv1
