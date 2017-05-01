module Test.Data.TimeSeries (testSeries) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..))
import Test.Assert (assert, ASSERT)

import LinearAlgebra.Vector (sum)
import Data.TimeSeries as TS
import Data.TimeSeries.Time as T


testSeries :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSeries = do

    log "\n# Time Series base tests"
    lengthTest
    testValues
    testSlicing
    testHead
    testLast
    testResolution
    testFilter
    testDrop
    testTake
    testZipWith
    testRolling
    testMap
    testDiff
    testIntegrate
    testGroupBy


lengthTest :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
lengthTest = do
    log "* Return series size" 
    let s1 = TS.fromValues [1.3, 2.6, 3.4, 4.6, 5.0]
    assert $ TS.length TS.empty == 0
    assert $ TS.length s1 == 5


testValues :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testValues = do
    log "* Return values" 
    let xs = [1.3, 2.6, 3.4, 4.6, 5.0]
    let s1 = TS.fromValues  xs
    assert $ TS.values s1 == xs


testHead :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testHead = do
    log "* Return first element" 
    let s1 = TS.fromValues [1, 2, 3, 4, 5]
    assert $ TS.head s1 == Just (TS.dataPoint 0.0 1)


testLast :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testLast = do
    log "* Return last element" 
    let s1 = TS.fromValues [1.3, 2.6, 3.4, 4.6, 5.0]
    assert $ TS.last s1 == Just (TS.dataPoint 4000.0 5.0)


testResolution :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testResolution = do
    log "* Distance between points" 
    let s1 = TS.fromValues [1, 2, 3, 4, 5]
    log $ show $ TS.resolution s1
    assert $ TS.resolution s1 == 1000.0


testSlicing :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSlicing = do
    log "* Slicing"
    let s1 = TS.fromValues [1.3, 2.6, 3.4, 4.6, 5.0]
    let start2 = T.fromSeconds 2
    let start20 = T.fromSeconds 20
    let end4 = T.fromSeconds 4

    let slicing1 = TS.slice start2 end4 s1
    assert $ TS.values slicing1 == [3.4, 4.6]
    log "* Slicing - empty series if wrong indexes"
    let slicing2 = TS.slice start20 end4 s1
    assert $ TS.values slicing2 == []


testFilter :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testFilter = do
    log "* Filtering"
    let s2 = TS.fromValues [4.0, 2.6, 3.4, 4.6, 1.5]
    let filtered1 = TS.filter (_ < 3.0) s2
    assert $ TS.values filtered1 == [2.6, 1.5]


testDrop :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testDrop = do
    log "* Drop n first elements"
    let s1 = TS.fromValues [1, 2, 3, 4, 5]
    assert $ TS.drop 3 s1 == TS.series [3000.0, 4000.0] [4, 5]


testTake :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testTake = do
    log "* Take n first elements"
    let s1 = TS.fromValues [1, 2, 3, 4, 5]
    assert $ TS.take 3 s1 == TS.fromValues [1, 2, 3]


testZipWith :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testZipWith = do
    log "* zipWith"
    let s1 = TS.fromValues [1.3, 2.6, 3.4, 4.6, 5.0]
    let s2 = TS.fromValues [4.0, 2.6, 3.4, 4.6, 1.5]
    let z1 = TS.zipWith (+) s1 s2
    assert $ TS.values z1 == [5.3, 5.2, 6.8, 9.2, 6.5]


testRolling :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testRolling = do
    log "* Rolling window"
    let xs = TS.fromValues [1.0, 2.0, 3.0, 4.0, 5.0]
    let ys = TS.rollingWindow 3 sum xs
    assert $ ys == TS.series [2000.0, 3000.0, 4000.0] [6.0, 9.0, 12.0]


testMap :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testMap = do
    log "* Map"
    let xs = TS.fromValues [1.0, 2.0, 3.0, 4.0, 5.0]
    let ys = map ((*) 2.0) xs
    assert $ ys == TS.fromValues [2.0, 4.0, 6.0, 8.0, 10.0]


testDiff :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testDiff = do
    log "* Differentiate"
    let xs = TS.fromValues [1.0, 2.0, 3.0, 4.0, 5.0]
    assert $ TS.diff xs == TS.series [1000.0, 2000.0, 3000.0, 4000.0] [1.0, 1.0, 1.0, 1.0]


testIntegrate :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testIntegrate = do
    log "* Integrate"
    let xs = TS.fromValues [1, 2, 3, 4, 5]
    assert $ TS.integrate xs == TS.series [0.0, 1000.0, 2000.0, 3000.0, 4000.0] [1, 3, 6, 10, 15]


testGroupBy :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testGroupBy = do
    log "* GroupBy"
    let xs = TS.fromValues [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]
    assert $ TS.groupBy 2e3 sum xs == TS.series [0.0, 2000.0, 4000.0] [3.0, 7.0, 11.0]
