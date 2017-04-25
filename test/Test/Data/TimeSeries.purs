module Test.Data.TimeSeries (testSeries) where

import Prelude
import Data.TimeSeries as TS
import Data.TimeSeries.Time as T
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)


testSeries :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSeries = do

    log "\n# time Series base tests"
    lengthTest
    testValues
    testSlicing
    testFilter
    testZipWith


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
    assert $ s1.values == xs


testSlicing :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSlicing = do
    log "* Slicing"
    let s1 = TS.fromValues [1.3, 2.6, 3.4, 4.6, 5.0]
    let start2 = T.fromSeconds 2
    let start20 = T.fromSeconds 20
    let end4 = T.fromSeconds 4

    let slicing1 = TS.slice start2 end4 s1
    assert $ slicing1.values == [3.4, 4.6]
    log "Slicing - empty series if wrong indexes"
    let slicing2 = TS.slice start20 end4 s1
    assert $ slicing2.values == []


testFilter :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testFilter = do

    log "* Filtering"
    let s2 = TS.fromValues [4.0, 2.6, 3.4, 4.6, 1.5]
    let filtered1 = TS.filter (_ < 3.0) s2
    assert $ filtered1.values == [2.6, 1.5]

testZipWith :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testZipWith = do
    log "* zipWith"
    let s1 = TS.fromValues [1.3, 2.6, 3.4, 4.6, 5.0]
    let s2 = TS.fromValues [4.0, 2.6, 3.4, 4.6, 1.5]
    let z1 = TS.zipWith (+) s1 s2
    assert $ z1.values == [5.3, 5.2, 6.8, 9.2, 6.5]


-- let xs = TS.tsSeries [1..] [1.0, 2.0, 3.0, 4.0, 5.0]
-- TS.rolling (TS.seconds 2) sum xs `shouldBe` TS.tsSeries [2..] [3.0, 5.0, 7.0, 9.0]    
