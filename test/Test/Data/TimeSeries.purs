module Test.Data.Series (testSeries) where

import Prelude
import Data.TimeSeries as TS
import Data.TimeSeries.Time as T
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)


testSeries :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testSeries = do

    log "Init test series"
    let emptySeries = TS.empty
    let s1 = TS.fromValues [10.0, 1.2, 32.4, 0.65, 11.0]

    log "Empty series has length 0"
    assert $ TS.length emptySeries == 0

    log "Return series size" 
    assert $ TS.length s1 == 5

    log "Return values" 
    assert $ TS.values s1 == [10.0, 1.2, 32.4, 0.65, 11.0]

    -- log "Slicing"
    -- let start = T.fromSeconds 2.0
    -- let end = T.fromSeconds 4.0
    -- log $ show $ TS.length (TS.slice start end s1)
    -- assert $ TS.length (TS.slice start end s1) == 3

    -- it "return data point value at given index" $ do
    --     let pos = posixSecondsToUTCTime 2
    --     TS.valueAt pos sampleSeries `shouldBe` Just 1.2

    -- it "return Nothing value if wrong index" $ do
    --     let idx = [10, 20, 30]
    --     let values = [10.0, 12.0, 32.4]
    --     let pos = posixSecondsToUTCTime 1234
    --     TS.valueAt pos (TS.tsSeries idx values) `shouldBe` Nothing

    -- it "return subset" $ do
    --     let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
    --     let start = utcFromSeconds 2
    --     let end = utcFromSeconds 3
    --     TS.size (TS.slice start end xs) `shouldBe` 2

    -- it "return empty subset" $ do
    --     let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
    --     let start = utcFromSeconds 12
    --     let end = utcFromSeconds 34
    --     TS.slice start end xs `shouldBe` TS.emptySeries    
