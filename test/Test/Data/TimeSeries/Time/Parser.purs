module Test.Data.TimeSeries.Time.Parser (testTimeParser) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..))
import Data.TimeSeries.Time.Parser (parseISOTime)
import Test.Assert (assert, ASSERT)



testTimeParser :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testTimeParser = do

    log "\n# Time parser tests"

    log "Parse yyyy"
    assert $ parseISOTime "2015" == Just 1420070400000.0

    log "Parse yyyy-mm"
    assert $ parseISOTime "2015-07" == Just 1435708800000.0

    log "Parse yyyy-mm-dd"
    assert $ parseISOTime "2015-07-03" == Just 1435881600000.0

    log "Parse yyyy-mm-ddThh:mm"
    assert $ parseISOTime "2015-07-03T16:34Z" == Just 1435941240000.0

    log "Parse yyyy-mm-ddThh:mm:ss"
    assert $ parseISOTime "2015-07-03T16:34:52Z" == Just 1435941292000.0

    log "Parse yyyy-mm-ddThh:mm:ss.ms"
    assert $ parseISOTime "2015-07-03T16:34:52.123Z" == Just 1435941292123.0

    log "Parse not a date"
    assert $ parseISOTime "not date" == Nothing

