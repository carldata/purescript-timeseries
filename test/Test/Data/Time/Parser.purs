module Test.Data.TimeSeries.Time.Parser (testTimeParser) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..), Time(..), date)
import Data.Enum (toEnum)
import Data.Maybe (fromMaybe)
import Data.TimeSeries.Time.Parser (parseISODateTime)
import Test.Assert (assert, ASSERT)


testTimeParser :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testTimeParser = do

    let d1 = fromMaybe bottom $ canonicalDate <$> toEnum 2015 <*> toEnum 1 <*> toEnum 1
    let d2 = fromMaybe bottom $ canonicalDate <$> toEnum 2015 <*> toEnum 7 <*> toEnum 1
    let d3 = fromMaybe bottom $ canonicalDate <$> toEnum 2015 <*> toEnum 7 <*> toEnum 3
    let t1 = fromMaybe bottom $ Time <$> toEnum 16 <*> toEnum 34 <*> toEnum 0 <*> toEnum 0
    let t2 = fromMaybe bottom $ Time <$> toEnum 16 <*> toEnum 34 <*> toEnum 52 <*> toEnum 0
    
    log "Parse yyyy"
    assert $ date (parseISODateTime "2015") == d1

    log "Parse yyyy-mm"
    assert $ date (parseISODateTime "2015-07") == d2

    log "Parse yyyy-mm-dd"
    assert $ date (parseISODateTime "2015-07-03") == d3

    log "Parse yyyy-mm-ddThh:mm"
    assert $ parseISODateTime "2015-07-03T16:34" == DateTime d3 t1

    log "Parse yyyy-mm-ddThh:mm:ss"
    assert $ parseISODateTime "2015-07-03T16:34:52" == DateTime d3 t2
