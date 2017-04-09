module Test.Data.TimeSeries.Time.Parser (testTimeParser) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Date (canonicalDate)
import Data.DateTime (date)
import Data.Enum (toEnum)
import Data.Maybe (fromMaybe)
import Data.TimeSeries.Time.Parser (parseISODateTime)
import Test.Assert (assert, ASSERT)


testTimeParser :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testTimeParser = do

    let d1 = fromMaybe bottom $ canonicalDate <$> toEnum 2015 <*> toEnum 1 <*> toEnum 1
    let d2 = fromMaybe bottom $ canonicalDate <$> toEnum 2015 <*> toEnum 7 <*> toEnum 1
    let d3 = fromMaybe bottom $ canonicalDate <$> toEnum 2015 <*> toEnum 7 <*> toEnum 3
    
    log "Parse Year"
    assert $ date (parseISODateTime "2015") == d1

    log "Parse Year"
    -- log $ show $ parseISODateTime "2015-12-03"
    assert $ date (parseISODateTime "2015-07") == d2

    log "Parse Year"
    -- log $ show $ parseISODateTime "2015-12-03"
    assert $ date (parseISODateTime "2015-07-03") == d3
