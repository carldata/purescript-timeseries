module Test.Data.TimeSeries.Time.Parser (testTimeParser) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Date (year)
import Data.DateTime (date)
import Data.Enum (toEnum)
import Data.Maybe (fromMaybe)
import Data.TimeSeries.Time.Parser (parseISODateTime)
import Test.Assert (assert, ASSERT)


testTimeParser :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testTimeParser = do

    log "Parse Year"
    -- log $ show $ parseISODateTime "2015"
    assert $ year (date (parseISODateTime "2015")) == fromMaybe (year bottom) (toEnum 2015)

