-- | Helper function for DateTime operations
module Data.TimeSeries.Time where

import Prelude
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))


-- | Convert Timestamp in seconds into the DateTime
fromSeconds :: Int -> DateTime
fromSeconds n = toDateTime ts
  where 
    ms = Milliseconds $ toNumber (1000 * n)
    ts = fromMaybe bottom (instant ms)
