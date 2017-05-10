-- | Helper function for DateTime operations
module Data.TimeSeries.Time (Timestamp, seconds, minutes, hours) where

import Prelude
import Data.Int (toNumber)


-- | Time stamp in milliseconds as defined by JS Date function
type Timestamp = Number

-- | Convert given number of seconds into Timestamp value.
seconds :: Int -> Timestamp
seconds n = toNumber (1000 * n)

-- | Convert given number of minutes into Timestamp value.
minutes :: Int -> Timestamp
minutes n = toNumber (60000 * n)

-- | Convert given number of hours into Timestamp value.
hours :: Int -> Timestamp
hours n = toNumber (60*60*1000 * n)
