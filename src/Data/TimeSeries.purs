module Data.TimeSeries 
    ( DataPoint(..)
    , Series
    , empty
    , fromValues
    , length
    , series
    ) where

import Prelude
import Data.Array as A
import Data.DateTime (DateTime, adjust)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Time.Duration (Seconds(Seconds))
import Partial.Unsafe (unsafePartial)


-- | Data points is a time indexed value
type DataPoint a = { index :: DateTime, value :: a }
                 
-- | Data structure for holding Series.
data Series a = Series (Array (DataPoint a))


-- Create data point
dataPoint :: ∀ a. DateTime -> a -> DataPoint a
dataPoint i v = { index: i, value: v }

-- | Create an empty series
empty :: ∀ a. Series a
empty = Series []

-- | Create series from DateTime and value.
series :: ∀ a. Array (DataPoint a) -> Series a
series xs = Series xs

-- | Create series from values.
-- | Index will be based on Instant starting from 0
fromValues :: ∀ a. Array a -> Series a
fromValues xs = Series $ A.zipWith dataPoint (mkIndex (A.length xs)) xs

-- Array of instants from time 0
mkIndex :: Int -> Array DateTime
mkIndex n = map adjustedTime (A.range 1 n)
    where
        adjustedTime :: Int -> DateTime
        adjustedTime dt = unsafePartial $ fromJust $ adjust (Seconds (toNumber dt)) bottom


-- | Get series length.
length :: ∀ a. Series a -> Int
length (Series xs) = A.length xs
