module Data.TimeSeries 
    ( DataPoint
    , Series
    , dataPoint
    , empty
    , fromValues
    , length
    , series
    , slice
    , values
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
fromValues xs = Series $ A.zipWith dataPoint idx xs
    where 
        idx = mkIndex (A.length xs)
    

-- Create index starting from lowest date (bottom)
-- In each step time is increased by 1 second
mkIndex :: Int -> Array DateTime
mkIndex n = map adjustedTime (A.range 1 n)
    where
        adjustedTime :: Int -> DateTime
        adjustedTime dt = unsafePartial $ fromJust $ adjust (Seconds (toNumber dt)) bottom


-- | Get series length.
length :: ∀ a. Series a -> Int
length (Series xs) = A.length xs


-- | Get values
values :: ∀ a. Series a -> Array a
values (Series xs) = map (\x -> x.value) xs

-- | Get subseries
slice :: ∀ a. DateTime  -- ^ Start time (inclusive)
      -> DateTime       -- ^ End time (inclusive)
      -> Series a       -- ^ Input series
      -> Series a       -- ^ Sliced Series
slice start end (Series xs) = Series $ A.filter (\x -> x.index >= start && x.index <= end) xs
