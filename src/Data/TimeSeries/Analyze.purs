-- | Analyze points in the Time Series
module Data.TimeSeries.Analyze (findMissing, findDifference) where


import Prelude 
import Data.Array as A
import Data.Array.Partial as AP
import Data.Maybe (fromMaybe)
import Partial.Unsafe (unsafePartial)

import Data.TimeSeries as TS 
import Data.TimeSeries.Time (Timestamp)


-- | Find missing points in Time Series.
-- | Points are estimated from provided time resolution
findMissing :: ∀ a. TS.Series a -> Timestamp -> Array Timestamp
findMissing xs dt = findMissing' si dt (TS.index xs) []
    where 
        si = fromMaybe 0.0 $ TS.dpIndex <$> TS.head xs

-- Helper function operating on Array of Timestamps
findMissing' :: Timestamp          -- ^ Previous Timestamp
             -> Timestamp          -- ^ Time delta
             -> Array Timestamp    -- ^ Imput array with timestamp from the series
             -> Array Timestamp    -- ^ Partial output array. Build so far
             -> Array Timestamp    -- ^ Output
findMissing' _ _ [] out = out
findMissing' idx dt xs out = 
  if idx < x then findMissing' nidx dt xs (A.snoc out idx)
  else if idx == x then findMissing' nidx dt ts out
  else findMissing' idx dt ts out
    where
      x = fromMaybe 0.0 $ A.head xs
      ts = fromMaybe [] $ A.tail xs
      nidx = idx + dt


-- | Find all points with different values in 2 Time Series
-- | Only consider points which are in both Series
-- | Return array with index entries of different points.
findDifference :: ∀ a. Ord a => TS.Series a -> TS.Series a -> Array Timestamp
findDifference xs ys = findDifference' (TS.toDataPoints xs) (TS.toDataPoints ys) []

findDifference' :: ∀ a. Ord a 
                => Array (TS.DataPoint a)   -- ^ First Series
                -> Array (TS.DataPoint a)   -- ^ Second series
                -> Array Timestamp          -- ^ Partial output array. build so far
                -> Array Timestamp          -- ^ Output array
findDifference' [] _ out = out
findDifference' _ [] out = out
findDifference' xs ys out = 
  if TS.dpIndex x > TS.dpIndex y then findDifference' xs yt out
  else if TS.dpIndex x < TS.dpIndex y then findDifference' xt ys out
  else findDifference' xt ys $ if TS.dpValue x == TS.dpValue y 
                               then out 
                               else A.snoc out (TS.dpIndex x)
    where 
      x = unsafePartial $ AP.head xs
      xt = unsafePartial $ AP.tail xs
      y = unsafePartial $ AP.head ys
      yt = unsafePartial $ AP.tail ys    