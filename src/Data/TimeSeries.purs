module Data.TimeSeries 
    ( DataPoint
    , Series
    , dataPoint
    -- * Constructors
    , empty
    , fromDataPoints
    , fromValues
    , series
    , mkIndex
    -- Data operations
    , filter
    , length
    , slice
    , toDataPoints
    , zipWith
    ) where

import Prelude
import Data.Array as A
import Data.TimeSeries.Time as T
import Data.DateTime (DateTime)
import Data.Maybe (fromMaybe, fromJust)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)


-- | Data points is a time indexed value
type DataPoint a = { index :: DateTime, value :: a }
                 
-- | Data structure for holding Series.
type Series a = { index :: Array DateTime  
                , values :: Array a 
                }


-- Create data point
dataPoint :: ∀ a. DateTime -> a -> DataPoint a
dataPoint i v = { index: i, value: v }


--  | Create series from indexes and values
series :: ∀ a. Array DateTime -> Array a -> Series a 
series is vs = {index: is, values: vs}


-- | Create an empty series
empty :: ∀ a. Series a
empty = series [] []


-- | Create series from DateTime and value.
fromDataPoints :: ∀ a. Array (DataPoint a) -> Series a
fromDataPoints xs = series (map _.index xs) (map _.value xs)


-- | Conver to array of data points
toDataPoints :: ∀ a. Series a -> Array (DataPoint a)
toDataPoints xs = (\t -> dataPoint (fst t) (snd t)) <$> A.zip xs.index xs.values


-- | Create series from values.
-- | Index will be based on Instant starting from 0
fromValues :: ∀ a. Array a -> Series a
fromValues xs = series (mkIndex (A.length xs)) xs
    

-- Create index starting from lowest date (bottom)
-- In each step time is increased by 1 second
mkIndex :: Int -> Array DateTime
mkIndex n = map T.fromSeconds (A.range 1 n)


-- | Get series length.
length :: ∀ a. Series a -> Int
length xs = A.length xs.index


-- | Get subseries
slice :: ∀ a. DateTime  -- ^ Start time (inclusive)
      -> DateTime       -- ^ End time (inclusive)
      -> Series a       -- ^ Input series
      -> Series a       -- ^ Sliced Series
slice start end xs = series (A.slice i j xs.index) (A.slice i j xs.values)
    where 
        n = length xs
        i = fromMaybe n $ A.findIndex (\x -> x >= start) xs.index
        j = fromMaybe n $ A.findLastIndex (\x -> x <= end) xs.index


-- | Filter a series, keeping the elements which satisfy a predicate function, creating a new series.
filter :: ∀ a. (a -> Boolean) -> Series a -> Series a
filter pred xs = series is vs
    where 
        -- zip
        xs1 = A.zip xs.index xs.values
        -- filter zipped
        xs2 = A.filter (\t -> pred (snd t)) xs1
        -- Unzip
        Tuple is vs = A.unzip xs2


-- | Join 2 series using function
-- | Series will be joinned on index. It means that both series need to have the same index entries
-- | If only 1 series contains given index then this item will be dropped
zipWith :: ∀ a b c. (a -> b -> c) -> Series a -> Series b -> Series c 
zipWith f xs ys = fromDataPoints $ zipWith' f (toDataPoints xs) (toDataPoints ys)

-- Helper function which works on data points
zipWith' :: ∀ a b c. (a -> b -> c) -> Array (DataPoint a) -> Array (DataPoint b) -> Array (DataPoint c)
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f xs ys = if x.index > y.index then zipWith' f xt ys
                   else if x.index < y.index then zipWith' f xs yt
                   else [dataPoint x.index (f x.value y.value)] <> zipWith' f xt yt
    where
        -- It is ok to use partial functions here since we already checked that arrays are not empty
        x = unsafePartial fromJust $ A.head xs
        xt = unsafePartial fromJust $ A.tail xs
        y = unsafePartial fromJust $ A.head ys
        yt = unsafePartial fromJust $ A.tail ys