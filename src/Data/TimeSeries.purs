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
    , rollingWindow
    , slice
    , toDataPoints
    , zipWith
    ) where

import Prelude
import Data.Array as A
import Data.Boolean (otherwise)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, fromJust)
import Data.TimeSeries.Time (Timestamp)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)


-- | Data point is a time indexed value
type DataPoint a = { index :: Timestamp, value :: a }
                 
-- | Data structure for holding Series.
type Series a = { index :: Array Timestamp  
                , values :: Array a 
                }


-- | Create data point
dataPoint :: ∀ a. Timestamp -> a -> DataPoint a
dataPoint i v = { index: i, value: v }


--  | Create series from indexes and values
series :: ∀ a. Array Timestamp -> Array a -> Series a 
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
-- | Index will be based on Timestamp starting from 0
fromValues :: ∀ a. Array a -> Series a
fromValues xs = series (mkIndex (A.length xs)) xs
    

-- | Create index starting from lowest date (bottom)
-- | In each step time is increased by 1 second
mkIndex :: Int -> Array Timestamp
mkIndex n = map (\x -> 1000.0 * (toNumber x)) (A.range 0 (n-1))


-- | Get series length.
length :: ∀ a. Series a -> Int
length xs = A.length xs.index


-- | Get subseries
slice :: ∀ a. Timestamp     -- ^ Start time (inclusive)
      -> Timestamp          -- ^ End time (inclusive)
      -> Series a           -- ^ Input series
      -> Series a           -- ^ Sliced Series
slice start end xs = series (A.slice i j xs.index) (A.slice i j xs.values)
    where 
        n = length xs
        i = fromMaybe n $ A.findIndex (\x -> x >= start) xs.index
        j = fromMaybe n $ A.findLastIndex (\x -> x <= end) xs.index


-- | Filter a series, keeping the elements which satisfy a predicate function, creating a new series.
filter :: ∀ a. (a -> Boolean) -> Series a -> Series a
filter pred xs = series (fst tu) (snd tu)
    where 
        -- zip
        xs1 = A.zip xs.index xs.values
        -- filter zipped
        xs2 = A.filter (\t -> pred (snd t)) xs1
        -- Unzip
        tu = A.unzip xs2


-- | Join 2 series using given function
-- | Series will be joinned on index. It means that both series need to have the same index entries
-- | If only 1 series contains given index then this item will be dropped
zipWith :: ∀ a b c. (a -> b -> c) -> Series a -> Series b -> Series c 
zipWith f xs ys = fromDataPoints $ zipWith' f (toDataPoints xs) (toDataPoints ys)

-- Helper function which works on data points
zipWith' :: ∀ a b c. (a -> b -> c) -> Array (DataPoint a) -> Array (DataPoint b) -> Array (DataPoint c)
zipWith' f xs ys = snd $ A.foldl g (Tuple ys []) xs 
    where
        g :: Tuple (Array (DataPoint b)) (Array (DataPoint c)) -> DataPoint a -> Tuple (Array (DataPoint b)) (Array (DataPoint c))
        g tu x = if A.null ys' then tu 
                 else if y'.index > x.index then Tuple ys' (snd tu)
                 else Tuple yt' (A.snoc (snd tu) (dataPoint x.index (f x.value y'.value)))
            where 
                ys' = A.dropWhile (\y -> y.index < x.index) (fst tu)
                y' = unsafePartial $ fromJust $ A.head ys'
                yt' = unsafePartial $ fromJust $ A.tail ys'


-- | Apply function to the rolling window and create new Series
rollingWindow :: ∀ a b. Int -> (Array a -> b) -> Series a -> Series b 
rollingWindow n f xs = series idx $ rolling' n f xs.values
    where 
        idx = A.drop (n - 1) xs.index

rolling' :: ∀ a b. Int -> (Array a -> b) -> Array a -> Array b 
rolling' n f xs 
    | A.length xs < n = []
    | otherwise = A.cons (f wnd) $ rolling' n f (A.drop 1 xs)
        where wnd = A.take n xs
