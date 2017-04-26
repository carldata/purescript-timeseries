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
    , index 
    , values
    , filter
    , length
    , rollingWindow
    , slice
    , toDataPoints
    , zipWith
    ) where

import Prelude
import Data.Array as A
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, fromJust)
import Data.TimeSeries.Time (Timestamp)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)


-- | Data point is a time indexed value
type DataPoint a = { index :: Timestamp, value :: a }
                 
-- | Data structure for holding Series.
data Series a = Series (Array Timestamp) (Array a)


-- | Create data point
dataPoint :: ∀ a. Timestamp -> a -> DataPoint a
dataPoint i v = { index: i, value: v }


-- | Get Series index
index :: ∀ a. Series a -> Array Timestamp
index (Series idx _) = idx 

-- | Get Series values 
values :: ∀ a. Series a -> Array a 
values (Series _ vs) = vs 


--  | Create series from indexes and values
series :: ∀ a. Array Timestamp -> Array a -> Series a 
series is vs = Series is vs


-- | Create an empty series
empty :: ∀ a. Series a
empty = series [] []


-- | Create series from DateTime and value.
fromDataPoints :: ∀ a. Array (DataPoint a) -> Series a
fromDataPoints xs = series (map _.index xs) (map _.value xs)


-- | Conver to array of data points
toDataPoints :: ∀ a. Series a -> Array (DataPoint a)
toDataPoints (Series idx vs) = (\t -> dataPoint (fst t) (snd t)) <$> A.zip idx vs


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
length (Series idx vs) = A.length idx


-- | Get subseries
slice :: ∀ a. Timestamp     -- ^ Start time (inclusive)
      -> Timestamp          -- ^ End time (inclusive)
      -> Series a           -- ^ Input series
      -> Series a           -- ^ Sliced Series
slice start end (Series idx vs) = series (A.slice i j idx) (A.slice i j vs)
    where 
        n = A.length idx
        i = fromMaybe n $ A.findIndex (\x -> x >= start) idx
        j = fromMaybe n $ A.findLastIndex (\x -> x <= end) idx


-- | Filter a series, keeping the elements which satisfy a predicate function, creating a new series.
filter :: ∀ a. (a -> Boolean) -> Series a -> Series a
filter pred (Series idx vs) = series (fst tu) (snd tu)
    where 
        -- zip
        xs1 = A.zip idx vs
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
rollingWindow n agg (Series idx vs) = series idx2 $ rolling' (A.take n1 vs) agg (A.drop n1 vs)
    where 
        n1 = n-1
        idx2 = A.drop n1 idx

rolling' :: ∀ a b. Array a -> (Array a -> b) -> Array a -> Array b 
rolling' mu agg xs = (A.foldl f {wnd: mu, out: []} xs).out
    where
      f {wnd: ws, out: os} x = {wnd: wnd2, out: A.snoc os (agg wnd1)}
        where 
            wnd1 = A.snoc ws x
            wnd2 = A.drop 1 wnd1
