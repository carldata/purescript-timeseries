module Data.TimeSeries 
    ( DataPoint
    , Series
    , dataPoint
    -- * Constructors
    , empty
    , fromDataPoints
    , fromValues
    , series
    -- Data access
    , dpIndex 
    , dpValue
    , drop
    , head 
    , index 
    , last
    , slice
    , resolution
    , values
    -- Data operations
    , diff
    , ensureInc
    , integrate
    , filter
    , groupBy
    , length
    , reindex
    , rollingWindow
    , take
    , toDataPoints
    , zipWith
    ) where

import Prelude
import Data.Array as A
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.TimeSeries.Time (Timestamp)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)
import Statistics.Sample as S


-- | Data point is a time indexed value
data DataPoint a = DP Timestamp a
                 
-- | Data structure for holding Time Series.
data Series a = Series (Array Timestamp) (Array a)


instance showDataPoint :: Show a => Show (DataPoint a) where 
    show (DP idx v) = "{ index: " <> show idx <> ", value: " <> show v <> " }"

instance eqDataPoint :: Eq a => Eq (DataPoint a) where 
    eq (DP idx1 vs1) (DP idx2 vs2) = (eq idx1 idx2) && (eq vs1 vs2)

instance showSeries :: Show a => Show (Series a) where 
    show (Series idx vs) = "{ index: " <> show idx <> "\n, values: " <> show vs <> "\n}"

instance eqSeries :: Eq a => Eq (Series a) where 
    eq (Series idx1 vs1) (Series idx2 vs2) = (eq idx1 idx2) && (eq vs1 vs2)

instance functorSeries :: Functor Series where 
    map f (Series idx vs) = Series idx (map f vs)


-- | Create data point
dataPoint :: ∀ a. Timestamp -> a -> DataPoint a
dataPoint i v = DP i v

-- Create zero data point
dpZero :: DataPoint Number 
dpZero = dataPoint 0.0 0.0


-- | Get DataPoint index
dpIndex :: ∀ a. DataPoint a -> Timestamp
dpIndex (DP i _) = i

-- | Get DataPoint value
dpValue :: ∀ a. DataPoint a -> a
dpValue (DP _ v) = v


-- | Get Series index
index :: ∀ a. Series a -> Array Timestamp
index (Series idx _) = idx 

-- | Get Series values 
values :: ∀ a. Series a -> Array a 
values (Series _ vs) = vs 


--  | Create series from indexes and values
series :: ∀ a. Array Timestamp -> Array a -> Series a 
series is vs = Series (fst tp) (snd tp)
    where
        tp = ensureInc is vs


-- | Create an empty series
empty :: ∀ a. Series a
empty = series [] []

-- | Ensure that index is monotonically increasing
ensureInc :: ∀ a. Array Timestamp -> Array a -> Tuple (Array Timestamp) (Array a)
ensureInc is vs = Tuple (A.slice 0 n is) (A.slice 0 n vs)
    where
        n = A.length $ A.takeWhile f $ g is        
        f x = fst x >= snd x            
        g axs = A.zip axs (A.cons 0.0 axs)  

-- | Create series from DataPoints.
fromDataPoints :: ∀ a. Array (DataPoint a) -> Series a
fromDataPoints xs = series (map dpIndex xs) (map dpValue xs)


-- | Convert series to the array of data points
toDataPoints :: ∀ a. Series a -> Array (DataPoint a)
toDataPoints (Series idx vs) = (\t -> dataPoint (fst t) (snd t)) <$> A.zip idx vs


-- | Create series from values.
-- | Index will be based on Timestamp starting from 0
fromValues :: ∀ a. Array a -> Series a
fromValues xs = series (mkIndex (A.length xs)) xs
    where 
        mkIndex :: Int -> Array Timestamp
        mkIndex n = map (\x -> 1000.0 * (toNumber x)) (A.range 0 (n-1))


-- | Get series length.
length :: ∀ a. Series a -> Int
length (Series idx vs) = A.length idx


-- | Get first element if exists
head :: ∀ a. Series a -> Maybe (DataPoint a)
head (Series idx vs) = do
    i <- A.head idx 
    v <- A.head vs 
    pure $ dataPoint i v


-- | Get last element if exists
last :: ∀ a. Series a -> Maybe (DataPoint a)
last (Series idx vs) = do
    i <- A.last idx 
    v <- A.last vs 
    pure $ dataPoint i v


-- | Get distance between points in this series
resolution :: ∀ a. Series a -> Number 
resolution (Series idx _) = fromMaybe 0.0 $ S.mode idx1
    where 
        idx1 = A.zipWith (\x1 x2 -> x2-x1) idx (fromMaybe [] (A.tail idx))


-- | Get subseries within given range. Lower and upper range is inclusive.
slice :: ∀ a. Timestamp     -- ^ Start time (inclusive)
      -> Timestamp          -- ^ End time (exclusive)
      -> Series a           -- ^ Input series
      -> Series a           -- ^ Sliced Series
slice start end (Series idx vs) = series (A.slice i j idx) (A.slice i j vs)
    where 
        n = A.length idx
        i = fromMaybe n $ A.findIndex (\x -> x >= start) idx
        j = fromMaybe n (A.findLastIndex (\x -> x <= end) idx) + 1


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


-- | Take n first elements of the series. Drop rest
take :: ∀ a. Int -> Series a -> Series a
take n (Series idx vs) = Series (A.take n idx) (A.take n vs)


-- | Drop n first elements of the series. Keep rest
drop :: ∀ a. Int -> Series a -> Series a
drop n (Series idx vs) = Series (A.drop n idx) (A.drop n vs)


-- | Join 2 series using given function.
-- |
-- | Series will be joinned on index. It means that both series need to have the same index entries.
-- |
-- | If only 1 series contains given index then this item will be dropped
zipWith :: ∀ a b c. (a -> b -> c) -> Series a -> Series b -> Series c 
zipWith f xs ys = fromDataPoints $ zipWith' f (toDataPoints xs) (toDataPoints ys)

-- Helper function which works on data points
zipWith' :: ∀ a b c. (a -> b -> c) -> Array (DataPoint a) -> Array (DataPoint b) -> Array (DataPoint c)
zipWith' f xs ys = snd $ A.foldl g (Tuple ys []) xs 
    where
        g :: Tuple (Array (DataPoint b)) (Array (DataPoint c)) -> DataPoint a -> Tuple (Array (DataPoint b)) (Array (DataPoint c))
        g tu x = if A.null ys' then tu 
                 else if (dpIndex y') > (dpIndex x) then Tuple ys' (snd tu)
                 else Tuple yt' (A.snoc (snd tu) (dataPoint (dpIndex x) (f (dpValue x) (dpValue y'))))
            where 
                ys' = A.dropWhile (\y -> (dpIndex y) < (dpIndex x)) (fst tu)
                y' = unsafePartial $ fromJust $ A.head ys'
                yt' = unsafePartial $ fromJust $ A.tail ys'


-- | Apply function to the rolling window and create new Series.
-- |
-- | Value is put at the last element of the window
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


-- | Differentiate Series xt = xt - x(t-1)
diff :: ∀ a. Ring a => Series a -> Series a 
diff (Series idx vs) = Series (A.drop 1 idx) vs2 
    where
        vs2 = A.zipWith (-) (A.drop 1 vs) vs


-- | Integrate Series.
integrate :: ∀ a. Ring a => Series a -> Series a 
integrate (Series idx vs) = Series idx v2
    where
        v2 = snd (A.foldl f (Tuple zero []) vs)
        f (Tuple a os) x = Tuple (a+x) (A.snoc os (a+x))


-- | Group series points. 
-- |
-- | First Number defines window size in milliseconds
groupBy :: ∀ a b. Number    -- ^ Window size in milliseconds
        -> (Array a -> b)   -- ^ Function applied to group values
        -> Series a         -- ^ Input series
        -> Series b         -- ^ Grouped series
groupBy dt f xs = fromDataPoints $ groupBy' dt f (toDataPoints xs)


groupBy' :: ∀ a b. Number -> (Array a -> b) -> Array (DataPoint a) -> Array (DataPoint b)
groupBy' dt f xs = snd $ A.foldl g (Tuple [] []) xs
    where
        g :: Tuple (Array (DataPoint a)) (Array (DataPoint b)) 
          -> DataPoint a 
          -> Tuple (Array (DataPoint a)) (Array (DataPoint b))
        g (Tuple acc out) x = case A.uncons acc of
            Just {head: y, tail} -> if (dpIndex x) < (dpIndex y) + dt 
                                    then Tuple (A.snoc acc x) out
                                    else Tuple [x] (A.snoc out (dataPoint (dpIndex y) (f' acc)))
            Nothing -> Tuple [x] out
        f' :: Array (DataPoint a) -> b
        f' ds = f $ map dpValue ds


-- | Reindex series. Ensure that series has point at each index value
-- | So there are no missing values. 
reindex :: Number -> Series Number -> Series Number
reindex dt xs = fromDataPoints $ reindex' dt (toDataPoints xs) [dp]
    where 
        dp = fromMaybe dpZero $ head xs


-- This function is implemented with ifs because PureScript doesn't support where
-- clausule in guards
reindex' :: Number                      -- ^ Time delta
         -> Array (DataPoint Number)    -- ^ Imput array
         -> Array (DataPoint Number)    -- ^ Partial output array. build so far
         -> Array (DataPoint Number)    -- ^ Output
reindex' dt [] out = out
reindex' dt xs out = if nidx < dpIndex x then reindex' dt xs (A.snoc out (dataPoint nidx mu))
                     else if nidx == dpIndex x then reindex' dt ts (A.snoc out x)
                     else reindex' dt ts out
    where
        x = fromMaybe dpZero $ A.head xs
        ts = fromMaybe [] $ A.tail xs
        y = fromMaybe dpZero $ A.last out
        nidx = dpIndex y + dt
        mu = (ty/(tx+ty)) * dpValue x + ((tx/(tx+ty)) * dpValue y)
        tx = (dpIndex x) - nidx
        ty = nidx - (dpIndex y)


