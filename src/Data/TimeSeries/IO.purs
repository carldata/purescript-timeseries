module Data.TimeSeries.IO (fromCsv, toCsv) where
  
import Prelude
import Data.Array as A
import Data.Maybe (Maybe, fromMaybe)
import Data.String as S
import Data.Tuple (Tuple(..), fst, snd)
import Global (readFloat)

import Data.TimeSeries.Time.Convert (parseISOTime, formatTime)
import Data.TimeSeries.Time (Timestamp)
import Data.TimeSeries as TS


-- Improve readability of this module
type Column = Array


-- | Load all columns from CSV. 
-- | First column is treated as a index for all Time Series. 
-- | So this function will return #column-1 Time Series
fromCsv :: String -> Array (TS.Series Number)
fromCsv str = map (\c -> TS.series idx c) cs
    where
        lines = S.split (S.Pattern "\n") str
        parsedLines = parseLines lines
        -- The following lines could be written as 
        -- Tuple idx cs
        idx = fst parsedLines
        cs = snd parsedLines


parseLines :: Array String -> Tuple (Column Timestamp) (Array (Column Number))
parseLines lines = Tuple (map fst rows) (toColumns (map snd rows))
    where rows = A.mapMaybe parseRow lines

-- Parse single line and return index with array of column values
parseRow :: String -> Maybe (Tuple Timestamp (Array Number))
parseRow str = do
    let fields = S.split (S.Pattern ",") str
    idx <- A.head fields  >>= parseISOTime
    vs <- (map readFloat) <$> A.tail fields
    pure $ Tuple idx vs

-- Convert Array with rows into Array with columns
toColumns :: ∀ a. Array (Array a) -> Array (Array a)
toColumns rows = map col (A.range 0 (len-1))
    where 
        len = fromMaybe 0 $ A.length <$> A.head rows
        col i = A.mapMaybe (\row -> A.index row i) rows


-- | Convert Series to the CSV format
toCsv :: ∀ a. Show a => TS.Series a -> String
toCsv xs = A.foldl f "time,value\n" $ TS.toDataPoints xs 
    where 
        f acc x = acc <> formatTime (TS.dpIndex x) <> "," <> show (TS.dpValue x) <> "\n"