module Data.TimeSeries.IO (fromCsv) where
  
import Data.TimeSeries as TS
import Data.Either (Either(..))
import Data.List (tail)
import Data.Maybe (fromMaybe)
import Prelude (bottom)
-- import Text.Parsing.CSV (defaultParsers)
import Text.Parsing.Parser (runParser)


-- | Load TimeSeries from CSV at given URL
fromCsv :: String -> TS.Series Number
fromCsv _ = TS.empty
-- fromCsv str = case runParser str defaultParsers.file of 
--     Left _ -> TS.empty
--     Right rows -> TS.empty
        -- let ts = fromMaybe [] (tail rows)
        -- in 


-- Parse single row
-- parseRow :: List String -> Maybe (TS.DataPoint Number)
-- parseRow (x : y : _) = Just $ TS.DataPoint index values
--     where 
--         index = bottom
--         value = 0.0
-- parseRow _ = Nothing


