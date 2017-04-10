module Data.TimeSeries.IO (fromCsv) where
  
import Prelude  
import Data.TimeSeries as TS
import Data.Either (Either(..))
import Data.List (List, mapMaybe, (:), toUnfoldable)
import Data.Maybe (Maybe(..))
import Text.Parsing.CSV (defaultParsers)
import Text.Parsing.Parser (runParser)
import Data.TimeSeries.Time.Parser (parseISOTime)
import Global (readFloat)


-- | Load TimeSeries from CSV at given URL
fromCsv :: String -> TS.Series Number
fromCsv str = case runParser str defaultParsers.file of 
    Left _ -> TS.empty
    Right rows -> 
        let xs  = mapMaybe parseRow rows
        in TS.series (toUnfoldable xs)


-- Parse single row
parseRow :: List String -> Maybe (TS.DataPoint Number)
parseRow (x : y : _) = (\i -> TS.dataPoint i (readFloat y)) <$> parseISOTime x
parseRow _ = Nothing


