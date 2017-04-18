module Data.TimeSeries.IO (fromCsv) where
  
import Prelude
import Data.Array as A
import Data.Maybe (Maybe)
import Data.String as S
import Data.TimeSeries.Time.Parser (parseISOTime)
import Global (readFloat)

import Data.TimeSeries as TS


-- | Load TimeSeries from CSV at given URL
fromCsv :: String -> TS.Series Number
fromCsv  = TS.fromDataPoints <<< parseCsv

-- | Load TimeSeries from CSV at given URL
parseCsv :: String -> Array (TS.DataPoint Number)
parseCsv str = A.mapMaybe (parseRow <<< fields) lines
    where 
        lines = S.split (S.Pattern "\n") str
        fields = (S.split (S.Pattern ","))  

-- Parse single row
parseRow :: Array String -> Maybe (TS.DataPoint Number)
parseRow row = do
    r1 <- A.index row 0
    i <- parseISOTime r1
    v <- readFloat <$> (A.index row 1)
    pure $ TS.dataPoint i v


