module Data.TimeSeries.Session
    ( Session(..)
    , find
    , session
    , sessionEnd
    , sessionStart
    ) where

import Prelude
import Data.Array as A
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst)

import Data.TimeSeries as TS
import Data.TimeSeries.Time (Timestamp)


-- | Session is a time range of a single event
-- | Some examples:
-- |
-- |   * Session with web application
-- |
-- |   * Rain event
-- |
data Session = Session Timestamp Timestamp -- ^ Session start and edn date

instance sessionShow :: Show Session where 
    show (Session x y) = "Session from: " <> show x <> " to: " <> show y 

instance sessionEq :: Eq Session where 
    eq (Session x1 y1) (Session x2 y2) = x1 == x2 && y1 == y2
                       
-- | Session start 
sessionStart :: Session -> Timestamp 
sessionStart (Session x _) = x

-- | Session end
sessionEnd :: Session -> Timestamp 
sessionEnd (Session _ y) = y

-- | Create session
session :: Timestamp -> Timestamp -> Session 
session x y = Session x y 


-- | Find all session in a given Time Series
find :: Timestamp -> TS.Series Number -> Array Session
find dt xs = joinSessions dt ys1
    where 
        ys1 = cnv [] $ findChanges (TS.toDataPoints xs)


-- Helper function for finding all places where value changes from 0 to any other value
-- Return tuple with timestamp and true if value changes from 0, and false if value changes to 0
findChanges :: Array (TS.DataPoint Number) -> Array Timestamp
findChanges ds = fst $ A.foldl f (Tuple [] zero) ds 
    where 
        f :: Tuple (Array Timestamp) Number 
          -> TS.DataPoint Number 
          -> Tuple (Array Timestamp) Number
        f (Tuple ss x) p 
            | x == zero && TS.dpValue p /= zero = 
                Tuple (A.snoc ss (TS.dpIndex p)) (TS.dpValue p)
            | x /= zero && TS.dpValue p == zero = 
                Tuple (A.snoc ss (TS.dpIndex p)) (TS.dpValue p)
            | otherwise = 
                (Tuple ss x)

-- Convert list of changes into sessions
cnv :: Array Session -> Array Timestamp -> Array Session
cnv ss [] = ss
cnv ss [_] = ss
cnv ss ts = cnv (A.snoc ss (session x y)) (A.drop 2 ts) 
    where 
        x = fromMaybe 0.0 $ A.index ts 0
        y = fromMaybe 0.0 $ A.index ts 1

-- Join events close to each other into single session
joinSessions :: Timestamp -> Array Session -> Array Session
joinSessions dt ss = A.foldl f [] ss 
    where 
        f :: Array Session -> Session -> Array Session
        f [] s = [s]
        f xs s = if (sessionEnd y) + dt > (sessionStart s)
                 then A.snoc (A.take (A.length xs - 1) xs) (mergeSessions y s)
                 else A.snoc xs s
            where 
                y = fromMaybe (session 0.0 0.0) (A.last xs)


-- Join 2 seesions
mergeSessions :: Session -> Session -> Session 
mergeSessions (Session x _) (Session _ y) = Session x y