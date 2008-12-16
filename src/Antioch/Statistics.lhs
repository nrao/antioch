> module Antioch.Statistics where

> import Antioch.Types
> import Data.List
> import Data.Time.Clock

> exProject = defaultProject {
>    pName = "testProject"
>  , semester = "08B"
>  }

> exSession = defaultSession {
>    sName = "test"
>  , project = exProject
>  , frequency = 1.2
>  , totalTime = 12
>  , totalUsed = 9
>  }

> exSession2 = defaultSession { sName = "test2"
>                             , project = exProject
>                             , frequency = 4.2
>                             , totalTime = 24
>                             , totalUsed = 0 }

> exSessions = [exSession, exSession2]

> exPeriods :: [Period]
> exPeriods = [defaultPeriod { session = exSession
>                          , startTime = gimmeTime 2008 1 1 0 
>                          , duration  = 2 }
>            , defaultPeriod { session = exSession
>                          , startTime = gimmeTime 2008 1 1 (3600*5)
>                          , duration  = 4 }
>            , defaultPeriod { session = exSession
>                          , startTime = gimmeTime 2008 1 2 0
>                          , duration  = 3 }]

Would like to do this, but with 'time + diffTime'.  How to?
 exPeriods :: IO [Period]
 exPeriods = do
     time <- getCurrentTime
     return [defaultPeriod { session = exSession, startTime = time }
       , defaultPeriod { session = exSession, startTime = time }
       , defaultPeriod { session = exSession, startTime = time }]

Example of scatter plot data w/ datetime:

> freqTime :: [Period] -> [(UTCTime, Double)]
> freqTime xs = [ (startTime x, frequency . session $ x) | x <- xs]

Example of log histogram data:
Compare allocated hours by frequency to observed hours by frequency.

> sessFreq    :: [Session] -> [(Double, Minutes)]
> sessFreq xs = histogram [1.0 .. 50.0] [(frequency x, totalTime x) | x <- xs]

> periodFreq    :: [Period] -> [(Double, Minutes)]
> periodFreq xs = histogram [1.0 .. 50.0]
>     [(frequency . session $ x, duration x) | x <- xs]

> sessTP :: [Period] -> [(Minutes, Int)]
> sessTP xs = histogram [1 .. 17] [(duration x, 1) | x <- xs]

> histogram :: (Ord a, Ord b, Num b) => [a] -> [(a, b)] -> [(a, b)]
> histogram buckets values = allocate buckets . sort $ values
>   where
>     allocate []     _   = []
>     allocate (b:bs) xys = (b, sum . map snd $ within) : allocate bs without
>       where
>         (within, without) = span (\(x, _) -> x <= b) xys
