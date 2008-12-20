> module Antioch.Statistics where

> import Antioch.DateTime (fromGregorian)
> import Antioch.Types
> import Data.List
> import Data.Time.Clock
> import Graphics.Gnuplot.Simple

> exProject = defaultProject {
>     pName    = "testProject"
>   , semester = "08B"
>   }

> exSession = defaultSession {
>     sName     = "test"
>   , project   = exProject
>   , frequency = 1.2
>   , ra        = 3.0
>   , dec       = 2.5
>   , totalTime = 12
>   , totalUsed = 9
>   }

> exSession2 = defaultSession {
>     sName     = "test2"
>   , project   = exProject
>   , frequency = 4.2
>   , ra        = 4.0
>   , dec       = 2.0
>   , totalTime = 24
>   , totalUsed = 0
>   }

> exSessions = [exSession, exSession2]

> exPeriods :: [Period]
> exPeriods = [defaultPeriod { session   = exSession2
>                            , startTime = fromGregorian 2008 1 1 0 0 0 
>                            , duration  = 2 }
>            , defaultPeriod { session   = exSession
>                            , startTime = fromGregorian 2008 1 1 5 0 0
>                            , duration  = 4 }
>            , defaultPeriod { session   = exSession
>                            , startTime = fromGregorian 2008 1 2 0 0 0
>                            , duration  = 3 }]

Would like to do this, but with 'time + diffTime'.  How to?
 exPeriods :: IO [Period]
 exPeriods = do
     time <- getCurrentTime
     return [defaultPeriod { session = exSession, startTime = time }
       , defaultPeriod { session = exSession, startTime = time }
       , defaultPeriod { session = exSession, startTime = time }]

dec vs frequency

> sessDecFreq :: [Session] -> [(Float, Float)]
> sessDecFreq = dec `vs` frequency

> vs       :: (a -> b) -> (a -> c) -> [a] -> [(b, c)]
> f `vs` g = map $ \x -> (f x, g x)

> perDecFreq :: [Period] -> [(Float, Float)]
> perDecFreq = promote sessDecFreq

> promote   :: ([Session] -> t) -> [Period] -> t
> promote f = f . map session

dec vs ra

> sessDecRA :: [Session] -> [(Float, Float)]
> sessDecRA = dec `vs` ra

> perDecRA :: [Period] -> [(Float, Float)]
> perDecRA = promote sessDecRA

Example of scatter plot data w/ datetime:

> freqTime :: [Period] -> [(UTCTime, Float)]
> freqTime = startTime `vs` (frequency . session)

Example of log histogram data:
Compare allocated hours by frequency to observed hours by frequency.

> sessFreq :: [Session] -> [(Float, Minutes)]
> sessFreq = histogram [1.0..50.0] . (frequency `vs` totalTime)

> periodFreq :: [Period] -> [(Float, Minutes)]
> periodFreq = histogram [1.0..50.0] . ((frequency . session) `vs` duration)

> sessTP :: [Period] -> [(Minutes, Int)]
> sessTP = count duration [1..17]

> sessDec :: [Float] -> [Session] -> [(Float, Float)]
> sessDec = count dec

> periodDec      :: [Float] -> [Period] -> [(Float, Float)]
> periodDec decs = promote (sessDec decs)

> count           :: (Ord a, Ord b, Num b) => (t -> a) -> [a] -> [t] -> [(a, b)]
> count f buckets = histogram buckets . (f `vs` const 1)

> histogram         :: (Ord a, Ord b, Num b) => [a] -> [(a, b)] -> [(a, b)]
> histogram buckets = allocate buckets . sort
>   where
>     allocate []     _   = []
>     allocate (b:bs) xys = (b, sum . map snd $ within) : allocate bs without
>       where
>         (within, without) = span (\(x, _) -> x <= b) xys
