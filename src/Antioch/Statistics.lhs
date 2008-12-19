> module Antioch.Statistics where

> import Antioch.DateTime (fromGregorian)
> import Antioch.Generators
> import Antioch.Types
> import Data.List
> import Data.Time.Clock
> import Graphics.Gnuplot.Simple
> import System.Random (getStdGen)
> import System.IO.Unsafe (unsafePerformIO)
> import Test.QuickCheck (generate)

> exSessions = unsafePerformIO $ do
>     g <- getStdGen
>     return $ generate 0 g $ genSessions' 100

> exPeriods = unsafePerformIO $ do
>     g <- getStdGen
>     return $ generate 0 g $ genPeriods' 100

Would like to do this, but with 'time + diffTime'.  How to?
 exPeriods :: IO [Period]
 exPeriods = do
     time <- getCurrentTime
     return [defaultPeriod { session = exSession, startTime = time }
       , defaultPeriod { session = exSession, startTime = time }
       , defaultPeriod { session = exSession, startTime = time }]

dec vs frequency

> sessDecFreq    :: [Session] -> [(Float, Float)]
> sessDecFreq xs = [(dec x, frequency x) | x <- xs]
> perDecFreq    :: [Period] -> [(Float, Float)]
> perDecFreq xs = [(dec $ session x, frequency $ session x) | x <- xs]

dec vs ra

> sessDecRA    :: [Session] -> [(Float, Float)]
> sessDecRA xs = [(dec x, ra x) | x <- xs]
> perDecRA    :: [Period] -> [(Float, Float)]
> perDecRA xs = [(dec $ session x, ra $ session x) | x <- xs]

       
Example of scatter plot data w/ datetime:

> freqTime :: [Period] -> [(UTCTime, Float)]
> freqTime xs = [ (startTime x, frequency . session $ x) | x <- xs]

Example of log histogram data:
Compare allocated hours by frequency to observed hours by frequency.

> sessFreq    :: [Session] -> [(Float, Minutes)]
> sessFreq xs = histogram [1.0 .. 50.0] [(frequency x, totalTime x) | x <- xs]
> periodFreq    :: [Period] -> [(Float, Minutes)]
> periodFreq xs = histogram [1.0 .. 50.0]
>     [(frequency . session $ x, duration x) | x <- xs]

> sessTP :: [Period] -> [(Minutes, Int)]
> sessTP xs = histogram [1 .. 17] [(duration x, 1) | x <- xs]
> sessDec         :: [Float] -> [Session] -> [(Float, Float)]
> sessDec decs xs = histogram decs [(dec x, 1.0) | x <- xs]

> periodDec         :: [Float] -> [Period] -> [(Float, Float)]
> periodDec decs xs = histogram decs [(dec $ session x, 1.0) | x <- xs]

> histogram :: (Ord a, Ord b, Num b) => [a] -> [(a, b)] -> [(a, b)]
> histogram buckets values = allocate buckets . sort $ values
>   where
>     allocate []     _   = []
>     allocate (b:bs) xys = (b, sum . map snd $ within) : allocate bs without
>       where
>         (within, without) = span (\(x, _) -> x <= b) xys
