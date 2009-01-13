> module Antioch.Statistics where

> import Antioch.DateTime (fromGregorian)
> import Antioch.Generators
> import Antioch.Types
> import Antioch.Utilities (rad2hr, rad2deg)
> import Data.Function (on)
> import Data.List
> import Data.Time.Clock
> import Graphics.Gnuplot.Simple
> import System.Random (getStdGen)
> import Test.QuickCheck (generate)

> exSessions = do
>     g <- getStdGen
>     return . generate 0 g . genSessions $ 100

> exPeriods = do
>     g <- getStdGen
>     return . generate 0 g . genPeriods $ 100

> sessFreqDec :: [Session] -> [(Float, Float)]
> sessFreqDec = frequency `vs` dec

Read Y versus X as you would expect with normal plotting nomenclature.
Produces list of (x, y) coordinate pairs.

> vs       :: (a -> b) -> (a -> c) -> [a] -> [(c, b)]
> y `vs` x = map $ \a -> (x a, y a)

> perDecFreq :: [Period] -> [(Float, Float)]
> perDecFreq = promote sessFreqDec

> promote   :: ([Session] -> t) -> [Period] -> t
> promote f = f . map session

> sessRADec :: [Session] -> [(Float, Float)]
> sessRADec= ra `vs` dec

> perRADec :: [Period] -> [(Float, Float)]
> perRADec = promote sessRADec

Example of scatter plot data w/ datetime:

> freqTime :: [Period] -> [(UTCTime, Float)]
> freqTime = (frequency . session) `vs` startTime

Example of log histogram data:
Compare allocated hours by frequency to observed hours by frequency.

> perBand :: [Period] -> [(Band, Minutes)]
> perBand = histogram [L::Band .. Q::Band] . (duration `vs` (band . session))

> perEfficiencyByBand :: [Period] -> [Float] -> [(Band, Float)]
> perEfficiencyByBand ps es = 
>     histogram bands . (snd `vs` (band . session . fst)) $ zip ps es
>   where bands = [L::Band .. Q::Band]

> decVsElevation :: [Period] -> [Float] -> [(Float, Float)]
> decVsElevation ps es = dec `vs` zenith $ highEffPeriods
>   where
>     zenith = map $
>         \Period { startTime = st, duration = dur } -> 90.0 - (st + dur / 2.0)
>     highEffPeriods = fst . (filter ((> 0.85) . snd)) $ zip ps es

> efficiencyVsFrequency :: [Session] -> [Float] -> [(Float, Float)]
> efficiencyVsFrequency sessions efficiencies =
>     snd `vs` (frequency . fst) $ zip sessions efficiencies

> frequencyBins =
>     [0.0, 2.0, 3.95, 5.85, 10.0, 15.4, 20.0, 24.0, 26.0, 30.0, 35.0, 40.0, 45.0, 50.0]

> meanFreqsByBin       :: [Float] -> [Float]
> meanFreqsByBin freqs = mean frequencyBins $ [(x, x) | x <- freqs]

> meanObsEffByBin :: [(Float, Float)] -> [Float]
> meanObsEffByBin = mean frequencyBins

> mean buckets xys = zipWith (/) `on` (map snd) $ totals counts
>   where
>     totals = histogram buckets xys
>     counts = histogram buckets [(x, 1) | x <- xys]

> historicalFreq :: [Period] -> [Float]
> historicalFreq = map (frequency . session)

> sessFreq :: [Session] -> [(Minutes, Float)]
> sessFreq = histogram [1.0..50.0] . (frequency `vs` totalTime)

> periodFreq :: [Period] -> [(Minutes, Float)]
> periodFreq = histogram [1.0..50.0] . ((frequency . session) `vs` duration)

Produces a tuple of (satisfaction ratio, sigma) for each frequency bin scheduled.

> satisfactionRatio :: [Session] -> [Period] -> [(Float, Float)]
> satisfactionRatio ss ps = zip sRatios sigmas
>   where 
>     pMinutes   = map snd (periodFreq ps) 
>     sMinutes   = map snd (sessFreq ss)
>     totalRatio = ratio pMinutes sMinutes
>     sRatios    = [x / y / totalRatio | (x, y) <- zip pMinutes sMinutes]
>     sigmas     = [x / y ^ (0.5) | (x, y) <- zip sRatios sMinutes]

> ratio :: [(Minutes, Float)] -> [(Minutes, Float)] -> Float
> ratio = (/) `on` (sum . map snd) 

> sessTP :: [Period] -> [(Minutes, Int)]
> sessTP = count ((`div` 60) . duration) [1..7]

> sessRA :: [Session] -> [(Float, Float)]
> sessRA = count (rad2hr . ra) [0..24]

> sessDec :: [Session] -> [(Float, Float)]
> sessDec = count (rad2deg . dec) [-40..90]

> periodDec :: [Period] -> [(Float, Float)]
> periodDec = promote sessDec

> count           :: (Ord a, Ord b, Num b) => (t -> a) -> [a] -> [t] -> [(a, b)]
> count f buckets = histogram buckets . (f `vs` const 1)

> histogram         :: (Ord a, Ord b, Num b) => [a] -> [(a, b)] -> [(a, b)]
> histogram buckets = allocate buckets . sort
>   where
>     allocate []     _   = []
>     allocate (b:bs) xys = (b, sum . map snd $ within) : allocate bs without
>       where
>         (within, without) = span (\(x, _) -> x <= b) xys
