> module Antioch.Statistics where

> import Antioch.DateTime   (fromGregorian, DateTime, addMinutes', diffMinutes')
> import Antioch.DateTime   (toGregorian')
> import Antioch.Generators
> import Antioch.Types
> import Antioch.Score      (Trace, zenithAngle, minObservingEff)
> import Antioch.Utilities  (rad2hr, rad2deg, utc2lstHours)
> import Antioch.Debug
> import Control.Arrow      ((&&&))
> import Data.Array
> import Data.Function      (on)
> import Data.List
> import Data.Time.Clock
> import Graphics.Gnuplot.Simple
> import System.Random      (getStdGen)
> import Test.QuickCheck    (generate, choose)

To Do List (port from Statistics.py):

   * used in error bars (used in plotObsEffVsFreq and plotMeanObsEffVsFreq)
      Stats done.  Still need to plot.
       * frequency mean
       * obs eff mean and standard deviation
   * historical bad fixed frequency and obs eff true (plotMeanObsEffVsFreq)
   * historical bad window frequency and obs eff true (plotMeanObsEffVsFreq)
   * true historical observing scores
   * historical pressure vs lst
      Need historical pressures
  
> sessionDecFreq :: [Session] -> [(Float, Radians)]
> sessionDecFreq = dec `vs` frequency

> periodDecFreq :: [Period] -> [(Float, Radians)]
> periodDecFreq = promote sessionDecFreq

> sessionDecRA :: [Session] -> [(Radians, Radians)]
> sessionDecRA = dec `vs` ra

> periodDecRA :: [Period] -> [(Radians, Radians)]
> periodDecRA = promote sessionDecRA

> sessionRA :: [Session] -> [(Radians, Float)]
> sessionRA = count (rad2hr . ra) [0..24]

> periodRA :: [Period] -> [(Radians, Float)]
> periodRA = promote sessionRA

> sessionRAHrs :: [Session] -> [(Radians, Float)]
> sessionRAHrs =  histogram [0..24] . (((/60) . fromIntegral . totalTime) `vs` (rad2hr . ra))

> periodRAHrs :: [Period] -> [(Radians, Float)]
> periodRAHrs = histogram [0..24] . (((/60.0) . fromIntegral . duration) `vs` (rad2hr . ra . session))

> sessionDec :: [Session] -> [(Radians, Float)]
> sessionDec = count (rad2deg . dec) [-40..90]

> periodDec :: [Period] -> [(Radians, Float)]
> periodDec = promote sessionDec

> sessionDecHrs :: [Session] -> [(Radians, Float)]
> sessionDecHrs =  histogram [-40..90] . (((/60) . fromIntegral . totalTime) `vs` (rad2deg . dec))

> periodDecHrs :: [Period] -> [(Float, Float)]
> periodDecHrs = histogram [-40..90] . (((/60.0) . fromIntegral . duration) `vs` (rad2deg . dec . session)) 
> sessionFreq :: [Session] -> [(Float, Minutes)]
> sessionFreq = histogram [1.0..50.0] . (totalTime `vs` frequency)

> sessionFreqHrs :: [Session] -> [(Float, Float)]
> sessionFreqHrs ss = histogramToHours $ sessionFreq ss

> periodFreq :: [Period] -> [(Float, Minutes)]
> periodFreq =
>     histogram [1.0..50.0] . (duration `vs` (frequency . session))

> periodFreqHrs :: [Period] -> [(Float, Float)]
> periodFreqHrs ps = histogramToHours $ periodFreq ps

> periodFreqBackupHrs :: [Period] -> [(Float, Float)]
> periodFreqBackupHrs ps = histogramToHours $ periodFreq ps'
>   where
>     ps' = [p | p <- ps, pBackup p]

> periodBackupFreqRatio :: [Period] -> [(Float, Float)]
> periodBackupFreqRatio ps = zipWith backupRatio (periodFreqHrsFunky ps) (periodFreqHrsFunky psBackups)
>   where
>     psBackups =  [p | p <- ps, pBackup p]
>     backupRatio obs backup = (fst obs, ((snd backup) / (snd obs)))

> periodFreqHrsFunky :: [Period] -> [(Float, Float)]
> periodFreqHrsFunky = histogram bins . (((/60) . fromIntegral . duration) `vs` (frequency . session))
>   where
>     bins = [2.0,3.95,5.85,10.0,15.4,20.0,24.0,26.0,30.0,35.0,40.0,45.0,50.0]

> histogramToHours :: [(Float, Minutes)] -> [(Float, Float)]
> histogramToHours =  map (\(f,t) -> (f,(fromIntegral t) / 60))

> sessionTP :: [Period] -> [(Minutes, Int)]
> sessionTP = count ((`div` 60) . duration) [1..7]

> sessionTPQtrs :: [Period] -> [(Minutes, Int)]
> sessionTPQtrs = count (duration) [0, quarter..(7*60)]

Counts how many sessions have a min duration for each quarter hour.
For randomly generated data, this should be a flat distribution.

> sessionMinDurationQtrs :: [Session] -> [(Minutes, Int)]
> sessionMinDurationQtrs = count (minDuration) [0, quarter..(7*60)]

> periodDuration :: [Period] -> [(Minutes, Minutes)]
> periodDuration = histogram [0, quarter..(7*60)] . (duration `vs` duration)

> sessionMinDuration :: [Session] -> [(Minutes, Minutes)]
> sessionMinDuration = histogram [0, quarter..(7*60)] . (minDuration `vs` minDuration)

What is the maximum amount of time that can be scheduled using the min duration.

> sessionMinDurMaxTime :: [Session] -> [(Minutes, Minutes)]
> sessionMinDurMaxTime = histogram [0, quarter..(7*60)] . (maxNumTPTime `vs` minDuration)
>   where
>     maxNumTPTime s = (maxNumTPs s) * (minDuration s)
>     maxNumTPs s = (totalTime s) `div` (minDuration s)


Example of scatter plot data w/ datetime:

> freqTime :: [Period] -> [(DateTime, Float)]
> freqTime = (frequency . session) `vs` startTime

Example of log histogram data:
Compare allocated hours by frequency to observed hours by frequency.

> periodBand :: [Period] -> [(Band, Float)]
> periodBand = histogram [L::Band .. Q::Band] . (((/60.0) . fromIntegral . duration) `vs` (band . session))

> periodEfficiencyByBand :: [Period] -> [Float] -> [(Band, Float)]
> periodEfficiencyByBand ps es = 
>     histogram bands . (effSchdMins `vs` (band . session . fst)) $ zip ps es
>   where 
>     bands = [L::Band .. Q::Band]
>     effSchdMins (p, e) = e * (fromIntegral (duration p) / 60.0)

> decVsElevation :: [Period] -> [Float] -> [(Float, Radians)]
> decVsElevation ps es = (dec . session) `vs` elevationFromZenith $ highEffPeriods
>   where
>     highEffPeriods = [p | (p, e) <- zip ps es, e > 0.85]

> etaFn :: [(Frequency, Float)]
> etaFn = [(f, minObservingEff f) | f <- [2.0 .. 60.0]]

We may want to move this function to a different file.

> elevationFromZenith :: Period -> Float
> elevationFromZenith p =
>     90 - rad2deg (zenithAngle dt (session p))
>   where
>     dt = addMinutes' (duration p `div` 2) $ startTime p

> efficiencyVsFrequency :: [Session] -> [Float] -> [(Float, Float)]
> efficiencyVsFrequency sessions =
>     (snd `vs` (frequency . fst)) . zip sessions

> historicalFreq :: [Period] -> [Float]
> historicalFreq = map (frequency . session)

> historicalDec :: [Period] -> [Radians]
> historicalDec = map (dec . session)

> historicalRA :: [Period] -> [Radians]
> historicalRA = map (ra . session)

> historicalTime :: [Period] -> [DateTime]
> historicalTime = map startTime
>
> historicalTime' :: [Period] -> [Int]
> historicalTime' ps = map ((`div` (24 * 60)) . flip diffMinutes' tzero) times
>   where
>     times = sort $ map startTime ps
>     tzero = head times

> historicalTime'' :: [DateTime] -> [Int]
> historicalTime'' dts = map ((`div` (24 * 60)) . flip diffMinutes' tzero) times
>   where
>     times = sort $ dts 
>     tzero = head times

> historicalLST :: [Period] -> [Float]
> historicalLST ps = [utc2lstHours $ addMinutes' (duration p `div` 2) $ startTime p | p <- ps]

Produces a tuple of (satisfaction ratio, sigma) for each frequency bin scheduled.

> killBad n | isNaN n      = 0.0 -- Is this is right value to return?
>           | isInfinite n = 1.0 -- Is this is right value to return?
>           | otherwise    = n

> satisfactionRatio :: [Session] -> [Period] -> [(Float, Float, Float)]
> satisfactionRatio ss ps = zip3 [frequency $ session p | p <- ps] sRatios sigmas
>   where 
>     pMinutes   = map (fromIntegral . snd) (periodFreq ps) 
>     sMinutes   = map (fromIntegral . snd) (sessionFreq ss)
>     totalRatio = ratio pMinutes sMinutes
>     sRatios    = [killBad (x / y / totalRatio) | (x, y) <- zip pMinutes sMinutes]
>     sigmas     = [killBad (sqrt (x / y)) | (x, y) <- zip sRatios sMinutes]

> totalHrs :: [Session] -> (Session -> Bool) -> Float
> totalHrs ss f = (fromIntegral . sum $ [totalTime s | s <- ss']) / 60.0
>   where
>     ss' = filter f ss

> totalPeriodHrs :: [Period] -> (Period -> Bool) -> Float
> totalPeriodHrs ps f = (fromIntegral . sum $ [duration p | p <- ps']) / 60.0
>   where
>     ps' = filter f ps

> isInSemester :: Session -> String -> Bool
> isInSemester s sem = (semester . project $ s) == sem

> isPeriodInSemester :: Period -> String -> Bool
> isPeriodInSemester p sem = (dt2semester' . startTime $ p) == sem

TBF: code duplication!  where to put this?

> dt2semester' :: DateTime -> String
> dt2semester' dt | month < 2                  = "O5C"
>                 | 2  <= month && month < 6   = "06A"
>                 | 6  <= month && month < 10  = "06B"
>                 | 10 <= month && month <= 12 = "06C"
>   where
>     (_, month, _) = toGregorian' dt

> bandPressuresByTime :: [Trace] -> [[(Float, Float)]]
> bandPressuresByTime trace = --[zip (replicate 3 1.0) (replicate 3 2.0)]
>     map bandData [L::Band .. Q::Band]
>   where
>     bandData band = [(fromIntegral x, y) | (x, y) <- zip days (getBandData band)]
>     fp    = getFreqPressureHistory trace -- [(array (L,W) [(L,9.850087), ..]]
>     times = getTimestampHistory trace
>     days  = historicalTime'' [getTimestamp t | t <- times]
>     getBandData band = getBandPressures band fp
>     

> getBandPressures :: Band -> [Trace] -> [Float]
> getBandPressures band bp = map (getBandPressure band) bp 
>   where
>     getBandPressure band t = (getFreqPressure t)!band
> 

Utilities:

Read Y versus X as you would expect with normal plotting nomenclature.
Produces list of (x, y) coordinate pairs.

> vs       :: (a -> b) -> (a -> c) -> [a] -> [(c, b)]
> y `vs` x = map $ x &&& y

> count :: (Ord a, Ord b, Num b) => (t -> a) -> [a] -> [t] -> [(a, b)]
> count f buckets = histogram buckets . (const 1 `vs` f)

> histogram :: (Ord a, Ord b, Num b) => [a] -> [(a, b)] -> [(a, b)]
> histogram buckets xys = [(x, sum ys) | (x, ys) <- allocate buckets xys]

> allocate buckets = allocate' buckets . sort
          
> allocate'           :: (Ord a, Ord b, Num b) => [a] -> [(a, b)] -> [(a, [b])]
> allocate' []     _   = []
> allocate' (b:bs) xys = (b, map snd within) : allocate' bs without
>   where
>     (within, without) = span (\(x, _) -> x <= b) xys

> meanFreqsByBin       :: [Float] -> [Float]
> meanFreqsByBin freqs = mean frequencyBins [(x, x) | x <- freqs]

> meanObsEffByBin :: [(Float, Float)] -> [Float]
> meanObsEffByBin = mean frequencyBins

> sdomObsEffByBin :: [(Float, Float)] -> [Float]
> sdomObsEffByBin = sdom frequencyBins

> mean, median, stddev, sdom :: [Float] -> [(Float, Float)] -> [Float]
> mean   = simpleStat mean'
> median = simpleStat median'
> stddev = simpleStat stddev'
> sdom   = simpleStat sdom'

> simpleStat f buckets = map (f . snd) . allocate buckets

> mean' xs = sum xs / (fromIntegral . length $ xs)

> median' xs = xs' !! (n `div` 2)
>   where
>     xs' = sort xs
>     n   = length xs

> stddev' xs = sqrt $ sum [(x - m) ^ 2 | x <- xs] / (fromIntegral . length $ xs)
>   where
>     m = mean' xs

> sdom' xs = stddev' xs / (sqrt . fromIntegral . length $ xs)

> ratio :: [Float] -> [Float] -> Float
> ratio = (/) `on` sum

> frequencyBins :: [Float]
> frequencyBins =
>     [0.0, 2.0, 3.95, 5.85, 10.0, 15.4, 20.0, 24.0, 26.0, 30.0, 35.0, 40.0, 45.0, 50.0]

> promote   :: ([Session] -> t) -> [Period] -> t
> promote f = f . map session
