> module Antioch.Statistics where

> import Antioch.DateTime   (fromGregorian, DateTime, addMinutes', diffMinutes')
> import Antioch.DateTime   (toGregorian')
> import Antioch.Generators
> import Antioch.Types
> -- import Antioch.Score      (Trace, zenithAngle, minObservingEff, elevationFromZenith)
> import Antioch.Score
> import Antioch.Utilities  (rad2hr, rad2deg, utc2lstHours, dt2semester) 
> import Antioch.Weather
> import Antioch.Debug
> import Control.Arrow      ((&&&), second)
> import Data.Array
> import Data.Fixed         (div')
> import Data.Function      (on)
> import Data.List
> import Data.Time.Clock
> import Data.Maybe         (fromMaybe, isJust)
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
  
> historicalSchdObsEffs ps = historicalSchdFactors ps observingEfficiency
> historicalSchdAtmEffs ps = historicalSchdFactors ps atmosphericOpacity
> historicalSchdTrkEffs ps = historicalSchdFactors ps trackingEfficiency
> historicalSchdSrfEffs ps = historicalSchdFactors ps surfaceObservingEfficiency

> historicalSchdMeanObsEffs ps = historicalSchdMeanFactors ps observingEfficiency
> historicalSchdMeanAtmEffs ps = historicalSchdMeanFactors ps atmosphericOpacity
> historicalSchdMeanTrkEffs ps = historicalSchdMeanFactors ps trackingEfficiency
> historicalSchdMeanSrfEffs ps = historicalSchdMeanFactors ps surfaceObservingEfficiency

> historicalSchdFactors :: [Period] -> ScoreFunc -> IO [Float]
> historicalSchdFactors ps sf = do
>   w <- getWeather Nothing
>   fs <- mapM (periodSchdFactors' w) ps
>   return $ concat fs
>     where
>       periodSchdFactors' w p = periodSchdFactors p sf w

This function can be useful if invalid scores are encountered, and the 
offending period/session/project needs to be revealed.

> historicalSchdFactorsDebug :: [Period] -> ScoreFunc -> IO [(Float,Period)]
> historicalSchdFactorsDebug ps sf = do
>   w <- getWeather Nothing
>   fs <- mapM (periodSchdFactors' w) ps
>   return $ concat $ zipWith (\x y -> map (\y' -> (y', x)) y) ps fs --concat fs
>     where
>       periodSchdFactors' w p = periodSchdFactors p sf w

> historicalSchdMeanFactors :: [Period] -> ScoreFunc -> IO [Float]
> historicalSchdMeanFactors ps sf = do
>   w <- getWeather Nothing
>   fs <- mapM (periodSchdFactors' w) ps
>   return $ map mean' fs
>     where
>       periodSchdFactors' w p = periodSchdFactors p sf w

> periodSchdFactors :: Period -> ScoreFunc -> Weather -> IO [Float]
> periodSchdFactors p sf w = do
>   w' <- newWeather w $ Just $ pForecast p
>   fs <- runScoring w rs $ scorePeriod p sf  
>   return $ map eval fs
>     where
>   rs = [] -- TBF: how to pass this down?

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
> sessionRAHrs =  histogram [0..24] . ((fractionalHours . sAlloted) `vs` (rad2hr . ra))

> periodRAHrs :: [Period] -> [(Radians, Float)]
> periodRAHrs = histogram [0..24] . ((fractionalHours . duration) `vs` (rad2hr . ra . session))

> fractionalHours min = fromIntegral min / 60.0

> sessionDec :: [Session] -> [(Radians, Float)]
> sessionDec = count (rad2deg . dec) [-40..90]

> periodDec :: [Period] -> [(Radians, Float)]
> periodDec = promote sessionDec

> sessionDecHrs :: [Session] -> [(Radians, Float)]
> sessionDecHrs =  histogram [-40..90] . ((fractionalHours . sAlloted) `vs` (rad2deg . dec))

> periodDecHrs :: [Period] -> [(Float, Float)]
> periodDecHrs = histogram [-40..90] . ((fractionalHours . duration) `vs` (rad2deg . dec . session)) 

> sessionFreq :: [Session] -> [(Float, Minutes)]
> sessionFreq = histogram [1.0..50.0] . (sAlloted `vs` frequency)

> sessionFreqHrs :: [Session] -> [(Float, Float)]
> sessionFreqHrs = histogramToHours . sessionFreq

> periodFreq :: [Period] -> [(Float, Minutes)]
> periodFreq =
>     histogram [1.0..50.0] . (duration `vs` (frequency . session))

> periodFreqHrs :: [Period] -> [(Float, Float)]
> periodFreqHrs = histogramToHours . periodFreq

> periodFreqBackupHrs :: [Period] -> [(Float, Float)]
> periodFreqBackupHrs = histogramToHours . periodFreq . filter pBackup

Produces a histogram of the ratio of the canceled to scheduled hours by 
a special frequency bin.  Note that the periods passed in are what
was observed, so the original schedule is the join of the non-backup observed
periods with those that were canceled.

> periodCanceledFreqRatio :: [Period] -> [Trace] ->  [(Float, Float)]
> periodCanceledFreqRatio ps trace = zipWith3 canceledRatio freqBinMidpoints (canceledFreqHrs trace frequencyBins) (scheduledFreqHrs ps trace frequencyBins)
>   where
>     canceledRatio midPoint (_, canceled) (_, 0.0)       = (midPoint, 0.0)
>     canceledRatio midPoint (_, canceled) (_, scheduled) = (midPoint, canceled / scheduled)

> freqBinMidpoints :: [Float]
> freqBinMidpoints = midPoints (0.0 : frequencyBins)

> midPoints    :: [Float] -> [Float]
> midPoints xs = [(x1 + x2) / 2.0 | (x1 : x2 : _) <- tails xs]

> scheduledFreqHrs :: [Period] -> [Trace] -> [Float] -> [(Float, Float)]
> scheduledFreqHrs ps trace bins = histogram bins . ((fractionalHours . duration) `vs` (frequency . session)) $ getScheduledPeriods ps trace 

> getScheduledPeriods :: [Period] -> [Trace] -> [Period]
> getScheduledPeriods observed trace = observed' ++ canceled
>   where
>     canceled  = getCanceledPeriods trace
>     observed' = [p | p <- observed, not . pBackup $ p]

> canceledFreqHrs :: [Trace] -> [Float] -> [(Float, Float)]
> canceledFreqHrs trace bins = histogram bins . ((fractionalHours . duration) `vs` (frequency . session)) . getCanceledPeriods $ trace

> periodBackupFreqRatio :: [Period] -> [(Float, Float)]
> periodBackupFreqRatio ps = zipWith backupRatio (periodFreqHrsBinned ps) (periodFreqHrsBinned psBackups)
>   where
>     psBackups =  [p | p <- ps, pBackup p]
>     backupRatio obs backup = (fst obs, snd backup / snd obs)

> periodFreqHrsBinned :: [Period] -> [(Float, Float)]
> periodFreqHrsBinned = histogram frequencyBins . ((fractionalHours . duration) `vs` (frequency . session))

> histogramToHours :: [(Float, Minutes)] -> [(Float, Float)]
> histogramToHours =  map $ second fractionalHours

> sessionTP    :: [Period] -> [(Float, Int)]
> sessionTP ps = count f d ps
>   where
>     f = fractionalHours . duration
>     d = findDomain 1.0 . map f $ ps

Search the data to find an enumerable range bounding the input given a fixed step size.

> findDomain'      :: Real a => a -> [(a, b)] -> [a]
> findDomain' step = findDomain step . map fst

> findDomain         :: Real a => a -> [a] -> [a]
> findDomain step xs = [step * fromIntegral x | x <- [x1, x1+1 .. x2+1]]
>   where
>     [x1, x2] = map ((`div'` step) . ($ xs)) [minimum, maximum]

> sessionTPQtrs :: [Period] -> [(Minutes, Int)]
> sessionTPQtrs = count (duration) [0, quarter..(13*60)]


Counts how many sessions have a min duration for each quarter hour.
For randomly generated data, this should be a flat distribution.

> sessionMinDurationQtrs :: [Session] -> [(Minutes, Int)]
> sessionMinDurationQtrs = count (minDuration) [0, quarter..(13*60)]

> periodDuration :: [Period] -> [(Minutes, Minutes)]
> periodDuration = histogram [0, quarter..(13*60)] . (duration `vs` duration)


> sessionMinDuration :: [Session] -> [(Minutes, Minutes)]
> sessionMinDuration = histogram [0, quarter..(13*60)] . (minDuration `vs` minDuration)

What is the maximum amount of time that can be scheduled using the min duration.

> sessionMinDurMaxTime :: [Session] -> [(Minutes, Minutes)]
> sessionMinDurMaxTime = histogram [0, quarter..(13*60)] . (maxNumTPTime `vs` minDuration)
>   where
>     maxNumTPTime s = maxNumTPs s * minDuration s
>     maxNumTPs s = sAlloted s `div` minDuration s

Example of scatter plot data w/ datetime:

> freqTime :: [Period] -> [(DateTime, Float)]
> freqTime = (frequency . session) `vs` startTime

Example of log histogram data:
Compare allocated hours by frequency to observed hours by frequency.

> periodBand :: [Period] -> [(Band, Float)]
> periodBand = histogram [L .. Q] . ((fractionalHours . duration) `vs` (band . session))

> sessionBand :: [Session] -> [(Band, Float)]
> sessionBand = histogram [L .. Q] . ((fractionalHours . sAlloted) `vs` band)

> sessionAvBand :: [Session] -> [(Band, Float)]
> sessionAvBand = histogram [L .. Q] . ((fractionalHours . availableTime) `vs` band)

> periodEfficiencyByBand :: [Period] -> [Float] -> [(Band, Float)]
> periodEfficiencyByBand ps es = 
>     histogram bands . (effSchdMins `vs` (band . session . fst)) $ zip ps es
>   where 
>     bands = [L .. Q]
>     effSchdMins (p, e) = e * (fractionalHours . duration $ p)

> decVsElevation :: [Period] -> [(Float, Radians)]
> decVsElevation ps = (dec . session) `vs` elevationFromZenith $ ps 

> etaFn :: [(Frequency, Float)]
> etaFn = [(f, minObservingEff f) | f <- [2.0 .. 60.0]]

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
> historicalTime' ps = map (minutesToDays . flip diffMinutes' tzero) times
>   where
>     times = sort . map startTime $ ps
>     tzero = head times

> minutesToDays  min = min `div` (24 * 60)
> fractionalDays min = fromIntegral min / (24.0 * 60.0)

> historicalExactTime' :: [Period] -> Maybe DateTime -> [Float]
> historicalExactTime' ps start = map (fractionalDays . flip diffMinutes' tzero) times
>   where
>     times = sort . map startTime $ ps
>     tzero = fromMaybe (head times) start

> historicalTime'' :: [DateTime] -> [Int]
> historicalTime'' dts = map (minutesToDays . flip diffMinutes' tzero) times
>   where
>     times = sort dts 
>     tzero = head times

> historicalExactTime'' :: [DateTime] -> Maybe DateTime -> [Float]
> historicalExactTime'' dts start = map (fractionalDays . flip diffMinutes' tzero) times
>   where
>     times = sort dts 
>     tzero = fromMaybe (head times) start

> historicalLST    :: [Period] -> [Float]
> historicalLST ps = [utc2lstHours . addMinutes' (duration p `div` 2) . startTime $ p | p <- ps]

Produces a tuple of (satisfaction ratio, sigma) for each frequency bin scheduled.

> killBad n | isNaN n      = 0.0 -- Is this is right value to return?
>           | isInfinite n = 1.0 -- Is this is right value to return?
>           | otherwise    = n

> satisfactionRatio :: [Session] -> [Period] -> [(Float, Float, Float)]
> satisfactionRatio ss ps = zip3 [1.0..50.0] sRatios sigmas
>   where 
>     pMinutes   = map (fromIntegral . snd) (periodFreq ps) 
>     sMinutes   = map (fromIntegral . snd) (sessionFreq ss)
>     totalRatio = ratio pMinutes sMinutes
>     sRatios    = [killBad (x / y / totalRatio) | (x, y) <- zip pMinutes sMinutes]
>     sigmas     = [killBad (sqrt (x / y)) | (x, y) <- zip sRatios sMinutes]

> totalHrs      :: [Session] -> (Session -> Bool) -> Float
> totalHrs ss f = fractionalHours . sum $ [sAlloted s | s <- ss, f s]

> totalPeriodHrs      :: [Period] -> (Period -> Bool) -> Float
> totalPeriodHrs ps f = fractionalHours . sum $ [duration p | p <- ps, f p]

> isInSemester :: Session -> String -> Bool
> isInSemester s sem = (semester . project $ s) == sem

> isPeriodInSemester :: Period -> String -> Bool
> isPeriodInSemester p sem = (dt2semester . startTime $ p) == sem

> isPeriodFromSemester :: Period -> String -> Bool
> isPeriodFromSemester p sem = (semester . project . session $ p) == sem

> bandPressuresByTime :: [Trace] -> [[(Float, Float)]]
> bandPressuresByTime trace = --[zip (replicate 3 1.0) (replicate 3 2.0)]
>     map bandData [L .. Q]
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
>     getBandPressure band t = getFreqPressure t ! band

> raPressuresByTime :: [Trace] -> [[(Float, Float)]]
> raPressuresByTime trace = 
>     map raData [0 .. 23]
>   where
>     raData ra = [(fromIntegral x, y) | (x, y) <- zip days (getRaData ra)]
>     rap   = getRaPressureHistory trace
>     times = getTimestampHistory trace
>     days  = historicalTime'' [getTimestamp t | t <- times]
>     getRaData ra = getRaPressures ra rap

> getRaPressures    :: Int -> [Trace] -> [Float]
> getRaPressures ra = map $ \t -> getRaPressure t ! ra

The originally scheduled periods can be reconstructed from the observed
periods, and those that were canceled: put every canceled period in its
original slot (this will be overwritting a backup period, or a blank).

> getOriginalSchedule :: [Period] -> [Trace] -> [Period]
> getOriginalSchedule observed trace = sort $ originals observed ++ canceled
>   where 
>     canceled = getCanceledPeriods trace
>     originals ps = [p | p <- ps, not . pBackup $ p]

> getOriginalSchedule' :: [Period] -> [Period] -> [Period]
> getOriginalSchedule' observed canceled = sort $ originals observed ++ canceled
>   where 
>     originals ps = [p | p <- ps, not . pBackup $ p]

> getScheduledDeadTime :: DateTime -> Minutes -> [Period] -> [Trace] -> [(DateTime, Minutes)]
> getScheduledDeadTime start dur observed = findScheduleGaps start dur . getOriginalSchedule observed

> getScheduledDeadTimeHrs :: DateTime -> Minutes -> [Period] -> [Trace] -> Float
> getScheduledDeadTimeHrs start dur obs = fractionalHours . sum . map (\dt -> snd dt) . getScheduledDeadTime start dur obs

> findScheduleGaps :: DateTime -> Minutes -> [Period] -> [(DateTime, Minutes)]
> findScheduleGaps start dur ps = findScheduleGaps' $
>     begin : [(startTime p, duration p) | p <- ps] ++ [end]
>   where
>     begin = (start, 0)
>     end   = (dur `addMinutes'` start, 0)

> findScheduleGaps' ps = [(d1 `addMinutes'` s1, gap) |
>     ((s1,d1), (s2,d2)) <- zip ps (tail ps), gap <- [(s2 `diffMinutes'` s1) - d1], gap > 0]

> getTotalHours :: [Period] -> Float
> getTotalHours = fractionalHours . sum . map duration

> totalSessionHrs :: [Session] -> Float
> totalSessionHrs = fractionalHours . sum . map sAlloted

If you were scheduling with the scheduleMinDuration strategy, how much
time could you really schedule with these sessions?

> totalSessMinDurHrs :: [Session] -> Float
> totalSessMinDurHrs = fractionalHours . sum . map availableTime

> availableTime s
>     | minDuration s == 0 = 0
>     | otherwise          = minDuration s * (sAlloted s `div` minDuration s)

> crossCheckSimulationBreakdown :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> String
> crossCheckSimulationBreakdown simulated scheduled observed canceled obsBackup totalDead schedDead failedBackup =
>     concat warnings ++ "\n"
>   where
>     error = "WARNING: "
>     w1 = if totalDead /= schedDead + failedBackup then error ++ "Total Dead Time != Scheduled Dead Time + Failed Backup Time!" else ""
>     w2 = if observed + totalDead /= simulated then error ++ "Total Simulated Time != Observed + Dead Times!\n" else ""
>     w3 = if scheduled - observed /= canceled - obsBackup then error ++ "Scheduled - Observed Time != Canceled - Observed Backup Times!\n" else ""
>     warnings = [w1, w2, w3]

> breakdownSimulationTimes :: [Session] -> DateTime -> Minutes -> [Period] -> [Period] -> (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)
> breakdownSimulationTimes sessions start dur observed canceled = 
>     ( fractionalHours dur                            -- simHrs
>     , totalSessionHrs sessions                       -- sessHrs
>     , totalSessionHrs . filter backup $ sessions     -- sessBackupHrs
>     , totalSessMinDurHrs sessions                    -- sessAvHrs
>     , totalSessMinDurHrs . filter backup $ sessions  -- sessAvBackupHrs
>     , getTotalHours originalSchedule                 -- scheduledHrs
>     , getTotalHours observed                         -- observedHrs
>     , canceledHrs
>     , obsBackupHrs
>     , fractionalHours . sum $ map snd observedGaps   -- totalObsDeadHrs
>     , fractionalHours . sum $ map snd scheduledGaps  -- totalSchDeadHrs
>     , canceledHrs - obsBackupHrs                     -- failedBackupHrs
>     )
>   where
>     originalSchedule = getOriginalSchedule' observed canceled
>     canceledHrs      = getTotalHours canceled
>     obsBackupHrs     = getTotalHours . filter pBackup $ observed
>     observedGaps     = findScheduleGaps start dur observed
>     scheduledGaps    = findScheduleGaps start dur originalSchedule

> scheduleHonorsFixed :: [Period] -> [Period] -> Bool
> scheduleHonorsFixed [] _ = True
> scheduleHonorsFixed fixed schedule =  dropWhile (==True) (findFixed fixed schedule) == []
>   where
>     findFixed fs schedule = [isJust (find (==f) schedule) | f <- fs]

Read Y versus X as you would expect with normal plotting nomenclature.
Produces list of (x, y) coordinate pairs.

> vs       :: (a -> b) -> (a -> c) -> [a] -> [(c, b)]
> y `vs` x = map $ x &&& y

> count :: (Ord a, Ord b, Num b) => (t -> a) -> [a] -> [t] -> [(a, b)]
> count f buckets = histogram buckets . (const 1 `vs` f)

> histogram :: (Ord a, Ord b, Num b) => [a] -> [(a, b)] -> [(a, b)]
> histogram buckets xys = [(x, sum ys) | (x, ys) <- allocate buckets xys]

> allocate buckets = allocate' buckets . sort
          
> allocate'            :: (Ord a, Ord b, Num b) => [a] -> [(a, b)] -> [(a, [b])]
> allocate' []     _   = []
> allocate' (b:bs) xys = (b, map snd within) : allocate' bs without
>   where
>     (within, without) = span (\(x, _) -> x <= b) xys

> meanFreqsByBin       :: [Float] -> [Float]
> meanFreqsByBin freqs = mean frequencyBins [(x, x) | x <- freqs]

> medianByBin :: [(Float, Float)] -> [Float]
> medianByBin  = median frequencyBins

> meanByBin :: [(Float, Float)] -> [Float]
> meanByBin  = mean frequencyBins

> stddevByBin :: [(Float, Float)] -> [Float]
> stddevByBin  = stddev frequencyBins

> sdomByBin :: [(Float, Float)] -> [Float]
> sdomByBin  = sdom frequencyBins


> mean, median, stddev, sdom   :: [Float] -> [(Float, Float)] -> [Float]
> [mean, median, stddev, sdom] = map simpleStat [mean', median', stddev', sdom']

simpleStat provides a way to perform statistics on binned data

> simpleStat f buckets = map (f . snd) . allocate buckets

histStat provides a way to perform statistics on historgram data
f is a function like mean', etc.

> histStat :: ([Float] -> Float) -> [(Float, Float)] -> Float
> histStat f histData = f newData
>   where
>     newData = concat $ map (\x -> replicate (round . snd $ x) (fst x)) histData

> mean' xs = sum xs / (fromIntegral . length $ xs)

> median' xs = sort xs !! (length xs `div` 2)

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
