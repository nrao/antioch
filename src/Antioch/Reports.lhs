> module Antioch.Reports where

> import Antioch.DateTime
> import Antioch.Generators (internalConflicts, endTime, genProjects, genSessions, genPeriods, generateVec)
> import Antioch.Plots
> import Antioch.Score
> import Antioch.Schedule
> import Antioch.Simulate
> import Antioch.Statistics
> import Antioch.Types
> import Antioch.Utilities (rad2deg, rad2hrs, printList)
> import Antioch.Weather
> import Antioch.Debug
> import Antioch.TimeAccounting
> --import Antioch.HardwareSchedule
> --import Antioch.DSSData
> --import Antioch.Settings (dssDataDB)
> import Control.Monad      (liftM)
> import Control.Monad.Trans (liftIO)
> import Data.List (intercalate, sort, (\\), nub)
> import Text.Printf
> import System.Random
> import System.CPUTime
> import Test.QuickCheck hiding (promote, frequency)
> import Graphics.Gnuplot.Simple

simRemainingTime

Here we are trying to reproduce subcompenents of the pressure calculations

> plotRemainingTimeByBand              :: StatsPlot
> plotRemainingTimeByBand fn n ss ps tr = if (length ps == 0) then print "no periods for plotRemainingTimeByBand" else plotRemainingTimeByBand' fn n ss ps tr

> plotRemainingTimeByBand'              :: StatsPlot
> plotRemainingTimeByBand' fn n ss' ps _ = do
>   let bandFracs = map (\ss -> remainingTimeByDays ss start days) ssBands
>   let plots = zip titles bandFracs 
>   linePlots (scatterAttrs title xl yl fn) $ plots 
>     where
>   title = "Remaining Time By Band" ++ n
>   xl = "Time [Days]"
>   yl = "Remaining Time Used In Pressures"
>   start = fst $ getPeriodRange ps
>   days = snd $ getPeriodRange ps
>   ss = updateSessions ss' ps []
>   ssBands = sessionsByBand ss
>   titles = map (\b -> (Just (show b))) [L .. Q]

simPastSemesterTime

> plotPastSemesterTimeByBand              :: StatsPlot
> plotPastSemesterTimeByBand fn n ss ps tr = if (length ps == 0) then print "no periods for plotPastSemesterTimeByBand" else plotPastSemesterTimeByBand' fn n ss ps tr

> plotPastSemesterTimeByBand'              :: StatsPlot
> plotPastSemesterTimeByBand' fn n ss' ps _ = do
>   let bandFracs = map (\ss -> pastSemesterTimeByDays ss start days) ssBands
>   let plots = zip titles bandFracs 
>   linePlots (scatterAttrs title xl yl fn) $ plots 
>     where
>   title = "Past Semester Time By Band" ++ n
>   xl = "Time [Days]"
>   yl = "Past Semester Time Used In Pressures"
>   start = fst $ getPeriodRange ps
>   days = snd $ getPeriodRange ps
>   ss = updateSessions ss' ps []
>   ssBands = sessionsByBand ss
>   titles = map (\b -> (Just (show b))) [L .. Q]

> sessionsByBand :: [Session] -> [[Session]]
> sessionsByBand ss = map (ssBand ss) [L .. Q]
>   where
>     isBand bandName s = band s == bandName
>     ssBand sess bandName = filter (isBand bandName) sess

> periodsByBand :: [Period] -> [[Period]]
> periodsByBand ps = map (psBand ps) [L .. Q]
>   where
>     isPBand bandName p = (band . session $ p) == bandName
>     psBand periods bandName = filter (isPBand bandName) periods 

simFracTime 

> plotFractionalTime              :: StatsPlot
> plotFractionalTime fn n ss ps tr = if (length ps == 0) then print "no periods for plotFractionalTime" else plotFractionalTime' fn n ss ps tr

> plotFractionalTime'              :: StatsPlot
> plotFractionalTime' fn n ss ps _ = do
>   let total = fracObservedTimeByDays ss ps 
>   let gradeA = fracObservedTimeByDays ssA psA
>   let gradeB = fracObservedTimeByDays ssB psB
>   let bandK = fracObservedTimeByDays ssK psK
>   linePlots (tail $ scatterAttrs title xl yl fn) [(Just "Total", total), (Just "Grade A", gradeA), (Just "Grade B", gradeB), (Just "K Band", bandK)]
>     where
>   title = "Fractional Observed Time " ++ n
>   xl = "Time [Days]"
>   yl = "Time Observed / Time Allocated"
>   ssA = filter isGradeA ss -- grade A sessions
>   psA = filter (isGradeA . session) ps -- grade A periods
>   ssB = filter isGradeB ss -- grade B sessions
>   psB = filter (isGradeB . session) ps -- grade B periods
>   ssK = filter (\s -> (band s) == K) ss -- grade B sessions
>   psK = filter (\p -> (band . session $ p) == K) ps -- K Band periods
>   isGradeA s = grade s >= 4
>   isGradeB s = grade s < 4 && grade s >= 3

simFracBandTime

> plotFracBandTime              :: StatsPlot
> plotFracBandTime fn n ss ps tr = if (length ps == 0) then print "no periods for plotFracBandTime" else plotFracBandTime' fn n ss ps tr

> plotFracBandTime'              :: StatsPlot
> plotFracBandTime' fn n ss ps _ = do
>   --let total = fracObservedTimeByDays ss ps 
>   let bandFracs = map (\d -> fracObservedTimeByDays' (fst d) (snd d) start days) $ zip ssBands psBands
>   let plots = zip titles bandFracs 
>   linePlots (tail $ scatterAttrs title xl yl fn) $ plots 
>     where
>   title = "Fractional Observed Time By Band" ++ n
>   xl = "Time [Days]"
>   yl = "Time Observed / Time Allocated"
>   start = fst $ getPeriodRange ps
>   days = snd $ getPeriodRange ps
>   ssBands = sessionsByBand ss 
>   psBands = periodsByBand ps 
>   titles = map (\b -> (Just (show b))) [L .. Q]

simFracSemesterTime

> plotFracSemesterTime              :: StatsPlot
> plotFracSemesterTime fn n ss ps tr = if (length ps == 0) then print "no periods for plotFracSemesterTime" else plotFracSemesterTime' fn n ss ps tr

> plotFracSemesterTime'              :: StatsPlot
> plotFracSemesterTime' fn n ss ps _ = do
>   let total = fracObservedTimeByDays ss ps 
>   let bandFracs = map (\d -> fracObservedTimeByDays' (fst d) (snd d) start days) $ zip ssSemesters psSemesters
>   let plots = zip titles bandFracs 
>   linePlots (tail $ scatterAttrs title xl yl fn) $ [(Just "Total", total)] ++ plots 
>     where
>   title = "Fractional Observed Time By Semester" ++ n
>   xl = "Time [Days]"
>   yl = "Time Observed / Time Allocated"
>   start = fst $ getPeriodRange ps
>   days = snd $ getPeriodRange ps
>   semesters = nub . sort $ map (semester . project) ss--["05C","06A", "06B", "06C"]
>   isSemester sem s = (semester . project $ s) == sem
>   ssSemester ss sem = filter (isSemester sem) ss 
>   ssSemesters = map (ssSemester ss) semesters --["05C","06A", "06B", "06C"]
>   isPSemester sem p = (semester . project . session $ p) == sem
>   psSemester ps sem = filter (isPSemester sem) ps 
>   psSemesters = map (psSemester ps) semesters
>   titles = map (\b -> (Just (show b))) semesters

This function produces a graph of the wind values taken directly from the
CLEO forecasts: the wind speed in mph.  This graph can then be compared to
the graph produced by CLEO forecasts, requesting 'Ground Speed', just sites
Elkins and Lewisburg, with the average.  An excellent tool for integration
tests.

> plotWindMPH :: DateTime -> Minutes -> IO ()
> plotWindMPH dt dur = do
>   w <- getWeather $ Just dt
>   winds_mph <- mapM (getWindsMPH' w) $ times
>   let plotData = zipWith (\a b -> (fromInteger . toInteger $ a, (maybe 0.0 id b))) deltas winds_mph
>   linePlots (tail $ scatterAttrs title xl yl fn) [(Just "wind_mph", plotData)]
>     where
>   title = "wind_mph, starting: " ++ (toSqlString dt)
>   xl = "delta (hrs)"
>   yl = "wind (mph)"
>   fn = "wind_mph.png"
>   hours = dur `div` 60 
>   times = [(i*60) `addMinutes'` dt | i <- [0 .. hours]]
>   deltas = [0 .. hours]
>   getWindsMPH' w dt = wind_mph w dt 

stringency

Stringency vs. Frequency @ 90 degress elevation

> plotStringency :: StatsPlot
> plotStringency fn n _ _ _ = do
>   w <- getWeather Nothing
>   str <- mapM (stringency w) freqs
>   let plotData = zipWith (\a b -> (a, (maybe 0.0 id b))) freqs str
>   linePlots (tail $ scatterAttrs title xl yl fn) [(Just "stringency", plotData)]
>     where
>   freqs = [2 .. 50]
>   stringency w f = totalStringency w f (pi/2)
>   title = "Stringency vs. Frequency (@90') " ++ n
>   xl = "Freq. (GHz)"
>   yl = "Stringency"

minEffSysTemp

minEffSysTemp vs. Frequency @ 90 degress elevation

> plotMinEffSysTemp :: StatsPlot
> plotMinEffSysTemp fn n _ _ _ = do
>   w <- getWeather Nothing
>   str <- mapM (mtsys w) freqs
>   let plotData = zipWith (\a b -> (a, (maybe 0.0 id b))) freqs str
>   linePlots (tail $ scatterAttrs title xl yl fn) [(Just "min eff sys temp", plotData)]
>     where
>   freqs = [2 .. 50]
>   mtsys w f = minTSysPrime w f (pi/2)
>   title = "Min. Effective Sys. Temp. vs. Frequency (@90') " ++ n
>   xl = "Freq. (GHz)"
>   yl = "Min. Effecitve System Temperature"

simDecFreq (stars, crosses)

> plotDecFreq          :: StatsPlot
> plotDecFreq fn n ss ps _ =
>      scatterPlots attrs $ zip titles $ [[(x, rad2deg y) | (x, y) <- sessionDecFreq ss]
>                                            , [(x, rad2deg y) | (x, y) <-  periodDecFreq ps]]
>   where
>     t   = "Dec vs Freq" ++ n
>     x   = "Frequency [GHz]"
>     y   = "Declination [deg]"
>     titles = [Just "Available", Just "Observed"]
>     attrs = (tail $ scatterAttrs t x y fn) ++ [XRange (0, 51), YRange (-40, 95)]

simDecRA (stars, crosses)

> plotDecVsRA          :: StatsPlot
> plotDecVsRA fn n ss ps _ =
>     scatterPlots attrs $ zip titles $ [[(rad2hrs x, rad2deg y) | (x, y) <- sessionDecRA ss]
>                                           , [(rad2hrs x, rad2deg y) | (x, y) <-  periodDecRA ps]]
>   where
>     t = "Dec vs RA" ++ n
>     x = "Right Ascension [hr]"
>     y = "Declination [deg]"
>     titles = [Just "Available", Just "Observed"]
>     attrs = (tail $ scatterAttrs t x y fn) ++ [XRange (-1, 25), YRange (-40, 95)]

Efficiency Plots:

simMinObsEff - simply the min. observing curve.  Note: observing efficiencies
that are below this line experience and exponential cutoff (i.e., not simply zero).
simEffFreq - simply the observed observing efficiency at the start of the period
simSchd* - these are plots that represent the efficiency at the time the
period was scheduled (*not*) the time it was observed.
simSchdMean* - these are the mean efficiencies for the periods when they were scheduled.  TBF: note that this is *not* a reflection of the scoring used when
the period was scheduled, because the Pack algorithm ignores the first quarter.
   * simSchdMeanEffFreq - Observing Efficiency
   * simSchdMeanAtmFreq - Atmospheric Efficiency
   * simSchdMeanSrfFreq - Surface Efficiency
   * simSchdMeanTrkFreq - Tracking Efficiency
simMeanObsEff - this plot shows the error bars for simSchdMeanEffFreq   

simSchdMeanEffFreq
error bars done separately in simMeanObsEff

> plotMeanObsEffVsFreq  :: StatsPlot
> plotMeanObsEffVsFreq fn n _ ps _ = do
>   effs <- historicalSchdMeanObsEffs ps
>   let t = "Scheduled Mean Observing Efficiency vs Frequency" ++ n
>   let y = "Mean Observing Efficiency"
>   plotEffVsFreq'' fn effs ps t y

simSchdMeanAtmFreq
Break down the above plot into the three factors that make up observing eff.

> plotMeanAtmEffVsFreq  :: StatsPlot
> plotMeanAtmEffVsFreq fn n _ ps _ = do
>   effs <- historicalSchdMeanAtmEffs ps
>   let t = "Scheduled Mean Atmospheric Efficiency vs Frequency" ++ n
>   let y = "Mean Atmospheric Efficiency"
>   plotEffVsFreq'' fn effs ps t y

simSchdMeanTrkFreq

> plotMeanTrkEffVsFreq  :: StatsPlot
> plotMeanTrkEffVsFreq fn n _ ps _ = do
>   effs <- historicalSchdMeanTrkEffs ps
>   let t = "Scheduled Mean Tracking Efficiency vs Frequency" ++ n
>   let y = "Mean Tracking Efficiency"
>   plotEffVsFreq'' fn effs ps t y

simSchdMeanSrfFreq

> plotMeanSrfEffVsFreq  :: StatsPlot
> plotMeanSrfEffVsFreq fn n _ ps _ = do
>   effs <- historicalSchdMeanSrfEffs ps
>   let t = "Scheduled Mean Surface Obs. Efficiency vs Frequency" ++ n
>   let y = "Mean Surface Obs. Efficiency"
>   plotEffVsFreq'' fn effs ps t y

simEffFreq (error bars, crosses, line plot) - Need stats from Dana
This plot is observing efficiency vs. frequency, where the obs. eff. is:
   * calculated at the time of the start of the Period
   * just for that one inital quarter (as oppsed to averaged over duration)
   * uses WRONG weather 

> plotEffVsFreq'         :: StatsPlot
> plotEffVsFreq' fn n _ ps _ = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   let t = "Observed Observing Efficiency (at start) vs Frequency" ++ n
>   let y = "Observing Efficiency"
>   plotEffVsFreq'' fn effs ps t y

General purpose function for scatter plots of some kind of efficiency vs. freq

> plotEffVsFreq'' fn effs ps t y =
>     scatterPlot attrs $ zip (historicalFreq ps) effs
>   where
>     x     = "Frequency [GHz]"
>     attrs = (tail $ scatterAttrs t x y fn) ++ [XRange (0, 51), YRange (-0.1, 1.1)]


simMeanEffVsFreq - errorbar plot of efficiencies (stand alone plot for now)

> plotEffVsFreqBin  :: StatsPlot
> plotEffVsFreqBin fn n _ ps _ = do
>     effs <- historicalSchdMeanObsEffs ps
>     plotEffVsFreq fn n effs ps


> plotEffVsFreq fn n effs ps = 
>     errorBarPlot attrs $ zip3 meanFreq meanEffFreq sdomEffFreq
>   where
>     meanFreq = meanFreqsByBin $ (map (frequency . session) ps)
>     meanEffFreq = meanByBin $ zip (map (frequency . session) ps) effs
>     sdomEffFreq = sdomByBin $ zip (map (frequency . session) ps) effs
>     t = "Observing Efficiency vs Frequency" ++ n
>     x = "Frequency [GHz]"
>     y = "Observing Efficiency"
>     attrs = (tail $ scatterAttrs t x y fn) ++ [XRange (0, 51), YRange (-0.1, 1.1)]

simTPFreq

> plotTPDurVsFreqBin  :: StatsPlot
> plotTPDurVsFreqBin fn n _ ps _ = do
>     let durs = map (fromIntegral . duration) ps
>     plotTPDurVsFreq fn n durs ps


> plotTPDurVsFreq fn n durs ps = 
>     errorBarPlot attrs $ zip3 meanFreq meanDurFreq stddevDurFreq
>   where
>     meanFreq = meanFreqsByBin $ (map (frequency . session) ps)
>     meanDurFreq = meanByBin $ zip (map (frequency . session) ps) durs
>     stddevDurFreq = stddevByBin $ zip (map (frequency . session) ps) durs
>     t = "Telescope Period Length vs Frequency" ++ n
>     x = "Frequency [GHz]"
>     y = "Telescope Period Length [Min]"
>     attrs = (tail $ scatterAttrs t x y fn) ++ [XRange (0, 51)] --, YRange (-0.1, 1.1)]

simMinObsEff - minimum observing efficiency (stand alone plot for now)

> plotMinObsEff          :: StatsPlot
> plotMinObsEff fn n _ _ _ = plotFunc attrs (linearScale 1000 (0, 50)) minObservingEff
>   where
>     t     = "Observing Efficiency vs Frequency" ++ n
>     x     = "Frequency [GHz]"
>     y     = "Observing Efficiency"
>     attrs = (tail $ scatterAttrs t x y fn) ++ [XRange (0, 51), YRange (-0.1, 1.1)]


simTPVsFreq - this does not yet work


> {-
> plotTPVsFreq           :: StatsPlot    
> plotTPVsFreq fn _ ps =
>     errorBarPlot attrs $ zip3 meanFreq meanTPFreq stddevTPFreq
>   where
>     meanFreq = meanFreqsByin $ (map (frequency . session) ps) 
>     meanTPFreq = meanByBin $ zip (map (frequency . session) ps) [duration p | p <- ps]
>     stddevTPFreq = stddevByBin $ zip (map (frequency . session) ps) [duration p | p <- ps]
>     t = "Telescope Period Length vs Frequency"
>     x = "Frequency [GHz]"
>     y = "Telescope Period Length [min]"
>     attrs = (tail $ scatterAttrs t x y fn)
> -}


simFreqTime (circles, dt on x-axis)

> plotFreqVsTime         :: StatsPlot
> plotFreqVsTime fn n _ ps _ =
>     scatterPlot attrs $ zip (map fromIntegral $ historicalTime' ps) (historicalFreq ps)
>   where
>     t = "Frequency vs Time" ++ n
>     x = "Time [days]"
>     y = "Frequency [GHz]"
>     attrs = (tail $ scatterAttrs t x y fn) ++ [YRange (0, 51)]

Same as above, but with scheduled periods, plus with backups & cancellations
simFreqSchTime (circles, dt on x-axis)

> plotSchdFreqVsTime fn n _ ps trace = if (length ps == 0) then print "no periods for plotSchdFreqVsTime" else scatterPlots attrs $ zip titles $ [pl1, pl2, pl3, pl4]
>     where
>       t = "Frequency vs Start Time" ++ n
>       x = "Time [fractional days]"
>       y = "Frequency [GHz]"
>       titles = [Just "Scheduled & Observed"
>               , Just "Canceled"
>               , Just "Backup"
>               , Just "Scheduled Deadtime"]
>       attrs = (tail $ scatterAttrs t x y fn) ++ [YRange (0, 51)]
>       ps' = [p | p <- ps, not . pBackup $ p]
>       backups = [p | p <- ps, pBackup p]
>       canceled = getCanceledPeriods trace
>       start = startTime . head $ ps
>       lastPs = last ps
>       end   = (duration lastPs) `addMinutes'` (startTime lastPs)
>       deadtime = getScheduledDeadTime start (end `diffMinutes'` start) ps trace 
>       pl1 = zip (historicalExactTime' ps' Nothing) (historicalFreq ps')
>       pl2 = zip (historicalExactTime' canceled (Just start)) (historicalFreq canceled)
>       pl3 = zip (historicalExactTime' backups (Just start)) (historicalFreq backups)
>       pl4 = zip (historicalExactTime'' (map fst deadtime) (Just start)) (replicate (length deadtime) 0.0)


simSatisfyFreq (error bars)

> plotSatRatioVsFreq          :: StatsPlot
> plotSatRatioVsFreq fn n ss ps _ =
>     errorBarPlot attrs $ satisfactionRatio ss ps
>   where
>     t = "Satisfaction Ratio vs Frequency" ++ n
>     x = "Frequency [GHz]"
>     y = "Satisfaction Ratio"
>     attrs = (tail $ scatterAttrs t x y fn) ++ [XRange (0, 51)]

simEffElev

> plotEffElev'          :: StatsPlot
> plotEffElev' fn n _ ps _ = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffElev fn n effs ps

> plotEffElev fn n effs ps = scatterPlot attrs $ zip (map elevationFromZenith ps) effs
>   where
>     t = "Observing Efficiency vs Elevation" ++ n
>     x = "Elevation [deg]"
>     y = "Observing Efficiency"
>     attrs = (tail $ scatterAttrs t x y fn) ++ [YRange (-0.1, 1.1)]



simEffLST

> plotEffLst'           :: StatsPlot
> plotEffLst' fn n _ ps _ = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffLst fn n effs ps

> plotEffLst fn n effs ps =
>     scatterPlot attrs $ zip (historicalLST ps) effs
>   where
>     t = "Observing Efficiency vs LST" ++ n
>     x = "LST [hours]"
>     y = "Observing Efficiency"
>     attrs = (tail $ scatterAttrs t x y fn) ++ [YRange (-0.1, 1.1)]

simElevDec

> plotElevDec           :: StatsPlot
> plotElevDec fn n _ ps _ = do
>     scatterPlot attrs $ [(x, rad2deg y) | (x, y) <- decVsElevation ps]
>   where
>     t = "Dec vs Elevation" ++ n
>     x = "Elevation [deg]"
>     y = "Declination [deg]"
>     attrs = (tail $ scatterAttrs t x y fn) ++ [YRange (-40, 95)]

simPFLST - need pressure history

simScoreElev


> plotScoreElev'           :: StatsPlot
> plotScoreElev' fn n _ ps _ = do
>   -- TBF: historicalObsScore not working, so make this a simpler plot for now
>   --w       <- getWeather Nothing
>   --scores  <- historicalObsScore w ps
>   let scores = map pScore ps
>   plotScoreElev fn n scores ps

> plotScoreElev fn n scores ps =
>     scatterPlot (tail $ scatterAttrs t x y fn) $ zip (map elevationFromZenith ps) scores
>   where
>     t = "Score vs Elevation" ++ n
>     x = "Elevation [deg]"
>     y = "Score"

simScoreLST

> plotLstScore'           :: StatsPlot
> plotLstScore' fn n _ ps _ = do
>   -- TBF: historicalObsScore not working, so make this a simpler plot for now
>   --w       <- getWeather Nothing
>   --scores  <- historicalObsScore w ps
>   let scores = map pScore ps
>   plotLstScore fn n scores ps
>
> plotLstScore fn n scores ps =
>     scatterPlot (tail $ scatterAttrs t x y fn) $ zip (historicalLST ps) scores
>   where
>     t = "Score vs LST" ++ n
>     x = "LST [hours]"
>     y = "Score"


simScoreFreq

> plotScoreFreq           :: StatsPlot
> plotScoreFreq fn n _ ps _ = do
>     scatterPlot attrs $ zip (historicalFreq ps) (map pScore ps)
>   where
>     t = "Score vs Frequency" ++ n
>     x = "Frequency [GHz]"
>     y = "Score"
>     attrs = (scatterAttrs t x y fn) ++ [XRange (0, 51)]



simBandPFTime

> plotBandPressureTime              :: StatsPlot
> plotBandPressureTime fn n _ _ trace = 
>     linePlots (scatterAttrs t x y fn) $ zip titles $ bandPressuresByTime trace 
>   where
>     t = "Band Pressure Factor vs Time" ++ n
>     x = "Time [days]"
>     y = "Band Pressure Factor"
>     titles = [Just "L", Just "S", Just "C", Just "X", Just "U", Just "K", Just "A", Just "Q"]
> 

simBandPBinPastTime


> plotBandPressureBinPastTime              :: StatsPlot
> plotBandPressureBinPastTime fn n _ _ trace = do 
>     let bins = bandPressureBinsByTime trace
>     let past = binsToPlotData bins snd
>     linePlots (scatterAttrs t x y fn) $ zip titles $ past 
>   where
>     t = "Band Pressure Past Bin vs Time" ++ n
>     x = "Time [days]"
>     y = "Band Pressure Past Bin"
>     titles = [Just "L", Just "S", Just "C", Just "X", Just "U", Just "K", Just "A", Just "Q"]

> plotBandPressureBinRemainingTime              :: StatsPlot
> plotBandPressureBinRemainingTime fn n _ _ trace = do 
>     let bins = bandPressureBinsByTime trace
>     let past = binsToPlotData bins fst
>     linePlots (scatterAttrs t x y fn) $ zip titles $ past 
>   where
>     t = "Band Pressure Remainging Bin vs Time" ++ n
>     x = "Time [days]"
>     y = "Band Pressure Remainging Bin"
>     titles = [Just "L", Just "S", Just "C", Just "X", Just "U", Just "K", Just "A", Just "Q"]

> binsToPlotData :: [[(Float, (Int, Int))]] -> ((Int,Int) -> Int) -> [[(Float, Float)]]
> binsToPlotData bins f = map (g f) bins
>   where
>     g f bin = map (h f) bin
>     h f b = (fst b, (fractionalHours . f . snd $ b))

simLSTPFTime1

> plotRAPressureTime1              :: StatsPlot
> plotRAPressureTime1 fn n _ _ trace =
>     linePlots (scatterAttrs t x y fn) $ take 8 $ zip titles $ raPressuresByTime trace 
>   where
>     t = "LST Pressure Factor vs Time" ++ n
>     x = "Time [days]"
>     y = "LST Pressure Factor"
>     titles = [Just (show a) | a <- [0 .. 7]]

simLSTPFTime2 - need pressure history

> plotRAPressureTime2              :: StatsPlot
> plotRAPressureTime2 fn n _ _ trace =
>     linePlots (scatterAttrs t x y fn) $ zip titles $ radata
>   where
>     (_, radata) = splitAt 8 $ raPressuresByTime trace
>     t = "LST Pressure Factor vs Time" ++ n
>     x = "Time [days]"
>     y = "LST Pressure Factor"
>     titles = [Just (show a) | a <- [8 .. 15]]

simLSTPFTime3 - need pressure history

> plotRAPressureTime3              :: StatsPlot
> plotRAPressureTime3 fn n _ _ trace =
>     linePlots (scatterAttrs t x y fn) $ zip titles $ radata
>   where
>     (_, radata) = splitAt 16 $ raPressuresByTime trace 
>     t = "LST Pressure Factor vs Time" ++ n
>     x = "Time [days]"
>     y = "LST Pressure Factor"
>     titles = [Just (show a) | a <- [16 .. 23]]

simHistRA

> histSessRA          :: StatsPlot
> histSessRA fn n ss ps _ =
>     histogramPlots attrs $ zip titles [sessionRAHrs ss, periodRAHrs ps]
>   where
>     t = "Right Ascension Histogram" ++ n
>     x = "RA [hr]"
>     y = "Counts [Hours]"
>     titles = [Just "Available", Just "Observed"]
>     attrs = (histAttrs t x y fn) ++ [XRange (-1, 25)]

simHistEffHr

> histEffHrBand'           :: StatsPlot
> histEffHrBand' fn n _ ps _ = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps 
>   histEffHrBand fn n effs ps
        
> histEffHrBand fn n effs ps =
>     histogramPlots (histAttrs t x y fn) $ zip titles [pBand, effByBand]
>       where
>         -- histogram data has to get shifted (in Plots.lhs)
>         -- but this looks silly for an enumeration, so (+1) below
>         pBand     = [((+1) . fromIntegral . fromEnum $ b, d) | (b, d) <- periodBand ps]
>         effByBand = [((+1) . fromIntegral . fromEnum $ b, e) | (b, e) <- periodEfficiencyByBand ps effs]
>         t = "Hours by Band Histogram" ++ n
>         x = "Band [L, S, C, X, U, K, A, Q]"
>         y = "Counts [Scheduled Hours]"
>         titles = [Just "Observed", Just "Obs * Eff"]

simHistFreq

> histSessFreq          :: StatsPlot
> histSessFreq fn n ss ps _ =
>     histogramPlots attrs $ zip titles [sessionFreqHrs ss, periodFreqHrs ps, periodFreqBackupHrs ps]
>   where
>     t = "Frequency Histogram" ++ n
>     x = "Frequency [GHz]"
>     y = "Counts [Hours]"
>     titles = [Just "Available", Just "Observed", Just "Obs. Backup"]
>     attrs = (histAttrs t x y fn) ++ [XRange (0, 51)]


simFracCanceledFreq

> histCanceledFreqRatio fn n _ ps trace =
>     scatterPlot attrs $ periodCanceledFreqRatio ps trace
>   where
>     t = "Canceled/Scheduled by Frequency" ++ n
>     x = "Frequency [GHz]"
>     y = "Canceled Hrs/Scheduled Hrs"
>     attrs = (tail $ histAttrs t x y fn) ++ [XRange (0, 51), YRange (0, 0.5)]

simHistDec

> histSessDec            :: StatsPlot
> histSessDec fn n ss ps _ =
>     histogramPlots attrs $ zip titles [sessionDecHrs ss, periodDecHrs ps]
>   where
>     t = "Declination Histogram" ++ n
>     x = "Declination [deg]"
>     y = "Counts [Hours]"
>     titles = [Just "Available", Just "Observed"]
>     attrs = (histAttrs t x y fn) ++ [XRange (-40, 90)]

simHistPFHours - need pressure history
simHistPF - need pressure history
simHistTP

> histSessTP         :: StatsPlot
> histSessTP fn n _ ps _ =
>     histogramPlot attrs $ [(x, fromIntegral y) | (x, y) <- sessionTP ps]
>   where
>     t = "Telescope Period Histogram" ++ n
>     x = "TP [Hours]"
>     y = "Counts"
>     attrs = (histAttrs t x y fn) ++ [XRange (0, 13), YRange (0.5, 1000.0)]

simHistTPQtrs 

> histSessTPQtrs :: StatsPlot
> histSessTPQtrs fn n ss ps _ = 
>     histogramPlot attrs tpDurs
>   where
>     tpDurs  = [(fromIntegral x, fromIntegral y) | (x, y) <- sessionTPQtrs ps]
>     totalNumTPs = sum $ map snd tpDurs
>     meanTimes = histStat mean' tpDurs
>     stdTimes = histStat stddev' tpDurs
>     t = printf "Telescope Period Historgram (%f, %f, %f) %s" totalNumTPs meanTimes stdTimes n
>     x = "TP [Minutes]"
>     y = "Counts"
>     attrs = (histAttrs t x y fn) ++ [XRange (60, 780), YRange (0.5, 1000.0)]

simHistTPDurs - how are Session minDuratin and Period duration distributed in terms of actual minutes?

> histSessTPDurs :: StatsPlot
> histSessTPDurs fn n ss ps _ = 
>     --histogramPlots attrs $ zip titles [maxTPTime, tpDurs]
>     histogramPlot attrs tpDurs
>   where
>     tpDurs  = [(fromIntegral x, fromIntegral y) | (x, y) <- periodDuration ps]
>     --maxTPTime  = [(fromIntegral x, fromIntegral y) | (x, y) <- sessionMinDurMaxTime ss]
>     t = "Telescope Period Historgram" ++ n
>     x = "TP [Minutes]"
>     y = "Counts [Minutes]"
>     --titles = [Just "Available", Just "Observed"]
>     attrs = (histAttrs t x y fn) ++ [XRange (60, 780)]

Utilities

> getObservingEfficiency w p = do 
>     let now' = pForecast p
>     w'     <- newWeather w $ Just now'
>     result <- runScoring w' [] (observingEfficiency (startTime p) (session p))
>     return $ eval result

> historicalObsEff w = mapM (getObservingEfficiency w) 

This function is only temporary until we get simulations integrated
TBF: how does this give us the score at the time that a period ran?
The weather is using (2006 1 1), so as year progresses, what forecast
will they be using?

> getScore      :: ScoreFunc -> Period -> Scoring Score
> getScore sf p = liftM eval . sf dt . session $ p
>   where
>     dt = replaceYear 2006 . startTime $ p

> historicalObsScore w ps = do
>     w' <- newWeather w . Just $ dt
>     runScoring w' [] $ genScore dt (map session ps) >>= \sf -> mapM (getScore sf) ps
>       where
>         dt = fromGregorian' 2006 1 1

Attributes

> scatterAttrs title xlab ylab fpath =
>     [LogScale "y"
>    , Title title
>    , XLabel xlab
>    , YLabel ylab
>     ] ++ if fpath == "" then [] else [PNG fpath]

> histAttrs title xlab ylab fpath =
>     [LogScale "y"
>    , Title title
>    , XLabel xlab
>    , YLabel ylab
>     ] ++ if fpath == "" then [] else [PNG fpath]

Testing Harness

> testPlot      :: StatsPlot -> String -> IO ()
> testPlot plot fn = do
>     (sessions, periods) <- getData
>     plot fn "" sessions periods undefined

> getData :: IO ([Session], [Period])
> getData = do
>     g <- getStdGen
>     let sessions = generate 0 g $ genSessions 100
>     let periods  = generate 0 g $ genPeriods 100
>     return $ (sessions, periods)

> testPlots      :: [([Session] -> [Period] -> IO ())] -> IO [()]
> testPlots plots = do
>     (sessions, periods) <- getData
>     sequence (map (\f -> f sessions periods) plots)

Simulator Harness

> type StatsPlot = String -> String -> [Session] -> [Period] -> [Trace] -> IO ()

> statsPlots = map (\f -> f "" "") statsPlotsList 

TBF: combine this list with the statsPlotsToFile fnc

> statsPlotsList = [
>    plotDecFreq 
>  , plotDecVsRA 
>  , plotEffVsFreq' 
>  , plotFreqVsTime 
>  , plotSatRatioVsFreq 
>  , plotEffElev' 
>  , plotMinObsEff 
>  , plotEffLst' 
>  , plotElevDec 
>  , plotScoreElev' 
>  , plotScoreFreq 
>  , plotLstScore' 
>  , histSessRA 
>  , histEffHrBand' 
>  , histSessFreq 
>  , histSessDec 
>  , histSessTP 
>  , histSessTPQtrs 
>  , histSessTPDurs 
>  , plotSchdFreqVsTime    
>  , histCanceledFreqRatio 
>  , plotBandPressureTime  
>  , plotRAPressureTime1   
>  , plotRAPressureTime2  
>  , plotRAPressureTime3  
>   ]

> statsPlotsToFile rootPath name = map (\f-> f n) [
>    plotDecFreq        $ rootPath ++ "/simDecFreq.png"
>  , plotDecVsRA        $ rootPath ++ "/simDecRA.png"
>  , plotEffVsFreq'     $ rootPath ++ "/simEffFreq.png"
>  , plotMeanObsEffVsFreq $ rootPath ++ "/simSchdMeanEffFreq.png"
>  , plotMeanAtmEffVsFreq $ rootPath ++ "/simSchdMeanAtmFreq.png"
>  , plotMeanTrkEffVsFreq $ rootPath ++ "/simSchdMeanTrkFreq.png"
>  , plotMeanSrfEffVsFreq $ rootPath ++ "/simSchdMeanSrfFreq.png"
>  , plotFreqVsTime     $ rootPath ++ "/simFreqTime.png"
>  --, plotSatRatioVsFreq $ rootPath ++ "/simSatisfyFreq.png"
>  , plotEffElev'       $ rootPath ++ "/simEffElev.png"
>  , plotEffLst'        $ rootPath ++ "/simEffLST.png"
>  , plotMinObsEff      $ rootPath ++ "/simMinObsEff.png"
>  , plotEffVsFreqBin   $ rootPath ++ "/simMeanObsEff.png"
>  , plotElevDec        $ rootPath ++ "/simElevDec.png"
>  --, plotScoreElev'     $ rootPath ++ "/simScoreElev.png"
>  , plotScoreFreq      $ rootPath ++ "/simScoreFreq.png"
>  --, plotLstScore'      $ rootPath ++ "/simScoreLST.png"
>  , histSessRA         $ rootPath ++ "/simHistRA.png"
>  , histEffHrBand'     $ rootPath ++ "/simHistEffHr.png"
>  , histSessFreq       $ rootPath ++ "/simHistFreq.png"
>  , histSessDec        $ rootPath ++ "/simHistDec.png"
>  --, histSessTP         $ rootPath ++ "/simHistTP.png"
>  , histSessTPQtrs     $ rootPath ++ "/simHistTPQtrs.png"
>  , histSessTPDurs     $ rootPath ++ "/simHistTPDurs.png"
>  , plotSchdFreqVsTime    $ rootPath ++ "/simFreqSchTime.png"
>  , histCanceledFreqRatio $ rootPath ++ "/simFracCanceledFreq.png"
>  , plotBandPressureTime  $ rootPath ++ "/simBandPFTime.png"
>  , plotRAPressureTime1   $ rootPath ++ "/simLSTPFTime1.png"
>  , plotRAPressureTime2   $ rootPath ++ "/simLSTPFTime2.png"
>  , plotRAPressureTime3   $ rootPath ++ "/simLSTPFTime3.png"
>  , plotFractionalTime   $ rootPath ++ "/simFracTime.png"
>  , plotFracBandTime   $ rootPath ++ "/simFracBandTime.png"
>  , plotFracSemesterTime $ rootPath ++ "/simFracSemesterTime.png"
>  , plotTPDurVsFreqBin $ rootPath ++ "/simTPFreq.png"
>  , plotRemainingTimeByBand $ rootPath ++ "/simRemainingTime.png"
>  , plotPastSemesterTimeByBand $ rootPath ++ "/simPastSemesterTime.png"
>  , plotBandPressureBinPastTime $ rootPath ++ "/simBandPBinPastTime.png"
>  , plotBandPressureBinRemainingTime $ rootPath ++ "/simBandPBinRemainingTime.png"
>  , plotStringency     $ rootPath ++ "/stringency.png"
>  , plotMinEffSysTemp  $ rootPath ++ "/minEffSysTemp.png"
>   ]
>   where
>     n = if name == "" then "" else " (" ++ name ++ ")"

> textReports :: String -> String -> DateTime -> Int -> DateTime -> Int -> String -> [Session] -> [Period] -> [Period] -> [(DateTime, Minutes)] -> [(String, [Float])] -> Bool -> ReceiverSchedule -> [Period] -> Bool -> IO () 
> textReports name outdir now execTime dt days strategyName ss ps canceled gaps scores simInput rs history quiet = do
>     if (quiet == False) then putStrLn $ report else putStrLn $ "Quiet Flag Set - report available in file: " ++ filepath
>     writeFile filepath report
>   where
>     (year, month, day, hours, minutes, seconds) = toGregorian now
>     nowStr = printf "%04d_%02d_%02d_%02d_%02d_%02d" year month day hours minutes seconds
>     filename = "simulation_" ++ nowStr ++ ".txt"
>     filepath = if last outdir == '/' then outdir ++ filename else outdir ++ "/" ++ filename
>     r1 = reportSimulationGeneralInfo name now execTime dt days strategyName ss ps simInput
>     r2 = reportScheduleChecks ss ps gaps history 
>     r3 = reportSimulationTimes ss dt (24 * 60 * days) ps canceled
>     r4 = reportSemesterTimes ss' ps' 
>     r5 = reportSemesterBandTimes ss' ps' 
>     r6 = reportBandTimes ss' ps' 
>     r7 = reportScheduleScores scores
>     r8 = reportSessionTypes ss ps
>     r9 = reportRcvrSchedule rs
>     r10 = reportPreScheduled history
>     r11 = reportFinalSchedule ps
>     r12 = reportCanceled canceled
>     r13 = reportSessionDetails ss
>     r14 = reportObserverDetails ss
>     report = concat [r1, r2, r6, r3, r4, r5, r7, r8, r9, r10, r11, r12, r13, r14] 
>     ss' = removeMaintenanceS ss
>     ps' = removeMaintenanceP ps

> removeMaintenanceS = filter (\s -> (sName s) /= "Maintenance") 

> removeMaintenanceP = filter (\p -> (sName . session $ p) /= "Maintenance")  

> reportObserverDetails :: [Session] -> String
> reportObserverDetails ss = "Observer Details: \n" ++ (concatMap (\s -> (show . observers . project $ s) ++ "\n") ss)

> reportSessionDetails :: [Session] -> String
> reportSessionDetails ss = "Session Details: \n" ++ (concatMap (\s -> (show s) ++ "\n") ss)

> reportSimulationGeneralInfo :: String -> DateTime -> Int -> DateTime -> Int -> String -> [Session] -> [Period] -> Bool -> String
> reportSimulationGeneralInfo name now execTime start days strategyName ss ps simInput =
>     heading ++ "    " ++ intercalate "    " [l0, l1, l2, l3, l4, l5, l6]
>   where
>     heading = "General Simulation Info: \n"
>     l0 = printf "Simulation Name: %s\n" name
>     l1 = printf "Ran Simulations on: %s\n" (toSqlString now)
>     l2 = printf "Simulation Execution Speed: %d seconds\n" execTime
>     l3 = printf "Ran Simulations starting at: %s for %d days (%d hours)\n" (toSqlString start) days (days*24)
>     l4 = printf "Ran strategy %s\n" strategyName
>     l5 = if simInput then printf "Using simulated data.\n" else "Using real data.\n"
>     l6 = printf "Number of Sessions as input: %d\n" (length ss)

> reportScheduleChecks :: [Session] -> [Period] -> [(DateTime, Minutes)] -> [Period] -> String
> reportScheduleChecks ss ps gaps history =
>     heading ++ "    " ++ intercalate "    " [overlaps, fixed, durs, sTime, pTime, tb, scores, gs, ras, decs, elevs, rfiFlag, lstEx, trans]
>   where
>     heading = "Schedule Checks: \n"
>     error = "WARNING: "
>     overlaps = if internalConflicts ps then error ++ "Overlaps in Schedule!\n" else "No Overlaps in Schedule\n"
>     fixed = if (not $ scheduleHonorsFixed history ps) then error ++ "Schedule does not honor pre-scheduled Periods!\n" else "Pre-scheduled Periods Honored\n"
>     durs = if (not . obeyDurations $ psOpen) then error ++ "Min/Max Durations NOT Honored!\n" else "Min/Max Durations Honored\n"
>     sTime = if (disobeySessionAlloted psOpen /= []) then error ++ "Session Alloted Time NOT Honored: " ++ (show . disobeySessionAlloted $ psOpen) ++ "\n" else "Session Alloted Time Honored\n"
>     pTime = if (disobeyProjectAlloted psOpen /= []) then error ++ "Project Alloted Time NOT Honored: " ++ (show . disobeyProjectAlloted $ psOpen) ++ "\n" else "Project Alloted Time Honored\n"
>     tb = if (disobeyTimeBetween psOpen /= []) then error ++ "Time Between NOT Honored: " ++ (show . disobeyTimeBetween $ psOpen) ++ "\n" else "Time Between Honored.\n"
>     scores = if (validScores ps) then "All scores >= 0.0\n" else error ++ "Socres < 0.0!\n"
>     gs = if (gaps == []) then "No Gaps in Schedule.\n" else error ++ "Gaps in Schedule: " ++ (show $ map (\g -> (toSqlString . fst $ g, snd g)) gaps) ++ "\n"
>     ras = if validRAs ss then "0 <= RAs <= 24\n" else error ++ "RAs NOT between 0 and 24 hours!\n"
>     decs = if validDecs ss then "-40 <= Decs <= 90\n" else error ++ "Decs NOT between -40 and 90 degrees!\n"
>     elevs = if validElevs psOpen then "5 <= Elevs <= 90\n" else error ++ "Elevations NOT between 5 and 90 degrees!\n"
>     rfiFlag = if (disobeyLowRFI psOpen) == [] then "Low RFI Flags Honored\n" else error ++ "Low RFI Flags NOT Honored: "++ (show . disobeyLowRFI $ ps) ++"\n"
>     lstEx = if (disobeyLSTExclusion psOpen) == [] then "LST Exclusion Ranges Honored\n" else error ++ "LST Exclusion Ranges NOT Honored: " ++ (show . disobeyLSTExclusion $ ps) ++ "\n"
>     trans = if (disobeyTransit psOpen) == [] then "Transit Flags Honored\n" else error ++ "Transit Flags NOT Honored: " ++ (show . disobeyTransit $ psOpen) ++ "\n"
>     psOpen = filter (\p -> (sType . session $ p) == Open) ps

> reportSimulationTimes :: [Session] -> DateTime -> Minutes -> [Period] -> [Period] -> String 
> reportSimulationTimes ss dt dur observed canceled = 
>     heading ++ "    " ++ intercalate "    " [l1, l2, l3, l4, l5]
>   where
>     heading = "Simulation Time Breakdown: \n"
>     l1 = printf "%-9s %-9s %-9s %-9s %-9s\n" "simulated" "session" "backup" "scheduled" "observed" 
>     l2 = printf "%-9.2f %-9.2f %-9.2f %-9.2f %-9.2f\n" t1 t2 t3 t6 t7
>     l3 = printf "%-9s %-9s %-9s %-9s %-9s\n"  "canceled" "obsBackup" "totalDead" "schedDead" "failedBckp"
>     l4 = printf "%-9.2f %-9.2f %-9.2f %-9.2f %-9.2f\n" t8 t9 t10 t11 t12
>     l5 = crossCheckSimulationBreakdown t1 t6 t7 t8 t9 t10 t11 t12 
>     (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = breakdownSimulationTimes ss dt dur observed canceled

> reportSemesterTimes :: [Session] -> [Period] -> String 
> reportSemesterTimes ss ps = do
>     heading ++ "    " ++ intercalate "    " ([hdr] ++ lines)
>   where
>     heading = "Simulation By Semester: \n"
>     hdr = printf "%-9s %-9s %-9s %-9s %-9s %-9s %-9s\n" "Sem  " "Total" "Backup" "ObsInSem" "ObsBpIn" "ObsFrSem" "ObsBpFr" 
>     semesters = ["0"++ show x ++ y | x <- [4..9], y <- ["A","B","C"]]
>     lines = map (reportSemesterHrs ss ps) semesters

> reportSemesterBandTimes :: [Session] -> [Period] -> String 
> reportSemesterBandTimes ss ps = do
>     heading ++ "    " ++ intercalate "    " ([hdr] ++ lines)
>   where
>     heading = "Simulation By Semester and Band: \n"
>     hdr = printf "%-9s %-9s %-9s %-9s %-9s %-9s %-9s %-9s %-9s\n" "Sem  " "L" "S" "C" "X" "U" "K" "A" "Q" 
>     semesters = ["0"++ show x ++ y | x <- [4..9], y <- ["A","B","C"]]
>     lines = map (reportSemesterBandHrs ss ps) semesters

> reportSessionTypes :: [Session] -> [Period] -> String
> reportSessionTypes ss ps = do
>     heading ++ "    " ++ intercalate "    " [hdr, l1, l2, l3, l4]
>   where
>     heading = "Simulation By Session Type (Fixed includes Maint.): \n"
>     hdr = printf "%-13s %-11s %-11s %-11s %-11s\n" "Type" "Session #" "Session Hrs" "Period #" "Period Hrs" 
>     l1 = reportSessionTypeHrs Open ss ps 
>     l2 = reportSessionTypeHrs Fixed ss ps 
>     l3 = reportSessionTypeHrs Windowed ss ps 
>     l4 = reportSessionNameHrs "Maintenance" ss ps 

> reportSessionTypeHrs :: SessionType -> [Session] -> [Period] -> String
> reportSessionTypeHrs st ss ps = printf "%-11s : %-11d %-11.2f %-11d %-11.2f\n" (show st) stCnt stHrs pstCnt pstHrs
>   where
>     ssTyped = filter (\s -> sType s == st) ss 
>     psTyped = filter (\p -> (sType . session $ p) == st) ps 
>     stCnt = length ssTyped
>     stHrs =  totalHrs ss (\s -> sType s == st) 
>     pstCnt = length psTyped
>     pstHrs =  totalPeriodHrs ps (\p -> (sType . session $ p) == st) 

> reportSessionNameHrs :: String -> [Session] -> [Period] -> String
> reportSessionNameHrs name ss ps = printf "%-9s : %-11d %-11.2f %-11d %-11.2f\n" (name) stCnt stHrs pstCnt pstHrs
>   where
>     ssTyped = filter (\s -> sName s == name) ss 
>     psTyped = filter (\p -> (sName . session $ p) == name) ps 
>     stCnt = length ssTyped
>     stHrs =  totalHrs ss (\s -> sName s == name) 
>     pstCnt = length psTyped
>     pstHrs =  totalPeriodHrs ps (\p -> (sName . session $ p) == name) 
 
> reportBandTimes :: [Session] -> [Period] -> String 
> reportBandTimes ss ps = do
>     heading ++ "    " ++ intercalate "    " [hdr, l1, l2]
>   where
>     heading = "Simulation By Band: \n"
>     hdr = printf "%s      %-9s %-9s %-9s %-9s %-9s %-9s %-9s %-9s\n" "Type" "L" "S" "C" "X" "Ku" "K" "Ka" "Q"
>     sessBandTimes = sessionBand ss
>     periodBandTimes = periodBand ps
>     l1 = "Sessions: " ++ toStr sessBandTimes
>     l2 = "Periods : " ++ toStr periodBandTimes
>     toStr times = (concatMap (printf "%-9.2f " . snd) times) ++ "\n"

> reportSemesterBandHrs :: [Session] -> [Period] -> String -> String
> reportSemesterBandHrs ss ps sem = semStr ++ (concat bandStrs) ++ "\n"
> -- printf "%-7s : %-9.2f %-9.2f %-9.2f %-9.2f %-9.2f %-9.2f\n" sem total totalBackup totalObs totalBackupObs totalObsFrom totalBackupObsFrom 
>   where
>     semStr = printf "%-7s : " sem
>     bandStrs = map (printf "%-9.2f ") bandSemHrs
>     bandSemHrs = map (bandSemHrs' ss) [L .. Q]
>     bandSemHrs' sess b = totalHrs sess $ isInSemesterAndBand sem b
>     isInSemesterAndBand semester b s = (isInSemester s semester) && (band s == b)


> reportSemesterHrs :: [Session] -> [Period] -> String -> String
> reportSemesterHrs ss ps sem = printf "%-7s : %-9.2f %-9.2f %-9.2f %-9.2f %-9.2f %-9.2f\n" sem total totalBackup totalObs totalBackupObs totalObsFrom totalBackupObsFrom 
>   where
>     total = totalHrs ss (\s -> isInSemester s sem) 
>     totalBackup = totalHrs ss (\s -> isInSemester s sem && backup s)
>     totalObs = totalPeriodHrs ps (\p -> isPeriodInSemester p sem)
>     totalBackupObs = totalPeriodHrs ps (\p -> isPeriodInSemester p sem && pBackup p)
>     totalObsFrom = totalPeriodHrs ps (\p -> isPeriodFromSemester p sem)
>     totalBackupObsFrom = totalPeriodHrs ps (\p -> isPeriodFromSemester p sem && pBackup p)

> reportScheduleScores :: [(String, [Score])] -> String
> reportScheduleScores scores =
>   heading ++ "    " ++ intercalate "    " [obsEff, atmEff, trkEff, srfEff]
>     where
>   heading = "Schedule Score Checks: \n"
>   error = "WARNING: "
>   getScores name s = snd . head $ filter (\x -> fst x == name) s
>   checkNormalized scores key name = if not . normalized $ getScores key scores then error ++ name ++ " not Normalized!\n" else "0.0 <= " ++ name ++ " <= 1.0\n"
>   obsEff = checkNormalized scores "obsEff" "Observing Efficiency"
>   atmEff = checkNormalized scores "atmEff" "Atmospheric Opacity"
>   trkEff = checkNormalized scores "trkEff" "Tracking Efficiency"
>   srfEff = checkNormalized scores "srfEff" "Surface Observing Efficiency"

> reportRcvrSchedule :: ReceiverSchedule -> String
> reportRcvrSchedule rs = hdr ++ (dates rs)
>   where
>     hdr = "Receiver Schedule:\n"
>     dates rs = concatMap (\(dt, rcvrs) -> (show . toSqlString $ dt) ++ " : " ++ (show rcvrs) ++ "\n") rs

> reportPreScheduled :: [Period] -> String
> reportPreScheduled ps = hdr ++ (printPeriods . sort $ ps)
>   where
>     hdr = "Pre-Schedule Periods:\n"
>     printPeriods ps = concatMap (\p -> (show p) ++ "\n") ps

> reportFinalSchedule :: [Period] -> String
> reportFinalSchedule ps = hdr ++ (printPeriods ps)
>   where
>     hdr = "Final Schedule:\n"
>     printPeriods ps = concatMap (\p -> (show p) ++ "\n") ps

> reportCanceled :: [Period] -> String
> reportCanceled ps = hdr ++ (printPeriods ps)
>   where
>     hdr = "Canceled Periods:\n"
>     printPeriods ps = concatMap (\p -> (show p) ++ "\n") ps

> reportTotalScore :: [Period] -> String
> reportTotalScore ps = "\n\nTotal Score: " ++ (show total) ++ "\n"
>   where
>     total = sum [pScore p * fromIntegral (duration p `div` 15) | p <- ps]

Trying to emulate the Beta Test's Scoring Tab:

> {-
> scoringReport :: Int -> DateTime -> Minutes -> IO ()
> scoringReport sessionId dt dur = do
>   (rs, ss, projs, history') <- dbInput dt
>   let s = head $ filter (\sess -> (sId sess) == sessionId) ss
>   scoringInfo s ss dt dur rs
> -}

> {-
> scoringReportByName :: String -> DateTime -> Minutes -> IO ()
> scoringReportByName name dt dur = do
>   (rs, ss, projs, history') <- dbInput dt
>   let s = head $ filter (\sess -> (sName sess) == name) ss
>   scoringInfo s ss dt dur rs
> -}

> createPlotsAndReports :: [[Session] -> [Period] -> [Trace] -> IO ()] -> String -> String -> DateTime -> Int -> DateTime -> Int -> String -> [Session] -> [Period] -> [Trace] -> Bool -> ReceiverSchedule -> [Period] -> Bool -> IO ()
> createPlotsAndReports sps name outdir now execTime dt days strategyName ss schedule trace simInput rs history quiet = do
>     let gaps = findScheduleGaps dt dur schedule
>     let canceled = getCanceledPeriods trace
>     let os = getOriginalSchedule' schedule canceled
>     schdObsEffs <- historicalSchdObsEffs schedule
>     schdAtmEffs <- historicalSchdAtmEffs schedule
>     schdTrkEffs <- historicalSchdTrkEffs schedule
>     schdSrfEffs <- historicalSchdSrfEffs schedule
>     let scores = [("obsEff", schdObsEffs)
>                 , ("atmEff", schdAtmEffs)
>                 , ("trkEff", schdTrkEffs)
>                 , ("srfEff", schdSrfEffs)]
>     -- text reports 
>     --now <- getCurrentTime
>     textReports name outdir now execTime dt days strategyName ss schedule canceled gaps scores simInput rs history quiet 
>     -- create plots
>     mapM_ (\f -> f ss' schedule' trace) sps
>   where
>     dur = days * 60 * 24
>     ss' = removeMaintenanceS ss
>     schedule' = removeMaintenanceP schedule

