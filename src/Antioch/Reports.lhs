
> module Antioch.Reports where

> import Antioch.DateTime
> import Antioch.Generators (internalConflicts, endTime, genProjects, genSessions, genPeriods, generateVec)
> import Antioch.Plots
> import Antioch.Score
> import Antioch.Schedule
> import Antioch.Simulate
> import Antioch.Statistics
> import Antioch.Types
> import Antioch.Utilities (rad2deg, rad2hr)
> import Antioch.Weather
> import Antioch.Debug
> import Control.Monad      (liftM)
> import Text.Printf
> import System.Random
> import System.CPUTime
> import Test.QuickCheck hiding (promote, frequency)
> import Graphics.Gnuplot.Simple

simDecFreq (stars, crosses)

> plotDecFreq          :: StatsPlot
> plotDecFreq fn ss ps =
>      scatterPlots (scatterAttrs t x y fn) $ zip titles $ [[(x, rad2deg y) | (x, y) <- sessionDecFreq ss]
>                                            , [(x, rad2deg y) | (x, y) <-  periodDecFreq ps]]
>   where
>     t   = "Dec vs Freq"
>     x   = "Frequency [GHz]"
>     y   = "Declination [deg]"
>     titles = [Just "Available", Just "Observed"]

simDecRA (stars, crosses)

> plotDecVsRA          :: StatsPlot
> plotDecVsRA fn ss ps =
>     scatterPlots (scatterAttrs t x y fn) $ zip titles $ [[(rad2hr x, rad2deg y) | (x, y) <- sessionDecRA ss]
>                                           , [(rad2hr x, rad2deg y) | (x, y) <-  periodDecRA ps]]
>   where
>     t = "Dec vs RA"
>     x = "Right Ascension [hr]"
>     y = "Declination [deg]"
>     titles = [Just "Available", Just "Observed"]

simEffFreq (error bars, crosses, line plot) - Need stats from Dana

> plotEffVsFreq'         :: StatsPlot
> plotEffVsFreq' fn _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffVsFreq'' fn effs ps

> plotEffVsFreq'' fn effs ps =
>     scatterPlot attrs $ zip (historicalFreq ps) effs
>   where
>     t     = "Observing Efficiency vs Frequency"
>     x     = "Frequency [GHz]"
>     y     = "Observing Efficiency"
>     attrs = (scatterAttrs t x y fn) ++ [XRange (0, 51), YRange (-0.1, 1.1)]


> plotEffVsFreq fn effs ps =
>     errorBarPlot (scatterAttrs t x y fn) $ zip3 meanEffFreq frequencyBins sdomEffFreq
>   where
>     meanEffFreq = meanObsEffByBin $ zip effs (map (frequency . session) ps)
>     sdomEffFreq = sdomObsEffByBin $ zip effs (map (frequency . session) ps)
>     t = "Observing Efficiency vs Frequency"
>     x = "Frequency [GHz]"
>     y = "Observing Efficiency"

simMeanEffFreq (error bars, crosses, line plot) - Need stats from Dana
simFreqTime (circles, dt on x-axis)

> plotFreqVsTime         :: StatsPlot
> plotFreqVsTime fn _ ps =
>     scatterPlot (scatterAttrs t x y fn) $ zip (map fromIntegral $ historicalTime' ps) (historicalFreq ps)
>   where
>     t = "Frequency vs Time"
>     x = "Time [days]"
>     y = "Frequency [GHz]"

simSatisfyFreq (error bars)

> plotSatRatioVsFreq          :: StatsPlot
> plotSatRatioVsFreq fn ss ps =
>     errorBarPlot (scatterAttrs t x y fn) $ satisfactionRatio ss ps
>   where
>     t = "Satisfaction Ratio vs Frequency"
>     x = "Frequency [GHz]"
>     y = "Satisfaction Ratio"

simEffElev

> plotEffElev'         :: StatsPlot
> plotEffElev' fn _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffElev fn effs ps

> plotEffElev fn effs ps = scatterPlot (scatterAttrs t x y fn) $ zip (map elevationFromZenith ps) effs
>   where
>     t = "Observing Efficiency vs Elevation"
>     x = "Elevation [deg]"
>     y = "Observing Efficiency"

> plotMinObsEff        :: StatsPlot
> plotMinObsEff fn _ _ = plotFunc attrs (linearScale 1000 (0, 50)) minObservingEff
>   where
>     t     = "Observing Efficiency vs Frequency"
>     x     = "Frequency [GHz]"
>     y     = "Observing Efficiency"
>     attrs = (scatterAttrs t x y fn) ++ [XRange (0, 51), YRange (-0.1, 1.1)]

simEffLST

> plotEffLst'         :: StatsPlot
> plotEffLst' fn _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffLst fn effs ps

> plotEffLst fn effs ps =
>     scatterPlot (scatterAttrs t x y fn) $ zip (historicalLST ps) effs
>   where
>     t = "Observing Efficiency vs LST"
>     x = "LST [hours]"
>     y = "Observing Efficiency"

simElevDec

> plotElevDec'         :: StatsPlot
> plotElevDec' fn _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotElevDec fn effs ps
>
> plotElevDec fn effs ps =
>     scatterPlot (scatterAttrs t x y fn) $ [(x, rad2deg y) | (x, y) <- decVsElevation ps effs]
>   where
>     t = "Dec vs Elevation"
>     x = "Elevation [deg]"
>     y = "Declination [deg]"

simPFLST - need pressure history

simScoreElev


> plotScoreElev'         :: StatsPlot
> plotScoreElev' fn _ ps = do
>   w       <- getWeather Nothing
>   --sf <- genScore $ map session ps
>   scores  <- historicalObsScore w ps
>   plotScoreElev fn scores ps

> plotScoreElev fn scores ps =
>     scatterPlot (scatterAttrs t x y fn) $ zip (map elevationFromZenith ps) scores
>   where
>     t = "Score vs Elevation"
>     x = "Elevation [deg]"
>     y = "Score"

simScoreLST

> plotLstScore'         :: StatsPlot
> plotLstScore' fn _ ps = do
>   w       <- getWeather Nothing
>   --sf <- genScore $ map session ps
>   scores  <- historicalObsScore w ps
>   plotLstScore fn scores ps
>
> plotLstScore fn scores ps =
>     scatterPlot (scatterAttrs t x y fn) $ zip (historicalLST ps) scores
>   where
>     t = "Score vs LST"
>     x = "LST [hours]"
>     y = "Score"

simBandPFTime

> 
> plotBandPressureTime fn _ trace = 
>     linePlots (scatterAttrs t x y fn) $ zip titles $ bandPressuresByTime trace 
>   where
>     t = "Band Pressure Factor vs Time"
>     x = "Time [days]"
>     y = "Band Pressure Factor"
>     titles = [Just "L", Just "S", Just "C", Just "X", Just "U", Just "K", Just "A", Just "Q"]
> 

simLSTPFTime1

> plotRAPressureTime1 fn _ trace =
>     linePlots (scatterAttrs t x y fn) $ take 8 $ zip titles $ raPressuresByTime trace 
>   where
>     t = "LST Pressure Factor vs Time"
>     x = "Time [days]"
>     y = "LST Pressure Factor"
>     titles = [Just (show a) | a <- [0 .. 7]]

simLSTPFTime2 - need pressure history

> plotRAPressureTime2 fn _ trace =
>     linePlots (scatterAttrs t x y fn) $ zip titles $ radata
>   where
>     (_, radata) = splitAt 8 $ raPressuresByTime trace
>     t = "LST Pressure Factor vs Time"
>     x = "Time [days]"
>     y = "LST Pressure Factor"
>     titles = [Just (show a) | a <- [8 .. 15]]

simLSTPFTime3 - need pressure history

> plotRAPressureTime3 fn _ trace =
>     linePlots (scatterAttrs t x y fn) $ zip titles $ radata
>   where
>     (_, radata) = splitAt 16 $ raPressuresByTime trace 
>     t = "LST Pressure Factor vs Time"
>     x = "Time [days]"
>     y = "LST Pressure Factor"
>     titles = [Just (show a) | a <- [16 .. 23]]

simHistRA

> histSessRA          :: StatsPlot
> histSessRA fn ss ps =
>     histogramPlots (histAttrs t x y fn) $ zip titles [sessionRAHrs ss, periodRAHrs ps]
>   where
>     t = "Right Ascension Histogram"
>     x = "RA [hr]"
>     y = "Counts [Hours]"
>     titles = [Just "Available", Just "Observed"]

simHistEffHr

> histEffHrBand'         :: StatsPlot
> histEffHrBand' fn _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   histEffHrBand fn effs ps
        
> histEffHrBand fn effs ps =
>     histogramPlots (histAttrs t x y fn) $ zip titles [pBand, effByBand]
>       where
>         pBand     = [(fromIntegral . fromEnum $ b, d) | (b, d) <- periodBand ps]
>         effByBand = [(fromIntegral . fromEnum $ b, e) | (b, e) <- periodEfficiencyByBand ps effs]
>         t = "Hours by Band Histogram"
>         x = "Band [L, S, C, X, U, K, A, Q]"
>         y = "Counts [Scheduled Hours]"
>         titles = [Just "Observed", Just "Obs * Eff"]

simHistFreq

> histSessFreq          :: StatsPlot
> histSessFreq fn ss ps =
>     histogramPlots (histAttrs t x y fn) $ zip titles [sessionFreqHrs ss, periodFreqHrs ps, periodFreqBackupHrs ps]
>   where
>     t = "Frequency Histogram"
>     x = "Frequency [GHz]"
>     y = "Counts [Hours]"
>     titles = [Just "Available", Just "Observed", Just "Obs. Backup"]

simHistCanceledFreq

> histCanceledFreqRatio fn ps trace =
>     histogramPlot (histAttrs t x y fn) $ periodCanceledFreqRatio ps trace
>   where
>     t = "Canceled/Scheduled Frequency Histogram"
>     x = "Frequency [GHz]"
>     y = "Canceled/Scheduled [Hours]"

simHistDec

> histSessDec          :: StatsPlot
> histSessDec fn ss ps =
>     histogramPlots (histAttrs t x y fn) $ zip titles [sessionDecHrs ss, periodDecHrs ps]
>   where
>     t = "Declination Histogram"
>     x = "Declination [deg]"
>     y = "Counts [Hours]"
>     titles = [Just "Available", Just "Observed"]

simHistPFHours - need pressure history
simHistPF - need pressure history
simHistTP

> histSessTP         :: StatsPlot
> histSessTP fn _ ps =
>     histogramPlot (histAttrs t x y fn) $ [(fromIntegral x, fromIntegral y) | (x, y) <- sessionTP ps]
>   where
>     t = "Telescope Period Histogram"
>     x = "TP [Hours]"
>     y = "Counts"

simHistTPQtrs 

> histSessTPQtrs :: StatsPlot
> histSessTPQtrs fn ss ps = 
>     histogramPlot (histAttrs t x y fn) tpDurs
>   where
>     tpDurs  = [(fromIntegral x, fromIntegral y) | (x, y) <- sessionTPQtrs ps]
>     t = "Telescope Period Historgram"
>     x = "TP [Minutes]"
>     y = "Counts"

simHistTPDurs - how are Session minDuratin and Period duration distributed in terms of actual minutes?

> histSessTPDurs :: StatsPlot
> histSessTPDurs fn ss ps = 
>     histogramPlots (histAttrs t x y fn) $ zip titles [maxTPTime, tpDurs]
>   where
>     tpDurs  = [(fromIntegral x, fromIntegral y) | (x, y) <- periodDuration ps]
>     maxTPTime  = [(fromIntegral x, fromIntegral y) | (x, y) <- sessionMinDurMaxTime ss]
>     t = "Telescope Period Historgram"
>     x = "TP [Minutes]"
>     y = "Counts [Minutes]"
>     titles = [Just "Available", Just "Observed"]

Utilities

> getEfficiency w p = do
>     let now' = (replaceYear 2006 (startTime p))
>     w'     <- newWeather w $ Just now'
>     result <- runScoring w' [] (efficiency now' (session p))
>     case result of
>         Nothing     -> return 0.0
>         Just result -> return result

> historicalObsEff w = mapM (getEfficiency w)

This function is only temporary until we get simulations integrated
TBF: how does this give us the score at the time that a period ran?
The weather is using (2006 1 1), so as year progresses, what forecast
will they be using?

> getScore      :: ScoreFunc -> Period -> Scoring Score
> getScore sf p = liftM eval . sf dt . session $ p
>   where
>     dt = replaceYear 2006 . startTime $ p

> --historicalObsScore :: Weather -> [Period] -> IO [Score]
> historicalObsScore w ps = do
>     w' <- newWeather w . Just $ fromGregorian' 2006 1 1
>     runScoring w' [] $ genScore (map session ps) >>= \sf -> mapM (getScore sf) ps

Attributes

> scatterAttrs title xlab ylab fpath =
>     [Title title
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
>     plot fn sessions periods

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

> type StatsPlot = String -> [Session] -> [Period] -> IO ()

> statsPlots = [
>    plotDecFreq ""
>  , plotDecVsRA ""
>  , plotEffVsFreq' ""
>  , plotFreqVsTime "" 
>  , plotSatRatioVsFreq ""
>  , plotEffElev' ""
>  , plotMinObsEff ""
>  , plotEffLst' ""
>  , plotElevDec' ""
>  --, plotScoreElev' ""
>  --, plotLstScore' ""
>  , histSessRA "" 
>  , histEffHrBand' ""
>  , histSessFreq ""
>  , histSessDec ""
>  , histSessTP ""
>  , histSessTPQtrs ""
>  , histSessTPDurs ""
>   ]

> statsPlotsToFile rootPath = [
>    plotDecFreq        $ rootPath ++ "/simDecFreq.png"
>  , plotDecVsRA        $ rootPath ++ "/simDecRA.png"
>  , plotEffVsFreq'     $ rootPath ++ "/simEffFreq.png"
>  , plotFreqVsTime     $ rootPath ++ "/simFreqTime.png"
>  , plotSatRatioVsFreq $ rootPath ++ "/simSatisfyFreq.png"
>  , plotEffElev'       $ rootPath ++ "/simEffElev.png"
>  , plotEffLst'        $ rootPath ++ "/simEffLST.png"
>  , plotMinObsEff      $ rootPath ++ "/simMinObsEff.png"
>  , plotElevDec'       $ rootPath ++ "/simElevDec.png"
>  --, plotScoreElev'     $ rootPath ++ "/simScoreElev.png"
>  --, plotLstScore'      $ rootPath ++ "/simScoreLST.png"
>  , histSessRA         $ rootPath ++ "/simHistRA.png"
>  , histEffHrBand'     $ rootPath ++ "/simHistEffHr.png"
>  , histSessFreq       $ rootPath ++ "/simHistFreq.png"
>  --, histSessBackupFreq $ rootPath ++ "/simHistBackupFreq.png"
>  , histSessDec        $ rootPath ++ "/simHistDec.png"
>  , histSessTP         $ rootPath ++ "/simHistTP.png"
>  , histSessTPQtrs     $ rootPath ++ "/simHistTPQtrs.png"
>  , histSessTPDurs     $ rootPath ++ "/simHistTPDurs.png"
>   ]

> tracePlotsToFile rootPath = [
>    histCanceledFreqRatio $ rootPath ++ "/simHistCanceledFreq.png"
>  , plotBandPressureTime $ rootPath ++ "/simBandPFTime.png"
>  , plotRAPressureTime1 $ rootPath ++ "/simLSTPFTime1.png"
>  , plotRAPressureTime2 $ rootPath ++ "/simLSTPFTime2.png"
>  , plotRAPressureTime3 $ rootPath ++ "/simLSTPFTime3.png"
>    ]


> generatePlots :: Strategy -> [[Session] -> [Period] -> IO ()] -> [[Period] -> [Trace] -> IO ()] -> Int -> IO ()
> generatePlots sched sps trace_plots days = do
>     w <- getWeather Nothing
>     let g   = mkStdGen 1
>     let projs = generate 0 g $ genProjects 255 --230 
>     let ss' = concatMap sessions projs
>     let ss  = zipWith (\s n -> s {sId = n}) ss' [0..]
>     putStrLn $ "Number of sessions: " ++ show (length ss)
>     putStrLn $ "Total Time: " ++ show (sum (map totalTime ss)) ++ " minutes"
>     start <- getCPUTime
>     (results, trace) <- simulate sched w rs dt dur int history [] ss
>     stop <- getCPUTime
>     let execTime = fromIntegral (stop-start) / 1.0e12 
>     putStrLn $ "Simulation Execution Speed: " ++ show execTime ++ " seconds"
>     let gaps = findScheduleGaps dt dur results
>     let canceled = getCanceledPeriods trace
>     -- text reports 
>     now <- getCurrentTime
>     textReports now execTime dt days "scheduleMinDuration" ss results canceled gaps
>     --putStrLn $ reportSimulationGeneralInfo now dt days "scheduleMinDuration" ss results
>     --putStrLn $ reportScheduleChecks results gaps
>     --putStrLn $ reportSimulationTimes ss dt dur results canceled
>     --putStrLn $ reportSemesterTimes ss results 
>     -- create plots
>     mapM_ (\f -> f ss results) sps
>     -- create plots from trace; TBF : fold these into above
>     mapM_ (\f -> f results trace) trace_plots
>   where
>     rs      = []
>     dt      = fromGregorian 2006 2 1 0 0 0
>     dur     = 60 * 24 * days
>     int     = 60 * 24 * 2
>     history = []

> textReports :: DateTime -> Float -> DateTime -> Int -> String -> [Session] -> [Period] -> [Period] ->[(DateTime, Minutes)] -> IO () 
> textReports now execTime dt days "scheduleMinDuration" ss ps canceled gaps = do
>     putStrLn $ report
>     writeFile filename report
>   where
>     (year, month, day, hours, minutes, seconds) = toGregorian now
>     nowStr = printf "%04d_%02d_%02d_%02d_%02d_%02d" year month day hours minutes seconds
>     filename = "simulation_" ++ nowStr ++ ".txt"
>     r1 = reportSimulationGeneralInfo now execTime dt days "scheduleMinDuration" ss ps
>     r2 = reportScheduleChecks ps gaps
>     r3 = reportSimulationTimes ss dt (24 * 60 * days) ps canceled
>     r4 = reportSemesterTimes ss ps 
>     report = concat [r1, r2, r3, r4]

> reportSimulationGeneralInfo :: DateTime -> Float -> DateTime -> Int -> String -> [Session] -> [Period] -> String
> reportSimulationGeneralInfo now execTime start days strategyName ss ps =
>   heading ++ (concat $ map ("    "++) [l0, l1, l2, l3, l4])
>     where
>   heading = "General Simulation Info: \n"
>   l0 = printf "Ran Simulations on: %s\n" (toSqlString now)
>   l1 = printf "Simulation Execution Speed: %f seconds\n" execTime
>   l2 = printf "Ran Simulations starting at: %s for %d days (%d hours)\n" (toSqlString start) days (days*24)
>   l3 = printf "Ran strategy %s\n" strategyName
>   l4 = printf "Number of Sessions as input: %d\n" (length ss)

> reportScheduleChecks :: [Period] -> [(DateTime, Minutes)] -> String
> reportScheduleChecks ps gaps =
>   heading ++ (concat $ map ("    "++) [overlaps, durs, scores, gs])
>     where
>   heading = "Schedule Checks: \n"
>   error = "WARNING: "
>   overlaps = if internalConflicts ps then error ++ "Overlaps in Schedule!\n" else "No Overlaps in Schedule\n"
>   durs = if (not . obeyDurations $ ps) then error ++ "Min/Max Durations NOT Honored!\n" else "Min/Max Durations Honored\n"
>   scores = if (validScores ps) then "All scores >= 0.0\n" else error ++ "Socres < 0.0!\n"
>   gs = if (gaps == []) then "No Gaps in Schedule.\n" else error ++ "Gaps in Schedule: " ++ (show $ map (\g -> (toSqlString . fst $ g, snd g)) gaps) ++ "\n"

> reportSimulationTimes :: [Session] -> DateTime -> Minutes -> [Period] -> [Period] -> String 
> reportSimulationTimes ss dt dur observed canceled = 
>     heading ++ (concat $ map ("    "++) [l1, l2, l3, l4, l5])
>   where
>     heading = "Simulation Time Breakdown: \n"
>     l1 = printf "%-9s %-9s %-9s %-9s %-9s %-9s %-9s\n" "simulated" "session" "backup" "avSess" "avBckp" "scheduled" "observed" 
>     l2 = printf "%-9.2f %-9.2f %-9.2f %-9.2f %-9.2f %-9.2f %-9.2f\n" t1 t2 t3 t4 t5 t6 t7
>     l3 = printf "%-9s %-9s %-9s %-9s %-9s\n"  "canceled" "obsBackup" "totalDead" "schedDead" "failedBckp"
>     l4 = printf "%-9.2f %-9.2f %-9.2f %-9.2f %-9.2f\n" t8 t9 t10 t11 t12
>     l5 = crossCheckSimulationBreakdown t1 t6 t7 t8 t9 t10 t11 t12 
>     (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) = breakdownSimulationTimes ss dt dur observed canceled

> reportSemesterTimes :: [Session] -> [Period] -> String 
> reportSemesterTimes ss ps = do
>     heading ++ (concat $ map ("    "++) [hdr, l1, l2, l3, l4])
>   where
>     heading = "Simulation By Semester: \n"
>     hdr = printf "%s   %-9s %-9s %-9s %-9s\n" "Sem" "Total" "Backup" "Obs" "ObsBp" 
>     l1 = reportSemesterHrs "05C" ss ps 
>     l2 = reportSemesterHrs "06A" ss ps 
>     l3 = reportSemesterHrs "06B" ss ps 
>     l4 = reportSemesterHrs "06C" ss ps 

> reportSemesterHrs :: String -> [Session] -> [Period] -> String
> reportSemesterHrs sem ss ps  = printf "%s : %-9.2f %-9.2f %-9.2f %-9.2f\n" sem total totalBackup totalObs totalBackupObs  
>   where
>     total = totalHrs ss (\s -> isInSemester s sem) 
>     totalBackup = totalHrs ss (\s -> isInSemester s sem && backup s)
>     totalObs = totalPeriodHrs ps (\p -> isPeriodInSemester p sem)
>     totalBackupObs = totalPeriodHrs ps (\p -> isPeriodInSemester p sem && pBackup p)

> runSim days filepath = generatePlots scheduleMinDuration (statsPlotsToFile filepath) days
