> module Antioch.Reports where

> import Antioch.DateTime
> import Antioch.Generators (genSessions, genPeriods)
> import Antioch.Plots
> import Antioch.Score
> import Antioch.Schedule
> import Antioch.Simulate
> import Antioch.Statistics
> import Antioch.Types
> import Antioch.Weather
> import Control.Monad      (liftM)

> import System.Random
> import Test.QuickCheck hiding (promote, frequency)
> import Graphics.Gnuplot.Simple

simDecFreq (stars, crosses)

> plotDecFreq ss ps = scatterPlots (scatterAttrs t x y) $ [sessionDecFreq ss, periodDecFreq ps]
>   where
>     t = "Dec vs Freq"
>     x = "Frequency [GHz]"
>     y = "Declination [deg]" 

simDecRA (stars, crosses)

> plotDecVsRA ss ps = scatterPlots (scatterAttrs t x y) $ [sessionDecRA ss, periodDecRA ps]
>   where
>     t = "Dec vs RA"
>     x = "Right Ascension [hr]"
>     y = "Declination [deg]"

simEffFreq (error bars, crosses, line plot) - Need stats from Dana

> plotEffVsFreq' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffVsFreq effs ps

> plotEffVsFreq effs ps = errorBarPlot (scatterAttrs t x y) $ zip3 meanEffFreq frequencyBins sdomEffFreq
>   where
>     meanEffFreq = meanObsEffByBin $ zip effs (map (frequency . session) ps)
>     sdomEffFreq = sdomObsEffByBin $ zip effs (map (frequency . session) ps)
>     t = "Observing Efficiency vs Frequency"
>     x = "Frequency [GHz]"
>     y = "Observing Efficiency"

simMeanEffFreq (error bars, crosses, line plot) - Need stats from Dana
simFreqTime (circles, dt on x-axis)

> plotFreqVsTime _ ps = scatterPlot (scatterAttrs t x y) $ zip (map fromIntegral $ historicalTime' ps) (historicalFreq ps)
>   where
>     t = "Frequency vs Time"
>     x = "Time [days]"
>     y = "Frequency [GHz]"

simSatisfyFreq (error bars)

> plotSatRatioVsFreq ss ps = errorBarPlot (scatterAttrs t x y) $ satisfactionRatio ss ps
>   where
>     t = "Satisfaction Ratio vs Frequency"
>     x = "Frequency [GHz]"
>     y = "Satisfaction Ratio"

simEffElev

> plotEffElev' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffElev effs ps

> plotEffElev effs ps = scatterPlot (scatterAttrs t x y) $ zip (map elevationFromZenith ps) effs
>   where
>     t = "Efficiency vs Elevation"
>     x = "Elevation [deg]"
>     y = "Observing Efficiency"

simEffLST

> plotEffLst' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffLst effs ps

> plotEffLst effs ps = scatterPlot (scatterAttrs t x y) $ zip (historicalLST ps) effs
>   where
>     t = "Efficiency vs LST"
>     x = "LST [hours]"
>     y = "Observing Efficiency"

simElevDec

> plotElevDec' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotElevDec effs ps
>
> plotElevDec effs ps = scatterPlot (scatterAttrs t x y) $ decVsElevation ps effs
>   where
>     t = "Elevation vs Dec"
>     x = "Declination [deg]"
>     y = "Elevation [deg]"

simPFLST - need pressure history

simScoreElev

> plotScoreElev' _ ps = do
>   w      <- getWeather Nothing
>   let sf = genScore $ map session ps
>   scores <- historicalObsScore w sf ps
>   plotScoreElev scores ps

> plotScoreElev scores ps = scatterPlot (scatterAttrs t x y) $ zip (map elevationFromZenith ps) scores
>   where
>     t = "Score vs Elevation"
>     x = "Elevation [deg]"
>     y = "Score"

simScoreLST

> plotLstScore' _ ps = do
>   w      <- getWeather Nothing
>   let sf = genScore $ map session ps
>   scores <- historicalObsScore w sf ps
>   plotLstScore scores ps
>
> plotLstScore scores ps = scatterPlot (scatterAttrs t x y) $ zip (historicalLST ps) scores
>   where
>     t = "Score vs LST"
>     x = "LST [hours]"
>     y = "Score"

simBandPFTime - need pressure history
simLSTPFTime1 - need pressure history
simLSTPFTime2 - need pressure history
simLSTPFTime3 - need pressure history

simHistRA

> histSessRA ss ps =
>     histogramPlots (histAttrs t x y) $ [sessionRA ss, periodRA ps]
>   where
>     t = "Right Ascension Histogram"
>     x = "RA [hr]"
>     y = "Counts [Hours]"

simHistEffHr

> histEffHrBand' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   histEffHrBand effs ps
        
> histEffHrBand effs ps =
>     histogramPlots (histAttrs t x y) $ [pBand, effByBand]
>       where
>         pBand     = [(fromIntegral . fromEnum $ b, fromIntegral d) | (b, d) <- periodBand ps]
>         effByBand = [(fromIntegral . fromEnum $ b, e) | (b, e) <- periodEfficiencyByBand ps effs]
>         t = "Hours by Band Histogram"
>         x = "Band [L, S, C, X, U, K, A, Q]"
>         y = "Counts [Scheduled Hours]"

simHistFreq

> histSessFreq ss ps =
>     histogramPlots (histAttrs t x y) $ [[(f, fromIntegral t) | (f, t) <- sessionFreq ss]
>                          , [(f, fromIntegral t) | (f, t) <- periodFreq ps]]
>   where
>     t = "Frequency Histogram"
>     x = "Frequency [GHz]"
>     y = "Counts [Hours]"

simHistDec

> histSessDec ss ps =
>     histogramPlots (histAttrs t x y) $ [sessionDec ss, periodDec ps]
>   where
>     t = "Declination Histogram"
>     x = "Declination [deg]"
>     y = "Counts [Hours]"

simHistPFHours - need pressure history
simHistPF - need pressure history
simHistTP

> histSessTP _ ps =
>     histogramPlot (tail $ histAttrs t x y) $ [(fromIntegral x, fromIntegral y) | (x, y) <- sessionTP ps]
>   where
>     t = "Telescope Period Histogram"
>     x = "Session TP [Hours]"
>     y = "Counts"

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

> getScore      :: ScoreFunc -> Period -> Scoring Score
> getScore sf p = liftM eval . sf dt . session $ p
>   where
>     dt = replaceYear 2006 . startTime $ p

> historicalObsScore w sf ps = do
>     w' <- newWeather w . Just $ fromGregorian' 2006 1 1
>     runScoring w' [] $ mapM (getScore sf) ps

> type StatsPlot = [Session] -> [Period] -> IO ()

Attributes

> scatterAttrs title xlab ylab =
>     [Title title
>    , XLabel xlab
>    , YLabel ylab
>     ]

> histAttrs title xlab ylab =
>     [LogScale "y"
>    , Title title
>    , XLabel xlab
>    , YLabel ylab
>     ]

Testing Harness

> testPlot      :: StatsPlot -> IO ()
> testPlot plot = do
>     (sessions, periods) <- getData
>     plot sessions periods

> getData :: IO ([Session], [Period])
> getData = do
>     g <- getStdGen
>     let sessions = generate 0 g $ genSessions 100
>     let periods  = generate 0 g $ genPeriods 100
>     return $ (sessions, periods)

> testPlots      :: [StatsPlot] -> IO [()]
> testPlots plots = do
>     (sessions, periods) <- getData
>     sequence (map (\f -> f sessions periods) plots)

Simulator Harness

> statsPlots = [
>    plotDecFreq
>  , plotDecVsRA
>  , plotEffVsFreq'
>  , plotFreqVsTime
>  , plotSatRatioVsFreq
>  , plotEffElev'
>  , plotEffLst'
>  , plotElevDec'
>  , plotScoreElev'
>  , plotLstScore'
>  , histSessRA
>  , histEffHrBand'
>  , histSessFreq
>  , histSessDec
>  , histSessTP
>   ]

> generatePlots :: Strategy -> [StatsPlot] -> Int -> IO [()]
> generatePlots sched sps days = do
>     w <- getWeather Nothing
>     g <- getStdGen
>     let sessions = generate 0 g $ genSessions 100
>     periods <- simulate sched w rs dt dur int history sessions
>     sequence $ map (\f -> f sessions periods) sps
>   where
>     rs      = []
>     dt      = fromGregorian 2006 1 1 0 0 0
>     dur     = 60 * 24 * days
>     int     = 60 * 24 * 2
>     history = []

  