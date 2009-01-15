> module Antioch.Reports where

> import Antioch.DateTime
> import Antioch.Generators (genSessions, genPeriods)
> import Antioch.Plots
> import Antioch.Score
> import Antioch.Statistics
> import Antioch.Types
> import Antioch.Weather (getWeather')

> import System.Random
> import Test.QuickCheck

> testPlot      :: ([Session] -> [Period] -> IO ()) -> IO ()
> testPlot plot = do
>     g <- getStdGen
>     let sessions = generate 0 g $ genSessions 100
>     let periods  = generate 0 g $ genPeriods 100
> --    putStrLn . show  $ map (toSqlString . startTime) periods
>     plot sessions periods

scatter plots ********************************************
simDecFreq (stars, crosses)

> plotDecFreq ss ps = scatterPlots $ [sessDecFreq ss, perDecFreq ps]

simDecRA (stars, crosses)

> plotDecVsRA ss ps = scatterPlots $ [sessRADec ss, perRADec ps]

simEffFreq (error bars, crosses, line plot) - Need stats from Dana
simMeanEffFreq (error bars, crosses, line plot) - Need stats from Dana

simFreqTime (circles, dt on x-axis)

> plotFreqVsTime _ ps = scatterPlot $ zip (map fromIntegral $ historicalTime' ps) (historicalFreq ps)

simSatisfyFreq (error bars)

> plotSatRatioVsFreq ss ps = errorBarPlot $ satisfactionRatio ss ps

scatter plots (crosses) ***********************************
simEffElev

> getEfficiency p = do
>   w <- getWeather' $ startTime p
>   result <- runScoring w [] (efficiency (startTime p) (session p))
>   case result of
>       Nothing     -> return 0.0
>       Just result -> return result

> historicalObsEff = mapM getEfficiency

> plotEffElev' _ ps = do
>   effs <- historicalObsEff ps
>   plotEffElev effs ps

> plotEffElev effs ps = scatterPlot $ zip (map elevationFromZenith ps) effs

simEffLST

> plotEffLst' _ ps = do
>   effs <- historicalObsEff ps
>   plotEffLst effs ps

> plotEffLst effs ps = scatterPlot $ zip (historicalLST ps) effs

simElevDec

> plotElevDec' _ ps = do
>   effs <- historicalObsEff ps
>   plotElevDec effs ps
>
> plotElevDec effs ps = scatterPlot $ decVsElevation ps effs

simPFLST - need pressure history

simScoreElev

> historicalObsScore ps = sequence [randomRIO (0.0, 10.0) | _ <- ps]
>
> plotScoreElev' _ ps = do
>   scores <- historicalObsScore ps
>   plotScoreElev scores ps

> plotScoreElev scores ps = scatterPlot $ zip (map elevationFromZenith ps) scores

simScoreLST

> plotLstScore' _ ps = do
>   scores <- historicalObsScore ps
>   plotLstScore scores ps
>
> plotLstScore scores ps = scatterPlot $ zip (historicalLST ps) scores

???

> plotRaDec ss ps = scatterPlots $ [sessRADec ss, perRADec ps]

line plots (functions) *************************************
simBandPFTime - need pressure history
simLSTPFTime1 - need pressure history
simLSTPFTime2 - need pressure history
simLSTPFTime3 - need pressure history

histograms 2x **********************************************

simHistRA

> histSessRA ss ps =
>     histogramPlots $ [sessRA ss, periodRA ps]

simHistEffHr

> histEffHrBand' _ ps = do
>   effs <- historicalObsEff ps
>   histEffHrBand effs ps
        
> histEffHrBand effs ps =
>     histogramPlots $ [pBand, effByBand]
>       where
>         pBand     = [(fromIntegral . fromEnum $ b, fromIntegral d) | (b, d) <- perBand ps]
>         effByBand = [(fromIntegral . fromEnum $ b, e) | (b, e) <- perEfficiencyByBand ps effs]

simHistFreq

> histSessFreq ss ps =
>     histogramPlots $ [[(f, fromIntegral t) | (f, t) <- sessFreq ss]
>                     , [(f, fromIntegral t) | (f, t) <- periodFreq ps]]

simHistDec

> histSessDec ss ps =
>     histogramPlots $ [sessDec ss, periodDec ps]

histograms *************************************************
simHistPFHours - need pressure history

simHistPF - need pressure history

simHistTP

> histSessTP _ ps =
>     histogramPlot $ [(fromIntegral x, fromIntegral y) | (x, y) <- sessTP ps]
