> module Antioch.Reports where

> import Antioch.Types
> import Antioch.Statistics
> import Antioch.Plots
> import Antioch.Generators (genSessions, genPeriods)
> import System.Random
> import Test.QuickCheck

> testPlot      :: ([Session] -> [Period] -> IO ()) -> IO ()
> testPlot plot = do
>     g <- getStdGen
>     let sessions = generate 0 g $ genSessions 100
>     let periods  = generate 0 g $ genPeriods 100
> --    putStrLn . show $ map startTime periods
>     plot sessions periods

scatter plots ********************************************
simDecFreq (stars, crosses)
simDecRA (stars, crosses)
simEffFreq (error bars, crosses, line plot)
simMeanEffFreq (error bars, crosses, line plot)

simFreqTime (circles, dt on x-axis)

> plotFreqVsTime _ ps = scatterPlot $ zip (historicalTime' ps) (historicalFreq ps)

simSatisfyFreq (error bars)

> plotSatRatioVsFreq ss ps = errorBarPlot $ satisfactionRatio ss ps

scatter plots (crosses) ***********************************
simEffElev

> historicalObsEff ps = sequence [randomRIO (0.0, 1.0) | _ <- ps]

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

simPFLST
simScoreElev
simScoreLST

> plotRaDec ss ps = scatterPlots $ [sessRADec ss, perRADec ps]

line plots (functions) *************************************
simBandPFTime
simLSTPFTime1
simLSTPFTime2
simLSTPFTime3

histograms 2x **********************************************

simHistRA

> histSessRA ss ps =
>     histogramPlots $ [sessRA ss, periodRA ps]

simHistEffHr

simHistFreq

> histSessFreq ss ps =
>     histogramPlots $ [sessFreq ss, periodFreq ps]

simHistDec

> histSessDec ss ps =
>     histogramPlots $ [sessDec ss, periodDec ps]

histograms *************************************************
simHistPFHours

simHistPF

simHistTP

> histSessTP _ ps =
>     histogramPlot $ [(fromIntegral x, fromIntegral y) | (x, y) <- sessTP ps]
