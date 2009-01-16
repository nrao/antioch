> module Antioch.Reports where

> import Antioch.DateTime
> import Antioch.Generators (genSessions, genPeriods)
> import Antioch.Plots
> import Antioch.Score
> import Antioch.Statistics
> import Antioch.Types
> import Antioch.Weather
> import Control.Monad      (liftM)

> import System.Random
> import Test.QuickCheck hiding (promote, frequency)

simDecFreq (stars, crosses)

> plotDecFreq ss ps = scatterPlots $ [sessDecFreq ss, perDecFreq ps]

simDecRA (stars, crosses)

> plotDecVsRA ss ps = scatterPlots $ [sessRADec ss, perRADec ps]

simEffFreq (error bars, crosses, line plot) - Need stats from Dana

> plotEffVsFreq' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffVsFreq effs ps

> plotEffVsFreq effs ps = errorBarPlot $ zip3 meanEffFreq frequencyBins sdomEffFreq
>   where
>     meanEffFreq = meanObsEffByBin $ zip effs (map (frequency . session) ps)
>     sdomEffFreq = sdomObsEffByBin $ zip effs (map (frequency . session) ps)

simMeanEffFreq (error bars, crosses, line plot) - Need stats from Dana
simFreqTime (circles, dt on x-axis)

> plotFreqVsTime _ ps = scatterPlot $ zip (map fromIntegral $ historicalTime' ps) (historicalFreq ps)

simSatisfyFreq (error bars)

> plotSatRatioVsFreq ss ps = errorBarPlot $ satisfactionRatio ss ps

simEffElev

> plotEffElev' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffElev effs ps

> plotEffElev effs ps = scatterPlot $ zip (map elevationFromZenith ps) effs

simEffLST

> plotEffLst' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotEffLst effs ps

> plotEffLst effs ps = scatterPlot $ zip (historicalLST ps) effs

simElevDec

> plotElevDec' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
>   plotElevDec effs ps
>
> plotElevDec effs ps = scatterPlot $ decVsElevation ps effs

simPFLST - need pressure history

simScoreElev

> plotScoreElev' _ ps = do
>   w      <- getWeather Nothing
>   let sf = genScore $ map session ps
>   scores <- historicalObsScore w sf ps
>   plotScoreElev scores ps

> plotScoreElev scores ps = scatterPlot $ zip (map elevationFromZenith ps) scores

simScoreLST

> plotLstScore' _ ps = do
>   w      <- getWeather Nothing
>   let sf = genScore $ map session ps
>   scores <- historicalObsScore w sf ps
>   plotLstScore scores ps
>
> plotLstScore scores ps = scatterPlot $ zip (historicalLST ps) scores

simBandPFTime - need pressure history
simLSTPFTime1 - need pressure history
simLSTPFTime2 - need pressure history
simLSTPFTime3 - need pressure history

simHistRA

> histSessRA ss ps =
>     histogramPlots $ [sessRA ss, periodRA ps]

simHistEffHr

> histEffHrBand' _ ps = do
>   w    <- getWeather Nothing
>   effs <- historicalObsEff w ps
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

simHistPFHours - need pressure history
simHistPF - need pressure history
simHistTP

> histSessTP _ ps =
>     histogramPlot $ [(fromIntegral x, fromIntegral y) | (x, y) <- sessTP ps]

Utilities

> getEfficiency w p = do
>     let now' = (replaceYear 2006 (startTime p))
>     w'     <- newWeather w $ Just now'
>     result <- runScoring w' [] (efficiency now' (session p))
>     case result of
>         Nothing     -> return 0.0
>         Just result -> return result
> historicalObsEff w = mapM (getEfficiency w)

> getScore      :: ScoreFunc -> Period -> Scoring Score
> getScore sf p = liftM eval . sf now . session $ p
>   where
>     now = replaceYear 2006 . startTime $ p
  
> historicalObsScore w sf ps = do
>     w' <- newWeather w . Just $ fromGregorian' 2006 1 1
>     runScoring w [] $ mapM (getScore sf) ps

Testing Harness

> testPlot      :: ([Session] -> [Period] -> IO ()) -> IO ()
> testPlot plot = do
>     g <- getStdGen
>     let sessions = generate 0 g $ genSessions 100
>     let periods  = generate 0 g $ genPeriods 100
> --    putStrLn . show  $ map (toSqlString . startTime) periods
>     plot sessions periods

