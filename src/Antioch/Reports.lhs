> module Antioch.Reports where

> import Antioch.Statistics
> import Antioch.Plots

scatter plots
simDecFreq (stars, crosses)
simDecRA (stars, crosses)
simEffFreq (error bars, crosses, line plot)
simMeanEffFreq (error bars, crosses, line plot)
simFreqTime (circles, dt on x-axis)
simSatisfyFreq (error bars)

scatter plots (crosses)
simEffElev
simEffLST
simElevDec
simPFLST
simScoreElev
simScoreLST

> plotDecRA ss ps = scatterPlots $ [sessDecRA ss, perDecRA ps]

line plots (functions)
simBandPFTime
simLSTPFTime1
simLSTPFTime2
simLSTPFTime3

histograms 2x 
simHistDec
simHistEffHr
simHistFreq
simHistRA

> histSessDec ss ps =
>     histogramPlots $ [sessDec [(-40), (-38)..92] ss
>                     , periodDec [(-40), (-38)..92] ps]

histograms
simHistPFHours
simHistPF
simHistTP

> histSessTP ps =
>     histogramPlot $ [(fromIntegral x, fromIntegral y) | (x, y) <- sessTP ps]
