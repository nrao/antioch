> module Antioch.Plots where

> import Graphics.Gnuplot.Simple

> histStyle :: PlotStyle
> histStyle = PlotStyle Boxes (CustomStyle []) Nothing

> histogramPlot    :: [(Float, Float)] -> IO ()
> histogramPlot xys =
>     plotPathStyle [] histStyle xys

> histogramPlots       :: [[(Float, Float)]] -> IO ()
> histogramPlots plots =
>     plotPathsStyle [] [(histStyle, xys) | xys <- plots]

> scatterStyle :: PlotStyle
> scatterStyle = PlotStyle Points (CustomStyle []) Nothing

> scatterPlot       :: [(Float, Float)] -> IO ()
> scatterPlot xys =
>     plotPathStyle [] scatterStyle xys

> scatterPlots       :: [[(Float, Float)]] -> IO ()
> scatterPlots plots =
>     plotPathsStyle [] [(scatterStyle, xys) | xys <- plots]
