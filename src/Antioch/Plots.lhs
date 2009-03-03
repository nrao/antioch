> module Antioch.Plots where

> import Antioch.DateTime (DateTime)
> import Graphics.Gnuplot.Simple

> histStyle :: PlotStyle
> histStyle = PlotStyle Boxes (CustomStyle []) Nothing

> histogramPlot       :: [Attribute] -> [(Float, Float)] -> IO ()
> histogramPlot attrs =
>     plotPathStyle attrs histStyle

> histogramPlots             :: [Attribute] -> [[(Float, Float)]] -> IO ()
> histogramPlots attrs plots =
>     plotPathsStyle attrs [(histStyle, xys) | xys <- plots]

> scatterStyle :: PlotStyle
> scatterStyle = PlotStyle Points (CustomStyle []) Nothing

> lineStyle :: PlotStyle
> lineStyle = PlotStyle Lines (CustomStyle []) Nothing

> scatterPlot       :: [Attribute] -> [(Float, Float)] -> IO ()
> scatterPlot attrs = plotPathStyle attrs scatterStyle

> scatterPlots             :: [Attribute] -> [[(Float, Float)]] -> IO ()
> scatterPlots attrs plots =
>     plotPathsStyle attrs [(scatterStyle, xys) | xys <- plots]

> errorBarPlot :: [Attribute] -> [(Float, Float, Float)] -> IO ()
> errorBarPlot = plotErrorBars

> linePlots             :: [Attribute] -> [[(Float, Float)]] -> IO ()
> linePlots attrs plots =
>     plotPathsStyle attrs [(lineStyle, xys) | xys <- plots]

