> module Antioch.Plots where

> import Antioch.DateTime (DateTime)
> import Graphics.Gnuplot.Simple

Histogram plots

> histStyle :: Maybe String -> PlotStyle
> histStyle title = PlotStyle Boxes (CustomStyle []) title

> histogramPlot       :: [Attribute] -> [(Float, Float)] -> IO ()
> histogramPlot attrs =
>     plotPathStyle attrs $ histStyle Nothing

> histogramPlots             :: [Attribute] -> [(Maybe String, [(Float, Float)])] -> IO ()
> histogramPlots attrs plots =
>     plotPathsStyle attrs [(histStyle t, xys) | (t, xys) <- plots]

Line plots

> lineStyle :: Maybe String -> PlotStyle
> lineStyle title = PlotStyle Lines (CustomStyle []) title

> linePlots             :: [Attribute] -> [(Maybe String, [(Float, Float)])] -> IO ()
> linePlots attrs plots =
>     plotPathsStyle attrs [(lineStyle t, xys) | (t, xys) <- plots]


> errorBarPlot :: [Attribute] -> [(Float, Float, Float)] -> IO ()
> errorBarPlot = plotErrorBars


Scatter plots

> scatterStyle :: Maybe String -> PlotStyle
> scatterStyle title = PlotStyle Points (CustomStyle []) title


> scatterPlot       :: [Attribute] -> [(Float, Float)] -> IO ()
> scatterPlot attrs = 
>     plotPathStyle attrs $ scatterStyle Nothing


> scatterPlots             :: [Attribute] -> [(Maybe String, [(Float, Float)])] -> IO ()
> scatterPlots attrs plots =
>     plotPathsStyle attrs [(scatterStyle t, xys) | (t, xys) <- plots]







