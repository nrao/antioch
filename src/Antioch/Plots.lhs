Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

> module Antioch.Plots where

> import Antioch.DateTime (DateTime)
> import Graphics.Gnuplot.Simple

Histogram plots

> histStyle :: Maybe String -> PlotStyle
> histStyle = PlotStyle Boxes (CustomStyle [])

> histogramPlot       :: [Attribute] -> [(Float, Float)] -> IO ()
> histogramPlot attrs =
>     plotPathStyle attrs (histStyle Nothing) . adjustHistData

> histogramPlots             :: [Attribute] -> [(Maybe String, [(Float, Float)])] -> IO ()
> histogramPlots attrs plots =
>     plotPathsStyle attrs [(histStyle t, adjustHistData xys) | (t, xys) <- plots]

Our stats functions return histograms w/ the following format:
[(b0, n0), (b1, n1), .. ]
Where n0 specifies the number of values at and below the value of b0, and
      n1 specifies the number of values at and below the value of b1, etc.
Unfortunetly, these plot routines don't interpret histograms this way. It 
seems that they shift everything to the right by half a bin.
Note: currently, irregularly spaced bins aren not supported.  Since it seems
noone is asking for this yet, this is not a problem.

> adjustHistData      :: [(Float, Float)] -> [(Float, Float)]
> adjustHistData xs
>     | length xs > 1 = map adjust xs
>     | otherwise     = xs
>  where
>     adjust (x, y) = (x - step, y)
>     step = (x2 - x1) / 2.0
>     ((x1, _) : (x2, _) : _) = xs

Line plots

> lineStyle :: Maybe String -> PlotStyle
> lineStyle = PlotStyle Lines (CustomStyle [])

> linePlots             :: [Attribute] -> [(Maybe String, [(Float, Float)])] -> IO ()
> linePlots attrs plots =
>     plotPathsStyle attrs [(lineStyle t, xys) | (t, xys) <- plots]

> errorBarPlot :: [Attribute] -> [(Float, Float, Float)] -> IO ()
> errorBarPlot = plotErrorBars

Scatter plots

> scatterStyle :: Maybe String -> PlotStyle
> scatterStyle = PlotStyle Points (CustomStyle [])

> scatterPlot       :: [Attribute] -> [(Float, Float)] -> IO ()
> scatterPlot attrs = 
>     plotPathStyle attrs $ scatterStyle Nothing

> scatterPlots             :: [Attribute] -> [(Maybe String, [(Float, Float)])] -> IO ()
> scatterPlots attrs plots =
>     plotPathsStyle attrs [(scatterStyle t, xys) | (t, xys) <- plots]
