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

> module Main where

> import Antioch.DateTime
> import Antioch.RunHistWeatherOpt (runHistWeatherOpt)
> import System.Environment
> import Data.Maybe

This is the main entry point for our 'poor man's parallel' code for 
updating the stringency table (part of historical weather).

./genhists "2008-01-01 00:00:00"  "2011-01-01 00:00:00"

> main = do
>   args <- getArgs
>   let start = fromJust . fromSqlString . head $ args
>   let end   = fromJust . fromSqlString . last $ args
>   let numCores = 2
>   runHistWeatherOpt start end numCores


