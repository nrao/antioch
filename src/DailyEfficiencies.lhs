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
> import Antioch.RunSimulation

> import System.Environment
> import Text.Printf
> import Maybe

./dailyEfficiencies "2008-02-01 00:00:00" 3

> main = do 
>   args <- getArgs
>   let start = fromJust . fromSqlString . head $ args
>   let numDaysStr = last args
>   putStrLn $ printf "Running daily efficiencies from %s for %s days, plots to current directory\n" (toSqlString start) numDaysStr 
>   -- error checking on these values
>   -- Story: https://www.pivotaltracker.com/story/show/14123373
>   let numDays = read numDaysStr::Int
>   runDailyEfficiencies start numDays True -- start days simInput
