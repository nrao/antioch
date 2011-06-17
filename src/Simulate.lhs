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
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Simulate
> import Antioch.Reports
> import Antioch.RunSimulation
> import SimulateOpts

> import Data.Maybe             (fromJust)
> import System.Environment
> import Text.Printf
> import Text.Regex

./simulate --help

./simulate -s=Pack -o=simulations -b=2008-02-01 -d=3 -n=test5_3

./simulate -s=Pack -o=simulations -b=2007-02-01 -d=30 -n=test5_3 -t=50/25/25 -m=True -l=100

> main = do 
>   args <- getArgs
>   (stgStr, dir, beginStr, numDaysStr, name, maintStr, typesStr, backlogStr) <- simulateOpts args
>   putStrLn $ printf "Running simulation '%s' using strategy %s for %s days, output to directory %s\n" name stgStr numDaysStr dir
>   -- error checking on these values
>   -- Story: https://www.pivotaltracker.com/story/show/14123373
>   let start = fromJust . fromSqlString $ (beginStr ++ " 00:00:00")
>   let numDays = read numDaysStr :: Int
>   let stg = read stgStr :: StrategyName
>   let maint = read maintStr :: Bool
>   let [open, fixed, windowed] = map read . splitRegex (mkRegex "/") $ typesStr :: [Int]
>   let backlog = read backlogStr :: Int
>   runSimulation stg start numDays maint open fixed windowed backlog dir name True True False
