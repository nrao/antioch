> module Main where

> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Simulate
> import Antioch.Reports
> import SimulateOpts

> import System.Environment
> import Text.Printf

> main = do 
>   args <- getArgs
>   (stgStr, dir, numDaysStr, name) <- simulateOpts args
>   putStrLn $ printf "Running simulation '%s' using strategy %s for %s days, output to directory %s\n" name stgStr numDaysStr dir
>   -- TBF: error checking on these values
>   let numDays = read numDaysStr::Int
>   let stg = read stgStr::StrategyName
>   generatePlots stg dir (statsPlotsToFile dir) numDays name
