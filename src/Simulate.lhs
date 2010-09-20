> module Main where

> import Antioch.DateTime
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Simulate
> import Antioch.Reports
> import Antioch.RunSimulation
> import SimulateOpts

> import System.Environment
> import Text.Printf

./simulate --help

./simulate -s=Pack -o=simulations -d=3 -n=test5_3

> main = do 
>   args <- getArgs
>   (stgStr, dir, numDaysStr, name) <- simulateOpts args
>   putStrLn $ printf "Running simulation '%s' using strategy %s for %s days, output to directory %s\n" name stgStr numDaysStr dir
>   -- TBF: error checking on these values
>   let numDays = read numDaysStr::Int
>   let stg = read stgStr::StrategyName
>   -- TBF: get this to be an option
>   let start = fromGregorian 2008 2 1 0 0 0
>   --generatePlots stg dir (statsPlotsToFile dir name) start numDays name True True
>   --generatePlots2db stg dir (statsPlotsToFile dir name) start numDays name False True
>   -- runSimulation strategyName outdir sps dt days name simInput quiet
>   runSimulation stg dir (statsPlotsToFile dir name) start numDays name True True
