> module Main where

> import Antioch.DateTime
> --import Antioch.Schedule
> --import Antioch.Score
> --import Antioch.Simulate
> --import Antioch.Reports
> import Antioch.RunSimulation
> --import SimulateOpts

> import System.Environment
> import Text.Printf
> import Maybe

./dailyEfficiencies "2008-02-01 00:00:00" 3

> main = do 
>   args <- getArgs
>   let start = fromJust . fromSqlString . head $ args
>   let numDaysStr = last args
>   putStrLn $ printf "Running daily efficiencies from %s for %s days, plots to current directory\n" (toSqlString start) numDaysStr 
>   -- TBF: error checking on these values
>   let numDays = read numDaysStr::Int
>   runDailyEfficiencies start numDays True -- start days simInput
