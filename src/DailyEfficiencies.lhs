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

./dailyEfficiencies 3

> main = do 
>   args <- getArgs
>   let numDaysStr = head args
>   putStrLn $ printf "Running daily efficiencies for %s days, plots to current directory\n" numDaysStr 
>   -- TBF: error checking on these values
>   let numDays = read numDaysStr::Int
>   -- TBF: get this to be an option
>   let start = fromGregorian 2006 2 1 0 0 0
>   runDailyEfficiencies start numDays True -- start days simInput
