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
