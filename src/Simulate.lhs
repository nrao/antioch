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
>   -- TBF: error checking on these values
>   let start = fromJust . fromSqlString $ (beginStr ++ " 00:00:00")
>   let numDays = read numDaysStr :: Int
>   let stg = read stgStr :: StrategyName
>   let maint = read maintStr :: Bool
>   let [open, fixed, windowed] = map read . splitRegex (mkRegex "/") $ typesStr :: [Int]
>   let backlog = read backlogStr :: Int
>   runSimulation stg start numDays maint open fixed windowed backlog dir name True True False
