> module ScoringReport where

> import Antioch.DateTime      (fromGregorian)
> import Antioch.Reports       (scoringReportByName)
> import Control.Monad.Error
> import System.Environment

> usage :: IOError -> IO()
> usage e = do 
>   putStrLn "\nOops! Check your command line.\n"
>   putStrLn "Arguments to ScoringReport are: <session name> <year> <month> <day> <hour> <minute> <duration>\n"

> main = (do 
>   [name, year', month', day', hour', minute', duration'] <- getArgs
>   let [year, month, day, hour, minute, duration] = map read [year', month', day', hour', minute', duration'] :: [Int]
>   let date = fromGregorian year month day hour minute 00 
>   scoringReportByName name date duration)
>   `catchError` usage
