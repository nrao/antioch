> module Main where

> import Antioch.DateTime
> import Antioch.RunHistWeatherOpt (runHistWeatherOpt, runFillStringencyOpt)
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


