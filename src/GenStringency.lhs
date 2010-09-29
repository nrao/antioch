> module Main where

> import Antioch.DateTime
> import Antioch.HistWeatherOpt (calcStringencyByDate)
> import System.Environment
> import Data.Maybe

Entry point for progarm that fills stringency table for given dates.
This will most likely be called by higher level program for filling
in historical weather with improved peformance (in parallel).

> main = do
>     args <- getArgs
>     print args
>     let dts = map (fromJust . fromSqlString) args
>     calcStringencyByDate (head dts) (last dts)
