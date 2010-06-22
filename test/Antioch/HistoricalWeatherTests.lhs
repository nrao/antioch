> module Antioch.HistoricalWeatherTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.HistoricalWeather
> import Antioch.Score
> import Antioch.ReceiverTemperatures
> import Antioch.Weather
> --import Control.Monad.Trans  (lift, liftIO)
> import Test.HUnit
> import Data.Maybe           (isJust, fromJust)

> tests = TestList [
>    test_getWeatherDates
>  , test_getMinEffSysTemp
>  , test_calculateEffSysTemps
>  , test_tSysPrimeNow
>  , test_getRaDec
>  , test_stringencyLimit
>                  ]

> test_getWeatherDates = TestCase $ do
>     print $ map toSqlString getWeatherDates
>     assertEqual "getWeatherDates" True True

> test_getMinEffSysTemp = TestCase $ do
>     m <- getMinEffSysTemp Rcvr1_2 1 10
>     assertEqual "getMinEffSysTemp" 23.936523 m


> test_calculateEffSysTemps = TestCase $ do
>   sysTemps <- calculateEffSysTemps Rcvr1_2 1 10
>   assertEqual "calculateEffSysTemps" 25 (length sysTemps)
>   assertEqual "calculateEffSysTemps_2" 25 (length sysTemps)
>   assertEqual "calculateEffSysTemps_3" (Just 23.964426) (head sysTemps)
>   assertEqual "calculateEffSysTemps_4" (Just 23.982384) (last sysTemps)

> test_tSysPrimeNow = TestCase $ do
>   tsys <- tSysPrimeNow Rcvr1_2 1 10 now
>   assertEqual "tSysPrimeNow" (Just 23.964115) tsys
>     where
>   now = fromGregorian 2006 6 1 1 0 0

TBF:

> test_getRaDec = TestCase $ do
>   assertEqual "getRaDec" True True

> test_stringencyLimit = TestCase $ do
>   w <- getWeather $ Just dt
>   rt <- getReceiverTemperatures
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr2_3 2.0 25.0 False dt
>   assertEqual "stringencyLimit" (Just 1.0) sl
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr40_52 45.0 45.0 False dt
>   assertEqual "stringencyLimit_2" (Just 0.0) sl
>     where
>   dt = fromGregorian 2006 6 1 1 0 0
>   rcvr = Rcvr2_3

> test_calculateStringencyLimits = TestCase $ do
>   sls <- calculateStringencyLimits Rcvr4_6 5 45 False
>   assertEqual "calculateStringencyLimits_1" 25 (length sls)
>   sls <- calculateStringencyLimits Rcvr40_52 45 45 False
>   assertEqual "calculateStringencyLimits_2" 25 (length sls)
