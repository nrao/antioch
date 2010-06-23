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
>     let wdts = map toSqlString getWeatherDates
>     assertEqual "getWeatherDates" True True

> test_getMinEffSysTempArgs = TestCase $ do
>   let args = getMinEffSysTempArgs
>   assertEqual "getMinEffSysTempArgs_1" 7396 (length args)
>   assertEqual "getMinEffSysTempArgs_2" (Rcvr1_2,1,5) (head args)
>   assertEqual "getMinEffSysTempArgs_3" (RcvrArray18_26,28,90) (last args)

> test_getMinEffSysTemp = TestCase $ do
>     w <- getWeather Nothing
>     rts <- getReceiverTemperatures
>     m <- getMinEffSysTemp w rts Rcvr1_2 2 10
>     assertEqual "getMinEffSysTemp" 23.936523 m


> test_calculateEffSysTemps = TestCase $ do
>   w <- getWeather Nothing
>   rts <- getReceiverTemperatures
>   sysTemps <- calculateEffSysTemps w rts Rcvr1_2 1 10
>   assertEqual "calculateEffSysTemps" 25 (length sysTemps)
>   assertEqual "calculateEffSysTemps_2" 25 (length sysTemps)
>   assertEqual "calculateEffSysTemps_3" (Just 23.964426) (head sysTemps)
>   assertEqual "calculateEffSysTemps_4" (Just 23.982384) (last sysTemps)

> test_tSysPrimeNow = TestCase $ do
>   w <- getWeather Nothing
>   rts <- getReceiverTemperatures
>   tsys <- tSysPrimeNow w rts Rcvr1_2 1 10 now
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
>   assertEqual "stringencyLimit" (Just 0.0) sl
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
