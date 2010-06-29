> module Antioch.HistoricalWeatherTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.HistoricalWeather
> import Antioch.Score
> import Antioch.ReceiverTemperatures
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.Score
> --import Control.Monad.Trans  (lift, liftIO)
> import Test.HUnit
> import Data.Maybe           (isJust, fromJust)

> tests = TestList [
>    test_getWeatherDates
>  , test_getMinEffSysTemp
>  , test_calculateEffSysTemps
>  , test_tSysPrimeNow
>  , test_getRaDec
>  --, test_stringencyLimit
>                  ]

> test_getWeatherDates = TestCase $ do
>     let wdts = map toSqlString getWeatherDates
>     assertEqual "getWeatherDates" True True

> test_getRcvrFreqs = TestCase $ do
>   let freqs = concatMap (getRcvrFreqs (\f -> f >= 2) 1 1) [Rcvr1_2 .. Rcvr2_3] 
>   assertEqual "getRcvrFreqs_1" (tail exp) freqs
>   let freqs = concatMap (getRcvrFreqs (\f -> True) 1 1) [Rcvr1_2 .. Rcvr2_3] 
>   assertEqual "getRcvrFreqs_2" exp freqs
>   let freqs = concatMap (getRcvrFreqs (\f -> True) 1000 1000) [Rcvr1_2 .. Rcvr2_3] 
>   assertEqual "getRcvrFreqs_3" exp2 freqs
>   let freqs = concatMap (getRcvrFreqs (\f -> True) 1000 100) [Rcvr_RRI .. Rcvr_1070] 
>   assertEqual "getRcvrFreqs_3" exp3 freqs
>     where
>   exp = [(Rcvr1_2, 1), (Rcvr1_2, 2), (Rcvr2_3, 2), (Rcvr2_3, 3)]
>   exp2 = map (\(r, f) -> (r, f*1000)) exp
>   rriFreqs = [(Rcvr_RRI, f) | f <- [100, 200 .. 1000]]
>   exp3 = rriFreqs ++ [(Rcvr_342, 300), (Rcvr_342, 400)
>       , (Rcvr_450, 400), (Rcvr_450, 500)
>       , (Rcvr_600, 500), (Rcvr_600, 600), (Rcvr_600, 700)
>       , (Rcvr_800, 700), (Rcvr_800, 800), (Rcvr_800, 900)
>       , (Rcvr_1070, 900), (Rcvr_1070, 1000)]

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
>   let raDecs = map getRaDec' args
>   let els = map (getElevs (head dts)) $ take 18 raDecs 
>   let elsInt = map round els
>   assertEqual "getRaDec" elsInt elevations
>   let els = map (getElevs (last dts)) $ drop 18 raDecs 
>   let elsInt = map round els
>   assertEqual "getRaDec" elsInt elevations
>     where
>       elevations = [5,10 .. 90]::[Int]
>       dts = [fromGregorian 2006 2 1 0 0 0
>            , fromGregorian 2006 7 1 0 0 0]
>       args = [(fromIntegral el, dt) | dt <- dts, el <- elevations]
>       getRaDec' (el, dt) = getRaDec el dt
>       getElevs dt (ra, dec) = rad2deg $ elevation dt (defaultSession {ra = ra, dec = dec})

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


> {-
> test_calculateStringencyLimits = TestCase $ do
>   sls <- calculateStringencyLimits Rcvr4_6 5 45 False
>   assertEqual "calculateStringencyLimits_1" 25 (length sls)
>   sls <- calculateStringencyLimits Rcvr40_52 45 45 False
>   assertEqual "calculateStringencyLimits_2" 25 (length sls)
> -}

> test_getStringencyArgs = TestCase $ do
>   let args = getStringencyArgs
>   assertEqual "getStringencyArgs_1" 8772 (length args)
>   assertEqual "getStringencyArgs_2" (Rcvr1_2,1,5,False) (head args)
>   assertEqual "getStringencyArgs_3" (RcvrArray18_26,28,90,True) (last args)

> test_getStringencies = TestCase $ do
>   rts <- getReceiverTemperatures
>   let args = [(Rcvr1_2, 1, 5, False), (Rcvr1_2, 1, 5, True)]
>   strs <- getStringencies rts args
>   print strs
>   assertEqual "getStringencies_1" True True 
