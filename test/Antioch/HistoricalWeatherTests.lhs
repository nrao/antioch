> module Antioch.HistoricalWeatherTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.HistoricalWeather
> import Antioch.Score
> import Antioch.ReceiverTemperatures
> import Antioch.Weather       (getWeatherTest)
> import Antioch.Utilities
> import Antioch.Score
> -- import Control.Monad.Trans  (lift, liftIO)
> import Test.HUnit
> import Data.Maybe           (maybeToList, isJust, fromJust)
> import qualified Data.Map as Map
> import Data.IORef           (newIORef, readIORef, writeIORef)

Interim solution: comparisons use some type of bigbox weather database.

> tests = TestList [
>    test_getWeatherDates
>  -- , test_getRcvrFreqs
>  -- , test_getRcvrFreqArgs
>  -- , test_getMinEffSysTempArgs
>  -- , test_getMinEffSysTemp
>  -- , test_calculateEffSysTemps
>  -- , test_tSysPrimeNow
>  -- , test_getRaDec
>  , test_stringencyLimit
>  -- , test_getStringencyArgs
>  -- , test_getStringencies
>  , test_getRcvrFreqIndices
>  -- , test_limitsToStringency
>  -- , test_stringencyLimitsByDate
>                  ]

> test_getWeatherDates = TestCase $ do
>     let wdts = map toSqlString getWeatherDates
>     assertEqual "getWeatherDates" True True

> {-
> test_getRcvrFreqArgs = TestCase $ do
>     let res = getRcvrFreqArgs' Rcvr_1070
>     assertEqual "test_getRcvrFreqArgs 1" res
>                                          [(Rcvr_1070,900),(Rcvr_1070,1000)]
>     let res = getRcvrFreqArgs' Rcvr_PAR
>     assertEqual "test_getRcvrFreqArgs 3" (length res) 11
>     assertEqual "test_getRcvrFreqArgs 4" (head res) (Rcvr_PAR,  80000)
>     assertEqual "test_getRcvrFreqArgs 5" (last res) (Rcvr_PAR, 100000)
>     assertEqual "test_getRcvrFreqArgs 6" (head . tail $ res) (Rcvr_PAR,  82000)
>     let res = getRcvrFreqArgs' Rcvr_342
>     assertEqual "test_getRcvrFreqArgs 7" res
>                                          [(Rcvr_342,300),(Rcvr_342,400)]
>     let res = getRcvrFreqArgs' Rcvr12_18
>     assertEqual "test_getRcvrFreqArgs 8" res
>                                          [(Rcvr12_18,12000),(Rcvr12_18,13000),(Rcvr12_18,14000),(Rcvr12_18,15000)]
                                          

> test_getMinEffSysTempArgs = TestCase $ do
>   let args = getMinEffSysTempArgs
>   assertEqual "getMinEffSysTempArgs_1" 8428 (length args)
>   assertEqual "getMinEffSysTempArgs_2" (Rcvr_RRI,100,5) (head args)
>   assertEqual "getMinEffSysTempArgs_3" (RcvrArray18_26,28000,90) (last args)


> test_tSysPrimeNow = TestCase $ do
>   w <- getWeatherTest Nothing
>   rts <- getReceiverTemperatures
>   tsys <- tSysPrimeNow w rts Rcvr1_2 2000 10 now
>   assertEqual "tSysPrimeNow" (Just 25.079811) tsys
>     where
>   now = fromGregorian 2006 6 1 1 0 0

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
> -}

> test_stringencyLimit = TestCase $ do
>   w <- getWeatherTest $ Just dt
>   rt <- getReceiverTemperatures
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr2_3 2.0 25.0 SpectralLine dt
>   assertEqual "stringencyLimit 1" True sl
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr40_52 45.0 45.0 SpectralLine dt
>   assertEqual "stringencyLimit 2" True sl
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr2_3 2.0 25.0 Continuum dt
>   assertEqual "stringencyLimit 3" False sl
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr40_52 45.0 45.0 Continuum dt
>   assertEqual "stringencyLimit 4" False sl
>     where
>   dt = fromGregorian 2006 6 1 1 0 0
>   rcvr = Rcvr2_3

> {-
> test_getStringencyArgs = TestCase $ do
>   let args = getStringencyArgs
>   assertEqual "getStringencyArgs_1" 16856 (length args)
>   assertEqual "getStringencyArgs_2" (Rcvr_RRI,100,5,False) (head args)
>   assertEqual "getStringencyArgs_3" (RcvrArray18_26,28000,90,True) (last args)

NOTE: here is a unit test that requires code changes.  Otherwise,
we'd be calculating over 6 years, and would take a long time.
   * make sure you're using a 'bigbox' DB
   * change getWeatherDates to go from 2006-06-10 00:00:00 for 1 day.

> test_getMinEffSysTemp = TestCase $ do
>     effs <- newIORef Map.empty
>     let dt = fromGregorian 2006 6 10 8 30 0
>     m <- getMinEffSysTemp effs Rcvr1_2 2.0 10 dt
>     assertEqual "getMinEffSysTemp" 24.838247 m
> -}
 
> test_getRcvrFreqIndices = TestCase $ do
>   assertEqual "getRcvrFreqIndices 1" [8000,9000,10000] (getRcvrFreqIndices Rcvr8_10)
>   assertEqual "getRcvrFreqIndices 2" [900,1000] (getRcvrFreqIndices Rcvr_1070)
>   assertEqual "getRcvrFreqIndices 3" 80000 (head . getRcvrFreqIndices $ Rcvr_PAR)
>   assertEqual "getRcvrFreqIndices 4" 100000 (last . getRcvrFreqIndices $ Rcvr_PAR)

> {-
> test_limitsToStringency = TestCase $ do
>   assertEqual "limitsToStringency 1" 0.11764706 (limitsToStringency [Just 2.0, Just 5.0, Just 10.0, Just 17.0])
>   assertEqual "limitsToStringency 2" 0.11764706 (limitsToStringency [Just 2.0, Nothing, Just 5.0, Just 10.0, Just 17.0])
> -}

NOTE: here is a unit test that requires code changes.  Otherwise,
we'd be calculating over 6 years, and would take a long time.
   * make sure you're using a 'bigbox' DB
   * change getWeatherDates to go from 2006-06-10 00:00:00 for 1 day.
Also, this test should give an array that in turn leads to the above result

> {-
> test_calculateEffSysTemps = TestCase $ do
>   w <- getWeatherTest Nothing
>   rts <- getReceiverTemperatures
>   assertEqual "calculateEffSysTemps_0" 25 (length getWeatherDates)
>   sysTemps <- calculateEffSysTemps w rts Rcvr1_2 2000 10
>   assertEqual "calculateEffSysTemps_1" 25 (length sysTemps)
>   assertEqual "calculateEffSysTemps_3" (Just 24.91206) (head sysTemps)
>   assertEqual "calculateEffSysTemps_4" (Just 26.538439) (last sysTemps)
>   assertEqual "calculateEffSysTemps_5" (Just 26.538439) (last sysTemps)
>   let minEff =  minimum $ concatMap maybeToList sysTemps
>   assertEqual "calculateEffSysTemps_6"  24.838247 minEff

> test_getStringencies = TestCase $ do
>   rts <- getReceiverTemperatures
>   let args = [(Rcvr1_2, 1000, 5, False), (Rcvr1_2, 1000, 5, True)]
>   strs <- getStringencies rts args
>   print strs
>   assertEqual "getStringencies_1" [1.0,2.7777777] strs

> test_stringencyLimitsByDate = TestCase $ do
>   rts <- getReceiverTemperatures
>   let dt = fromGregorian 2006 6 10 1 0 0
>   res <- stringencyLimitsByDate rts [(Rcvr2_3, 2000, 46, False),(Rcvr2_3, 2000, 47, False)] dt
>   assertEqual "test_stringencyLimitsByDate 1" [Just 1.0, Just 1.0] res
>   res <- stringencyLimitsByDate rts [(Rcvr26_40, 28000, 46, False),(Rcvr26_40, 28000, 47, False)] dt
>   assertEqual "test_stringencyLimitsByDate 2" [Just 0.0, Just 0.0] res
> -}
