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
>  , test_stringencyLimit
>  , test_tSysPrimeNow'
>  , test_getMinEffSysTemp
>  , test_getStringency
>                  ]

> test_getWeatherDates = TestCase $ do
>     --let wdts = map toSqlString getWeatherDates
>     let wdts = getWeatherDates
>     assertEqual "getWeatherDates 1" (24*365) (length wdts)
>     assertEqual "getWeatherDates 2" (fromGregorian 2006 1 1 0 0 0) (head wdts)
>     assertEqual "getWeatherDates 3" (fromGregorian 2006 1 1 1 0 0) (head $ drop 1 wdts)
>     assertEqual "getWeatherDates 4" (fromGregorian 2006 12 31 23 0 0) (last wdts)

> test_stringencyLimit = TestCase $ do
>   w <- getWeatherTest $ Just dt
>   rt <- getReceiverTemperatures
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr2_3 2.0 25.0 SpectralLine dt
>   assertEqual "stringencyLimit 1" True sl
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr40_52 45.0 45.0 SpectralLine dt
>   assertEqual "stringencyLimit 2" True sl
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr2_3 2.0 25.0 Continuum dt
>   assertEqual "stringencyLimit 3" True sl
>   -- lower the elevation a little, and watch the stringency fail
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr2_3 2.0 20.0 Continuum dt
>   assertEqual "stringencyLimit 3.5" False sl
>   sl <- runScoring w [] rt $ stringencyLimit Rcvr40_52 45.0 45.0 Continuum dt
>   assertEqual "stringencyLimit 4" True sl
>     where
>   dt = fromGregorian 2006 6 1 1 0 0
>   rcvr = Rcvr2_3

> test_getStringency = TestCase $ do
>   stringencies <- newIORef Map.empty
>   rts <- getReceiverTemperatures
>   let dt = fromGregorian  2006 6 1 1 0 0
>   w <- getWeatherTest $ Just (addMinutes (-60) dt)
>   let rcvr = Rcvr1_2
>   let freq = head $ getRcvrFreqIndices rcvr
>   let elev = 45
>   -- get one particular stringency
>   result <- runScoring w [] rts $ getStringency stringencies rcvr freq elev Continuum dt
>   strs <- readIORef stringencies
>   let str = Map.lookup (rcvr, freq, elev, Continuum) strs
>   assertEqual "getStringency 1" (Just 1) str 

> test_getMinEffSysTemp = TestCase $ do
>   efficiencies <- newIORef Map.empty
>   rts <- getReceiverTemperatures
>   let dt = fromGregorian  2006 6 1 1 0 0
>   let rcvr = Rcvr1_2
>   let freq = head $ getRcvrFreqIndices rcvr
>   let elev = 45
>   -- dt offset insures that we get forecasts & not real wind
>   w <- getWeatherTest . Just $ (addMinutes (-60) dt)
>   runScoring w [] rts $ getMinEffSysTemp efficiencies rcvr freq elev dt
>   effs <- readIORef efficiencies
>   let eff = Map.lookup (rcvr, freq, elev) effs
>   assertEqual "getMinEffSysTemp 1" (Just 30.83303) eff

> 
> test_tSysPrimeNow' = TestCase $ do
>     let dt = fromGregorian 2006 6 10 8 0 0
>     w <- getWeatherTest $ Just dt
>     rts <- getReceiverTemperatures
>     tsys <- runScoring w [] rts $ tSysPrimeNow' Rcvr1_2 1.1 5 dt
>     assertEqual "test_tSysPrimeNow' 1" (Just 52.92739) tsys
>     tsys <- runScoring w [] rts $ tSysPrimeNow' Rcvr1_2 1.1 45 dt
>     assertEqual "test_tSysPrimeNow' 2" (Just 30.807869) tsys
>     tsys <- runScoring w [] rts $ tSysPrimeNow' Rcvr1_2 1.1 85 dt
>     assertEqual "test_tSysPrimeNow' 3" (Just 29.939125) tsys
>     tsys <- runScoring w [] rts $ tSysPrimeNow' Rcvr2_3 2.2 5 dt
>     assertEqual "test_tSysPrimeNow' 4" (Just 38.114845) tsys
>     tsys <- runScoring w [] rts $ tSysPrimeNow' Rcvr2_3 2.2 45 dt
>     assertEqual "test_tSysPrimeNow' 5" (Just 17.032955) tsys
>     tsys <- runScoring w [] rts $ tSysPrimeNow' Rcvr2_3 2.2 85 dt
>     assertEqual "test_tSysPrimeNow' 6" (Just 16.204964) tsys


