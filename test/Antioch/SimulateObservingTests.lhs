> module Antioch.SimulateObservingTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.SimulateObserving
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.ReceiverTemperatures
> import Antioch.Weather
> import Data.List (sort, find)
> import Data.Maybe
> import Test.HUnit
> import System.Random
> import Control.Monad.Writer
> import Control.Monad.RWS.Strict

> tests = TestList [
>      test_findCanceledPeriods
>    , test_inCancelRange
>    , test_scheduleBackup
>    , test_replaceWithBackup
>                  ]

> test_replaceWithBackup = TestCase $ do
>   w <- getWeatherTest . Just $ dt
>   rt <- getReceiverTemperatures
>   mp <- replaceWithBackupTest w rt ss p 
>   assertEqual "test_replaceWithBackup_1" Nothing mp
>   w2 <- getWeatherTest . Just $ dt2
>   mp <- replaceWithBackupTest w rt ss p2 
>   assertEqual "test_replaceWithBackup_2" (Just backupP2) mp
>   w3 <- getWeatherTest . Just $ dt3
>   mp <- replaceWithBackupTest w rt ss p3 
>   assertEqual "test_replaceWithBackup_3" (Just backupP3) mp
>     where
>   dt = fromGregorian 2006 2 1 0 0 0
>   dt2 = fromGregorian 2006 5 1 0 0 0
>   dt3 = fromGregorian 2006 2 1 6 0 0
>   dur = (2*24*60)
>   names = ["GB","CV","LP","TX","VA","WV","AS"]
>   ss' = concatMap (\name -> findPSessionsByName name) names
>   ss = map (\s -> s { backup = True, minDuration = 30, maxDuration = 120 }) ss'
>   s = head ss
>   p = defaultPeriod { session = s
>                     , startTime = dt
>                     , duration = 60 }
>   p2 = p { startTime = dt2 }
>   p3 = p { startTime = dt3 }
>   backupP2 = p2 { session = (ss!!2) } 
>   backupP3 = p3 { session = (ss!!2) } 
>   replaceWithBackupTest w rt ss p = runScoring w [] rt $ do
>       sf <- genScore (startTime p) ss
>       -- replaceWithBackup gets called from scheduleBackup, which
>       -- resets the weather to use:
>       let wDt = addMinutes' (-60) (startTime p)
>       w' <- liftIO $ newWeather w (Just wDt)
>       local (\env -> env { envWeather = w', envMeasuredWind = True}) $ replaceWithBackup Pack sf ss p   
> 

> test_scheduleBackup = TestCase $ do
>   w <- getWeatherTest . Just $ dt
>   rt <- getReceiverTemperatures
>   mp <- scheduleBackupTest w rt ss p dt dur
>   assertEqual "test_scheduleBackup_1" Nothing mp
>   -- now try 3 months later
>   w2 <- getWeatherTest . Just $ dt2
>   mp <- scheduleBackupTest w2 rt ss p2 dt2 dur
>   assertEqual "test_scheduleBackup_2" (Just backupP2) mp
>   -- now try it such that the oringal period is NOT canceled
>   w3 <- getWeatherTest . Just $ dt3
>   mp <- scheduleBackupTest w3 rt ss pNotCanceled dt3 dur
>   assertEqual "test_scheduleBackup_3" (Just pNotCanceled) mp
>     where
>   dt = fromGregorian 2006 2 1 0 0 0
>   dt2 = fromGregorian 2006 5 1 0 0 0
>   dt3 = fromGregorian 2006 2 2 6 0 0
>   dur = (2*24*60)
>   names = ["GB","CV","LP","TX","VA","WV","AS"]
>   ss' = concatMap (\name -> findPSessionsByName name) names
>   ss = map (\s -> s { backup = True, minDuration = 30, maxDuration = 120 }) ss'
>   s = head ss
>   p = defaultPeriod { session = s
>                     , startTime = dt
>                     , duration = 60 }
>   p2 = p { startTime = dt2 }
>   backupP2 = p2 { session = (ss!!2) } 
>   pNotCanceled = defaultPeriod { session = last ss
>                                , startTime = dt3
>                                , duration = 60 }
>   scheduleBackupTest w rt ss p dt dur = runScoring w [] rt $ do
>       sf <- genScore dt ss
>       scheduleBackup sf Pack ss p dt dur   
>    

> test_findCanceledPeriods = TestCase $ do
>   assertEqual "SimulationTests_test_findCanceledPeriods1" [] $ findCanceledPeriods [] []
>   assertEqual "SimulationTests_test_findCanceledPeriods2" [] $ findCanceledPeriods [p1] [p1]
>   assertEqual "SimulationTests_test_findCanceledPeriods3" [] $ findCanceledPeriods [p1,p2] [p1,p2]
>   assertEqual "SimulationTests_test_findCanceledPeriods4" [p2] $ findCanceledPeriods [p1,p2] [p1]
>   assertEqual "SimulationTests_test_findCanceledPeriods5" [p1] $ findCanceledPeriods [p1,p2] [p2]
>   assertEqual "SimulationTests_test_findCanceledPeriods6" [p2] $ findCanceledPeriods [p1,p2] [p1,p3]
>     where
>   dt1 = fromGregorian 2006 2 1 0 0 0
>   dt2 = fromGregorian 2006 2 1 1 0 0
>   dt3 = fromGregorian 2006 2 1 2 0 0
>   p1 = Period 0 defaultSession dt1 1 0.0 Pending dt1 False 1
>   p2 = Period 0 defaultSession dt2 1 0.0 Pending dt2 False 1
>   p3 = Period 0 defaultSession dt3 1 0.0 Pending dt3 False 1

> test_inCancelRange = TestCase $ do
>     assertEqual "test_inCancelRange_1" True  (inCancelRange p dt1 dur)
>     assertEqual "test_inCancelRange_2" True  (inCancelRange p dt2 dur)
>     assertEqual "test_inCancelRange_3" False (inCancelRange p dt3 dur)
>     assertEqual "test_inCancelRange_4" False (inCancelRange p dt4 dur)
>     assertEqual "test_inCancelRange_5" False (inCancelRange p dt5 dur)
>   where
>     pDt = fromGregorian 2006 1 1 12 0 0
>     p = defaultPeriod { startTime = pDt, duration = 60 }
>     dur = 24 * 60
>     dt1 = fromGregorian 2006 1 1 0 0 0
>     dt2 = fromGregorian 2006 1 1 12 30 0
>     dt3 = fromGregorian 2005 12 31 12 30 0
>     dt4 = fromGregorian 2004 1 1 0 0 0
>     dt5 = fromGregorian 2008 1 1 0 0 0


