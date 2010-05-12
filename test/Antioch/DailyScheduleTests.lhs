> module Antioch.DailyScheduleTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Score
> import Antioch.Schedule
> import Antioch.DailySchedule
> import Data.List (zipWith6, sort, find)
> import Data.Maybe
> import Test.HUnit
> import System.Random

> tests = TestList [
>       test_removeBuffer
>     , test_removeBuffer_2
>     , test_runDailySchedule_1
>     , test_runDailySchedule_2
>     , test_runDailySchedule_3
>     ]

> test_removeBuffer = TestCase $ do
>   -- simplest case
>   let result = removeBuffer start dur ps history
>   assertEqual "test_removeBuffer_1" exp result
>   -- now make sure history isn't removed
>   let result = removeBuffer start dur ps history2
>   assertEqual "test_removeBuffer_2" exp2 result
>   -- shift the boundaries a little bit
>   let result = removeBuffer start dur2 ps history2
>   assertEqual "test_removeBuffer_3" exp3 result
>     where
>   start = fromGregorian 2006 6 1 12 0 0
>   dur = 24*60
>   history = []
>   -- 4 hour periods, from start to 24 + 12 hours later
>   times = [(60*4*m) `addMinutes` start | m <- [0 .. 9]]
>   ps = map (\dt -> defaultPeriod {startTime = dt, duration = (4*60)}) times
>   -- we should drop the last four periods
>   rest = (length ps) - 4
>   exp = take rest ps
>   history2 = [last ps]
>   exp2 = exp ++ history2
>   dur2 = 25*60
>   exp3 = (take ((length ps) - 3) ps) ++ history2

> test_removeBuffer_2 = TestCase $ do
>   -- simplest case
>   let result = removeBuffer start dur ps history
>   assertEqual "test_removeBuffer_2" ps result
>     where
>   start = fromGregorian 2006 2 2 0 0 0
>   dur = 24*60
>   ps = [defaultPeriod {startTime = fromGregorian 2006 2 2 12 0 0, duration = 2160}]

>   history = []

Here do the simplest schedule you can handle: one giant period
just make sure it gets cut off properly.

> test_runDailySchedule_1 = TestCase $ do
>   w <- getWeather Nothing
>   -- get one big period
>   --results <- runScoring w [] $ runDailySchedule Pack dt minutes history [s]  
>   results <- runScoring w [] $ dailySchedule' Pack dt minutes history [s]  
>   assertEqual "test_runDailySchedule_1_1" exp results
>   -- nothing should get filtered out
>   let filtered = removeBuffer start minutes results history
>   assertEqual "test_runDailySchedule_1_2" exp filtered
>   -- make sure you work around pre-scheduled ones
>   --results <- runScoring w [] $ runDailySchedule Pack dt minutes history2 [s]  
>   results <- runScoring w [] $ dailySchedule' Pack dt minutes history2 [s]  
>   assertEqual "test_runDailySchedule_1_3" exp2 results
>   -- last one should get filtered out
>   let filtered = removeBuffer start minutes results history
>   assertEqual "test_runDailySchedule_1_4" exp3 filtered
>     where
>   dt = fromGregorian 2006 2 2 12 0 0
>   start = fromGregorian 2006 2 2 12 0 0
>   days = 1
>   minutes = (24*60*days)::Minutes
>   history = []
>   s = getSchedulableSession 
>   times = [(start, 2160)]
>   exp = map (mkPeriod s) times
>   -- for the history test, place pre-scheduled periods at the end & start 
>   times2 = [(start, 60), (fromGregorian 2006 2 3 23 0 0, 60)]
>   history2 = map (mkPeriod s) times2 
>   times3 = [(head times2)
>           , (fromGregorian 2006 2 2 13 0 0, (2160 - 120))
>           , (last times2)]
>   exp2 = map (mkPeriod s) times3
>   exp3 = take 2 exp2

Now, make a number of shorter spaced out periods by 
adjusting max duration and time between.

> test_runDailySchedule_2 = TestCase $ do
>   w <- getWeather Nothing
>   -- get a number of smaller periods
>   results <- runScoring w [] $ dailySchedule' Pack dt minutes history [s]  
>   assertEqual "test_runDailySchedule_2_1" exp results
>   -- some of these should now get removed
>   let filtered = removeBuffer start minutes results history
>   assertEqual "test_runDailySchedule_2_2" exp2 filtered
>     where
>   dt = fromGregorian 2006 2 2 12 0 0
>   start = fromGregorian 2006 2 2 12 0 0
>   days = 1
>   minutes = (24*60*days)::Minutes
>   history = []
>   s' = getSchedulableSession --head ss
>   s = s' {maxDuration = (5*60), timeBetween = (5*60)}
>   times = [(fromGregorian 2006 2 2 13 45 0, 300)
>          , (fromGregorian 2006 2 3  1 30 0, 300)
>          , (fromGregorian 2006 2 3 13 15 0, 300)
>           ]
>   exp = map (mkPeriod s) times
>   exp2 = take 2 exp

> test_runDailySchedule_3 = TestCase $ do
>   w <- getWeather Nothing
>   let ss = concatMap sessions pTestProjects
>   let ps = concatMap periods ss
>   results <- runScoring w [] $ dailySchedule' Pack dt minutes ps ss
>   assertEqual "test_runDailySchedule_3" p (head $ results)
>     where
>       dt = fromGregorian 2006 10 20 4 0 0
>       days = 1
>       minutes = (24*60*days)::Minutes
>       p = (periods . findPSessionByName $ "TestWindowed2") !! 1

Utilities:

> mkPeriod s time = defaultPeriod { session = s
>                                 , startTime = fst time
>                                 , duration = snd time
>                                 , pDuration = snd time
>                                 }

> getSchedulableSession :: Session
> getSchedulableSession = head $ sessions getSchedulableProject

> getSchedulableProject :: Project
> getSchedulableProject = makeProject proj' (100*60) (100*60) [s']
>   where
>     -- simplest session that can be scheduled at anytime
>     -- TBF: hour angle limit fails on this occassionally - shouldn't happen
>     proj' = defaultProject { pAllottedT = 100*60 }
>     s' = defaultSession { sAllottedT = 100*60
>                         , sAllottedS = 100*60
>                         , minDuration = 2*60
>                         , maxDuration = 100*60
>                         , frequency = 2.0
>                         , ra = 0.0 
>                         , dec = 1.5 --1.2217 -- always up
>                          }
