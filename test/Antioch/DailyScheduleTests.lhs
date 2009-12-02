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
>       test_overlap
>     , test_removeBuffer
>     , test_runDailySchedule_1
>     , test_runDailySchedule_2
>     ]

> test_overlap = TestCase $ do
>   assertEqual "test_overlap_1" True  (overlap dt1 dur1 p1)
>   assertEqual "test_overlap_2" True  (overlap dt1 dur2 p1)
>   assertEqual "test_overlap_3" True  (overlap dt1 dur1 p2)
>   assertEqual "test_overlap_4" False (overlap dt2 dur1 p1)
>     where
>   dt1 = fromGregorian 2006 6 1 12 0 0
>   dur1 = 4*60
>   p1 = defaultPeriod {startTime = dt1, duration = dur1}
>   dur2 = 6*60
>   p2 = defaultPeriod {startTime = dt1, duration = dur2}
>   dt2 = fromGregorian 2006 6 1 20 0 0
>   
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
>   results <- runScoring w [] $ runDailySchedule Pack dt days history [s]  
>   assertEqual "test_runDailySchedule_1_1" exp results
>   -- nothing should get filtered out
>   let filtered = removeBuffer start (days*24*60) results history
>   assertEqual "test_runDailySchedule_1_2" exp filtered
>     where
>   dt = fromGregorian 2006 2 2 0 0 0
>   start = fromGregorian 2006 2 2 12 0 0
>   days = 1
>   history = []
>   s = getSchedulableSession 
>   times = [(start, 2160)]
>   exp = map (mkPeriod s) times

Now, make a number of shorter spaced out periods by 
adjusting max duration and time between.

> test_runDailySchedule_2 = TestCase $ do
>   w <- getWeather Nothing
>   -- get a number of smaller periods
>   results <- runScoring w [] $ runDailySchedule Pack dt days history [s]  
>   assertEqual "test_runDailySchedule_2_1" exp results
>   -- some of these should now get removed
>   let filtered = removeBuffer start (days*24*60) results history
>   assertEqual "test_runDailySchedule_2_2" exp2 filtered
>     where
>   dt = fromGregorian 2006 2 2 0 0 0
>   start = fromGregorian 2006 2 2 12 0 0
>   days = 1
>   history = []
>   s' = getSchedulableSession --head ss
>   s = s' {maxDuration = (5*60), timeBetween = (5*60)}
>   times = [(fromGregorian 2006 2 2 13 45 0, 300)
>          , (fromGregorian 2006 2 3  1 30 0, 300)
>          , (fromGregorian 2006 2 3 13 15 0, 300)
>           ]
>   exp = map (mkPeriod s) times
>   exp2 = take 2 exp



Utilities:

> mkPeriod s time = defaultPeriod { session = s
>                                 , startTime = fst time
>                                 , duration = snd time
>                                 }

> getSchedulableSession :: Session
> getSchedulableSession = head $ sessions getSchedulableProject

> getSchedulableProject :: Project
> getSchedulableProject = makeProject proj' (100*60) [s']
>   where
>     -- simplest session that can be scheduled at anytime
>     -- TBF: hour angle limit fails on this occassionally - shouldn't happen
>     proj' = defaultProject { pAlloted = 100*60 }
>     s' = defaultSession { sAlloted = 100*60
>                      , minDuration = 2*60
>                      , maxDuration = 100*60
>                      , frequency = 2.0
>                      , ra = 0.0 
>                      , dec = 1.5 --1.2217 -- always up
>                       }
