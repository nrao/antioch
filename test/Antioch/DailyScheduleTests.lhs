> module Antioch.DailyScheduleTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.DailySchedule
> import Data.List (zipWith6, sort, find)
> import Data.Maybe
> import Test.HUnit
> import System.Random

> tests = TestList [
>       test_overlap
>     , test_removeBuffer
>     --, test_runDailySchedule
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


