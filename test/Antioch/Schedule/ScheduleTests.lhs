> module Antioch.Schedule.ScheduleTests where

> import Antioch.DateTime
> import Antioch.Schedule.Pack
> import Antioch.PProjects
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Test.HUnit

> tests = TestList [
>     test_schedule_open
>   ]

> test_schedule_open = TestCase $ do
>       w      <- getWeather . Just $ fromGregorian 2006 10 14 9 15 2
>       result <- runScoring w rs (pack sf dt dur history ss)
>       assertEqual "test_schedule_open" expected result
>   where
>       rs       = []
>       ss       = concatMap sessions pTestProjects
>       sf       =  genScore ss
>       dt       = fromGregorian 2006 9 2 8 0 0
>       dur      = 24*60
>       history  = []
>       expected = [
>           defaultPeriod {
>               session = head $ filter (\s -> "CV" == (sName s)) ss
>             , startTime = fromGregorian 2006 9 2 14 30 0
>             , duration = 225
>             }
>         , defaultPeriod {
>               session = head $ filter (\s -> "AS" == (sName s)) ss
>             , startTime = fromGregorian 2006 9 2 18 15 0
>             , duration = 480
>           }
>         ]
>       -- expected = [
>       --     ('CV', datetime(2006, 9, 2, 14, 30), 225, 0)
>       --   , ('AS', datetime(2006, 9, 2, 18, 15), 480, 0)
>       --   ]



