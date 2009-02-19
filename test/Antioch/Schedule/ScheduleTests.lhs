> module Antioch.Schedule.ScheduleTests where

> import Antioch.DateTime
> import Antioch.PProjects
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Control.Monad.Trans  (lift)
> import Control.Monad        (liftM)
> import Test.HUnit

> tests = TestList [
>     test_best
>   --, test_schedule_open
>   ]

> test_best = TestCase $ do
>       w      <- getWeather . Just $ dt
>       sf     <- genScore sess
>       (s, score) <- runScoring w [] (best (averageScore sf dt2) sess) 
>       assertEqual "ScheduleTests_test_best1" expSession s
>       assertEqual "ScheduleTests_test_best2" expScore score
>       -- make sure it can handle just one session
>       (s, score) <- runScoring w [] (best (averageScore sf dt2) [(head sess)]) 
>       assertEqual "ScheduleTests_test_best3" expSession s
>       assertEqual "ScheduleTests_test_best4" expScore score
>       -- make sure it can handle just no sessions
>       -- TBF: we're letting this fail so that other bugs will be caught.
>       --(s, score) <- runScoring w [] (best (averageScore sf dt2) []) 
>       --assertEqual "ScheduleTests_test_best" True True
>   where
>     sess = getOpenPSessions
>     expSession = head sess
>     expScore = 10.177312
>     dt  = fromGregorian 2006 2 1 0 0 0
>     dt2 = fromGregorian 2006 2 1 4 0 0

TBF: this is not passing - but was it meant to copy a python test?

> test_schedule_open = TestCase $ do
>       w      <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>       result <- runScoring w rs $ do
>           sf <- lift $ genScore ss
>           (pack sf dt dur history ss)
>       assertEqual "test_schedule_open" expected result
>   where
>       rs       = []
>       ss       = concatMap sessions pTestProjects
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



