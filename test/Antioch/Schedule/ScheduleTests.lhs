> module Antioch.Schedule.ScheduleTests where

> import Antioch.DateTime
> import Antioch.PProjects
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Data.List            (zipWith, zipWith4, (\\))
> import Control.Monad.Trans  (lift)
> import Control.Monad        (liftM)
> import Test.HUnit

> tests = TestList [
>     test_best
>   , test_constrain
>   , test_schedMinDuration
>   , test_schedMinDuration_starvation
>   ]

This test of this strategy should have results that are a subset of the
similar test in SimulationTests.

> test_schedMinDuration = TestCase $ do
>     w <- getWeather $ Just wdt
>     sf <- genScore ss
>     result <- runScoring w rs $ scheduleMinDuration sf dt dur history ss
>     assertEqual "ScheduleTests_test_schedMinDuration" exp result
>   where
>     rs  = []
>     dt  = fromGregorian 2006 2  1  0 0 0
>     wdt = fromGregorian 2006 1 31 12 0 0
>     dur = 60 * 24 * 1
>     history = []
>     ss' = getOpenPSessions
>     ss = filter timeLeft ss'
>     timeLeft s = ((totalTime s) - (totalUsed s)) > (minDuration s)
>     gb = head $ findPSessionByName "GB"
>     va = head $ findPSessionByName "VA"
>     tx = head $ findPSessionByName "TX"
>     expSs = [gb, va, va, tx, tx] 
>     dts = [ fromGregorian 2006 2 1 2 30 0
>           , fromGregorian 2006 2 1 4 30 0
>           , fromGregorian 2006 2 1 8 30 0
>           , fromGregorian 2006 2 1 12 30 0
>           , fromGregorian 2006 2 1 16 30 0]
>     durs = [120, 240, 240, 240, 240]
>     scores = replicate 5 0.0
>     exp = zipWith4 Period expSs dts durs scores

This test ensures that the scheduleMinDuratin strategy can handle running
out of stuff to schedule, and doesn't over schedule sessions.
TBF: reveils bug.

> test_schedMinDuration_starvation = TestCase $ do
>     w <- getWeather $ Just wdt
>     sf <- genScore ss
>     result <- runScoring w rs $ scheduleMinDuration sf dt dur history ss
>     assertEqual "ScheduleTests_test_schedMinDuration_starvation" exp result
>   where
>     rs  = []
>     dt  = fromGregorian 2006 2  1  0 0 0
>     wdt = fromGregorian 2006 1 31 12 0 0
>     dur = 60 * 24 * 2
>     history = []
>     s = defaultSession {minDuration = 120, totalTime = 240}
>     ss = [s]
>     exp = [Period s (fromGregorian 2006 2 1 16 30 0) 120 0.0
>          , Period s (fromGregorian 2006 2 1 18 30 0) 120 0.0]

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

TBF: constrain has not been fully implemented yet

> test_constrain = TestCase $ do
>     assertEqual "ScheduleTests_test_constrain_1" ss (constrain [] ss)
>     assertEqual "ScheduleTests_test_constrain_2" ss (constrain [p1] ss)
>     -- one away from maxing out this session
>     assertEqual "ScheduleTests_test_constrain_3" ss (constrain (tail maxTPs) ss)
>     -- adding one more period should use up all it's time
>     assertEqual "ScheduleTests_test_constrain_4" ssMinusCV (constrain maxTPs ss)
>     -- the same type of checks, but using the session's periods field
>     assertEqual "ScheduleTests_test_constrain_5" (almostBookedSession:ss) (constrain [] (almostBookedSession:ss))
>     assertEqual "ScheduleTests_test_constrain_6" ss (constrain [] (bookedSession:ss))
>     -- now confuse things by placing identical periods in both the
>     -- periods list, *and* the session's periods
>     assertEqual "ScheduleTests_test_constrain_7" (almostBookedSession:ss) (constrain [Period s' dt 1 0.0] (almostBookedSession:ss))
>     
>   where
>     dt  = fromGregorian 2006 2 1 0 0 0
>     dt2 = fromGregorian 2006 2 1 1 0 0
>     ss = getOpenPSessions
>     cv = head $ findPSessionByName "CV"
>     ssMinusCV = ss \\ [cv]
>     s' = defaultSession {sId = 1000, totalTime = 2, minDuration = 1}
>     almostBookedSession = s' {periods = [Period s' dt 1 0.0]}
>     bookedSession = s' {periods = [Period s' dt 1 0.0, Period s' dt2 1 0.0]}
>     p1 = Period cv dt (minDuration cv) 0.0
>     maxNumTPs = (totalTime cv) `div` (minDuration cv)
>     maxTPs' = replicate maxNumTPs p1
>     dts = [(hr*60) `addMinutes'` dt | hr <- [0..maxNumTPs]] --replicate maxNumTPs dt 
>     maxTPs = zipWith adjustPeriod maxTPs' dts
>       where
>         adjustPeriod p dt = p {startTime = dt}


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



