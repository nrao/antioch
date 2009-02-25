> module Antioch.SimulationTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Schedule
> import Antioch.Simulate
> import Data.List (zipWith4)
> import Test.HUnit
> import System.Random

> tests = TestList [
>      test_sim_schedMinDuration
>    , test_sim_schedMinDuration_backup
>    , test_sim_schedMinDuration_fail_backup
>    , test_sim_schedMinDuration_starvation
>   ]
>

> test_sim_schedMinDuration = TestCase $ do
>     w <- getWeather $ Just dt
>     result <- simulate scheduleMinDuration w rs dt dur int history ss
>     print result
>     assertEqual "SimulationTests_test_sim_schedMinDuration" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
>     ss = getOpenPSessions
>     lp = head $ findPSessionByName "LP"
>     cv = head $ findPSessionByName "CV"
>     gb = head $ findPSessionByName "GB"
>     va = head $ findPSessionByName "VA"
>     tx = head $ findPSessionByName "TX"
>     wv = head $ findPSessionByName "WV"
>     expSs = [gb, va, va, tx, tx, wv, gb, gb, lp, cv, tx]
>     dts = [ fromGregorian 2006 2 1 2 30 0
>           , fromGregorian 2006 2 1 4 30 0
>           , fromGregorian 2006 2 1 8 30 0
>           , fromGregorian 2006 2 1 12 30 0
>           , fromGregorian 2006 2 1 16 30 0
>           , fromGregorian 2006 2 1 22 15 0
>           , fromGregorian 2006 2 2  2 15 0
>           , fromGregorian 2006 2 2  4 15 0
>           , fromGregorian 2006 2 2  6 15 0
>           , fromGregorian 2006 2 2 10 15 0
>           , fromGregorian 2006 2 2 15 30 0 ]
>     durs = [120, 240, 240, 240, 240, 240, 120, 120, 240, 120, 240]
>     scores = replicate 11 0.0
>     exp = zipWith4 Period expSs dts durs scores

Test the case where a bady performing TP is replaced with a backup

> test_sim_schedMinDuration_backup = TestCase $ do
>     w <- getWeather $ Just dt
>     result <- simulate scheduleMinDuration w rs dt dur int history ss
>     print result
>     assertEqual "SimulationTests_test_sim_schedMinDuration_backup" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 4 6 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
>     ss' = getOpenPSessions
>     lp = head $ findPSessionByName "LP"
>     cv = head $ findPSessionByName "CV"
>     gb = head $ findPSessionByName "GB"
>     as = head $ findPSessionByName "AS"
>     -- backup sessions aren't above 10 GHz, but in our example, we only
>     -- want this to be scheduled when GB's MOC fails.
>     backup = gb {sName = "backup", frequency = 27.5, backup = True}
>     ss = backup:ss'
>     expSs = [as, backup, lp, lp, gb, cv]
>     dts = [ fromGregorian 2006 2 4 6  0 0
>           , fromGregorian 2006 2 5 2 30 0
>           , fromGregorian 2006 2 5 4 30 0
>           , fromGregorian 2006 2 5 8 30 0
>           , fromGregorian 2006 2 6 1 30 0
>           , fromGregorian 2006 2 6 3 30 0 ]
>     durs = [360, 120, 240, 240, 120, 120]
>     scores = replicate 6 0.0
>     exp = zipWith4 Period expSs dts durs scores

Now have the same session fail it's MOC, but there is no backup - make deadtime
TBF: this is retaining the bad TP instead of replacing it w/ dead time

> test_sim_schedMinDuration_fail_backup = TestCase $ do
>     w <- getWeather $ Just dt
>     result <- simulate scheduleMinDuration w rs dt dur int history ss
>     print result
>     assertEqual "SimulationTests_test_sim_schedMinDuration_fail_backup" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 4 6 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
>     ss = getOpenPSessions
>     lp = head $ findPSessionByName "LP"
>     cv = head $ findPSessionByName "CV"
>     gb = head $ findPSessionByName "GB"
>     as = head $ findPSessionByName "AS"
>     expSs = [as, lp, lp, gb, cv]
>     dts = [ fromGregorian 2006 2 4 6  0 0
>           , fromGregorian 2006 2 5 4 30 0
>           , fromGregorian 2006 2 5 8 30 0
>           , fromGregorian 2006 2 6 1 30 0
>           , fromGregorian 2006 2 6 3 30 0 ]
>     durs = [360, 240, 240, 120, 120]
>     scores = replicate 5 0.0
>     exp = zipWith4 Period expSs dts durs scores
>       where

Make sure the simulation can handle running out of sessions to schedule, and
that it does not over allocate periods to a session.
TBF: this is overallocating periods in the sim's first call to schMinDuration

> test_sim_schedMinDuration_starvation = TestCase $ do
>     w <- getWeather $ Just dt
>     result <- simulate scheduleMinDuration w rs dt dur int history ss
>     print result
>     assertEqual "SimulationTests_test_sim_schedMinDuration_starvation" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 10
>     int = 60 * 24 * 1
>     history = []
>     s = defaultSession {minDuration = 120, totalTime = 240}
>     ss = [s]
>     exp = [Period s (fromGregorian 2006 2 1 16 30 0) 120 0.0
>          , Period s (fromGregorian 2006 2 1 18 30 0) 120 0.0]
