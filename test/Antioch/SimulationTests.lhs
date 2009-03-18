> module Antioch.SimulationTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Schedule
> import Antioch.Simulate
> import Data.List (zipWith6)
> import Test.HUnit
> import System.Random

> tests = TestList [
>      test_findCanceledPeriods
>    , test_sim_schedMinDuration
>    -- test_sim_schedMinDuration_backup TBF: broken
>    , test_sim_schedMinDuration_fail_backup
>    , test_sim_schedMinDuration_starvation
>   ]
>

> test_sim_schedMinDuration = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, t) <- simulate scheduleMinDuration w rs dt dur int history cnl ss
>     assertEqual "SimulationTests_test_sim_schedMinDuration" exp result
>     --assertEqual "SimulationTests_test_sim_schedMinDuration_2" canceled c
>   where
>     --canceled = []
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
>     cnl = []
>     ss = getOpenPSessions
>     lp = findPSessionByName "LP"
>     cv = findPSessionByName "CV"
>     gb = findPSessionByName "GB"
>     va = findPSessionByName "VA"
>     tx = findPSessionByName "TX"
>     wv = findPSessionByName "WV"
>     expSs = [gb, gb, gb, va, tx, tx, gb, wv, gb, lp, cv, tx, tx]
>     dts = [ fromGregorian 2006 2 1 1 30 0
>           , fromGregorian 2006 2 1 3 30 0
>           , fromGregorian 2006 2 1 5 30 0
>           , fromGregorian 2006 2 1 7 30 0
>           , fromGregorian 2006 2 1 11 30 0
>           , fromGregorian 2006 2 1 15 30 0
>           , fromGregorian 2006 2 1 22 15 0
>           , fromGregorian 2006 2 2  0 15 0
>           , fromGregorian 2006 2 2  4 15 0
>           , fromGregorian 2006 2 2  6 15 0
>           , fromGregorian 2006 2 2 10 15 0
>           , fromGregorian 2006 2 2 12 15 0
>           , fromGregorian 2006 2 2 16 15 0 ]
>     durs = [120, 120, 120, 240, 240, 240, 120, 240, 120, 240, 120, 240, 240]
>     scores = replicate 13 0.0
>     exp = zipWith6 Period expSs dts durs scores (repeat undefined) (repeat False)

Test the case where a bady performing TP is replaced with a backup

> test_sim_schedMinDuration_backup = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, c) <- simulate scheduleMinDuration w rs dt dur int history [] ss
>     assertEqual "SimulationTests_test_sim_schedMinDuration_backup" exp result
>     --assertEqual "SimulationTests_test_sim_schedMinDuration_backup_2" [canceled] c
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 4 6 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
>     ss' = getOpenPSessions
>     lp = findPSessionByName "LP"
>     cv = findPSessionByName "CV"
>     gb = findPSessionByName "GB"
>     as = findPSessionByName "AS"
>     -- backup sessions aren't above 10 GHz, but in our example, we only
>     -- want this to be scheduled when GB's MOC fails.  The other complication
>     -- is that we want this 'backup' to score low enough so that it doesn't
>     -- get scheduled regularly, but it has a score > 0.0 so that it can
>     -- replace the session GB: thus the GradeC.
>     backup = gb {frequency = 9.0, sName = "backup", sId = 1001, grade = GradeC, backup = True}
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
>     exp = zipWith6 Period expSs dts durs scores (repeat undefined) (repeat False)
>     canceled = Period gb (fromGregorian 2006 2 5 2 30 0) 120 0.0 undefined False

Now have the same session fail it's MOC, but there is no backup - make deadtime

> test_sim_schedMinDuration_fail_backup = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, _) <- simulate scheduleMinDuration w rs dt dur int history [] ss
>     assertEqual "SimulationTests_test_sim_schedMinDuration_fail_backup" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 4 6 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
>     ss = getOpenPSessions
>     lp = findPSessionByName "LP"
>     cv = findPSessionByName "CV"
>     as = findPSessionByName "AS"
>     expSs = [as, cv, cv, as, cv]
>     dts = [ fromGregorian 2006 2 4  6  0 0
>           , fromGregorian 2006 2 5  3 30 0
>           , fromGregorian 2006 2 5  5 30 0
>           , fromGregorian 2006 2 5  7 30 0
>           , fromGregorian 2006 2 6  3 30 0]
>     durs = [360, 120, 120, 360, 120]
>     scores = [5.7547455, 3.8890452, 2.928565, 3.9593077, 3.2085283]
>     exp = zipWith6 Period expSs dts durs scores (repeat undefined) (repeat False)

Make sure the simulation can handle running out of sessions to schedule, and
that it does not over allocate periods to a session.

> test_sim_schedMinDuration_starvation = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, c) <- simulate scheduleMinDuration w rs dt dur int history [] ss
>     assertEqual "SimulationTests_test_sim_schedMinDuration_starvation" exp result
>     --assertEqual "SimulationTests_test_sim_schedMinDuration_starvation2" [] c 
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 10
>     int = 60 * 24 * 1
>     history = []
>     s = defaultSession {minDuration = 120
>                       , totalTime   = 240
>                       , project     = defaultProject {semester = "06A"}
>                        }
>     ss = [s]
>     exp = [Period s (fromGregorian 2006 2 1 16 30 0) 120 0.0 undefined False
>          , Period s (fromGregorian 2006 2 1 18 30 0) 120 0.0 undefined False]

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
>   p1 = Period defaultSession dt1 1 0.0 undefined False
>   p2 = Period defaultSession dt2 1 0.0 undefined False
>   p3 = Period defaultSession dt3 1 0.0 undefined False
