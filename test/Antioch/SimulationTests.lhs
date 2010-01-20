> module Antioch.SimulationTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Schedule
> import Antioch.Simulate
> import Antioch.Statistics (scheduleHonorsFixed)
> import Data.List (sort, find)
> import Data.Maybe
> import Test.HUnit
> import System.Random

> tests = TestList [
>      test_findCanceledPeriods
>    , test_sim_pack
>    , test_sim_pack_starvation
>    , test_sim_pack_starvation2
>    -- , test_sim_pack_around_history TBF: broken
>    -- , test_sim_schd_pack  TBF must be run singly
>    -- , test_sim_schd_pack_around_history TBF broken
>    , test_sim_pack_completion
>    , test_sim_schd_pack_exhaustive_history
>    , test_sim_schedMinDuration
>    -- test_sim_schedMinDuration_backup TBF: broken
>    , test_sim_schedMinDuration_fail_backup
>    , test_sim_schedMinDuration_famine
>    , test_sim_schedMinDuration_starvation
>    , test_sim_timeLeft
>    , test_schedulableSessions
>   ]
>

> test_sim_schedMinDuration = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, t) <- simulate ScheduleMinDuration w rs dt dur int history cnl ss
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
>     expSs = [gb, gb, va, va, tx, tx, gb, wv, gb, lp, cv, cv, tx]
>     dts = [ fromGregorian 2006 2 1 1 30 0
>           , fromGregorian 2006 2 1 3 30 0
>           , fromGregorian 2006 2 1 5 30 0
>           , fromGregorian 2006 2 1 9 30 0
>           , fromGregorian 2006 2 1 13 30 0
>           , fromGregorian 2006 2 1 17 30 0
>           , fromGregorian 2006 2 1 22 15 0
>           , fromGregorian 2006 2 2  0 15 0
>           , fromGregorian 2006 2 2  4 15 0
>           , fromGregorian 2006 2 2  6 15 0
>           , fromGregorian 2006 2 2 10 15 0
>           , fromGregorian 2006 2 2 12 15 0
>           , fromGregorian 2006 2 2 14 15 0 ]
>     durs = [120, 120, 240, 240, 240, 240, 120, 240, 120, 240, 120, 120, 240]
>     scores = replicate 13 0.0
>     exp = zipWith8 Period (repeat 0) expSs dts durs scores dts (repeat False) durs

Test the case where a bady performing TP is replaced with a backup

> test_sim_schedMinDuration_backup = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, c) <- simulate ScheduleMinDuration w rs dt dur int history [] ss
>     assertEqual "SimulationTests_test_sim_schedMinDuration_backup" exp result
>     --assertEqual "SimulationTests_test_sim_schedMinDuration_backup_2" [canceled] c
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 4 6 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
>     ss' = getOpenPSessions
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
>     exp = zipWith8 Period (repeat 0) expSs dts durs scores dts (repeat False) durs
>     canceled = Period 0 gb (fromGregorian 2006 2 5 2 30 0) 120 0.0 (head dts) False 120

Now have the same session fail it's MOC, but there is no backup - make deadtime

> test_sim_schedMinDuration_fail_backup = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, _) <- simulate ScheduleMinDuration w rs dt dur int history [] ss
>     assertEqual "SimulationTests_test_sim_schedMinDuration_fail_backup" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 4 6 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
>     ss = getOpenPSessions
>     expSs = [lp, cv, cv, as, cv]
>     dts = [ fromGregorian 2006 2 4  6  0 0
>           , fromGregorian 2006 2 4 10  0 0
>           , fromGregorian 2006 2 5  3 30 0
>           , fromGregorian 2006 2 5  5 30 0
>           , fromGregorian 2006 2 5 11 30 0]
>     durs = [240, 120, 120, 360, 120]
>     scores = [5.7547455, 3.8890452, 2.928565, 3.9593077, 3.2085283]
>     exp = zipWith8 Period (repeat 0) expSs dts durs scores dts (repeat False) durs

Make sure the simulation can handle running out of sessions to schedule, and
that it does not over allocate periods to a session.

> test_sim_schedMinDuration_starvation = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, c) <- simulate ScheduleMinDuration w rs dt dur int history [] ss
>     assertEqual "SimulationTests_test_sim_schedMinDuration_starvation" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 10
>     int = 60 * 24 * 1
>     history = []
>     p = defaultProject { semester = "06A"
>                        , pAlloted = 240
>                        }
>     s = defaultSession {minDuration = 120
>                       , sAlloted   = 240
>                       , project     = p 
>                        }
>     ss = [s]
>     exp = [Period 0 s (fromGregorian 2006 2 1 16 15 0) 120 0.0 dt False 120
>          , Period 0 s (fromGregorian 2006 2 1 18 15 0) 120 0.0 dt False 120]

Can't simulate anything because the project doesn't have enough time!

> test_sim_schedMinDuration_famine = TestCase $ do
>     w <- getWeather $ Just dt
>     (r, c) <- simulate ScheduleMinDuration w [] dt dur int [] [] [s]
>     assertEqual "SimulationTests_test_sim_schedMinDuration_famine" [] r
>   where
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 10
>     int = 60 * 24 * 1
>     s = defaultSession {minDuration = 120
>                       , sAlloted   = 240
>                       , project     = defaultProject -- not enought time! 
>                        }



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
>   p1 = Period 0 defaultSession dt1 1 0.0 dt1 False 1
>   p2 = Period 0 defaultSession dt2 1 0.0 dt2 False 1
>   p3 = Period 0 defaultSession dt3 1 0.0 dt3 False 1

> test_sim_pack = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, t) <- simulate Pack w rs dt dur int history cnl ss
>     assertEqual "SimulationTests_test_sim_pack" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
>     cnl = []
>     ss = getOpenPSessions
>     expSs = [gb, va, va, tx, tx, wv, gb, lp, cv, tx]
>     dts = [ fromGregorian 2006 2 1  1 30 0
>           , fromGregorian 2006 2 1  4 30 0
>           , fromGregorian 2006 2 1  8 30 0
>           , fromGregorian 2006 2 1 12 30 0
>           , fromGregorian 2006 2 1 17 30 0
>           , fromGregorian 2006 2 1 22 30 0
>           , fromGregorian 2006 2 2  4 30 0
>           , fromGregorian 2006 2 2  7 45 0
>           , fromGregorian 2006 2 2 12  0 0
>           , fromGregorian 2006 2 2 14 15 0 ]
>     durs = [180, 240, 240, 300, 240, 360, 195, 255, 135, 360]
>     scores = replicate 10 0.0
>     exp = zipWith8 Period (repeat 0) expSs dts durs scores dts (repeat False) durs
>     

TBF: this test shows we aren't constraining withing pack: see how the allotted
time exceeds the sessions total time

> test_sim_pack_starvation = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, c) <- simulate Pack w rs dt dur int history [] ss
>     assertEqual "SimulationTests_test_sim_pack_starvation" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 10
>     int = 60 * 24 * 1
>     history = []
>     p = defaultProject {semester = "06A"
>                       , pAlloted = 360
>                        }
>     s = defaultSession {minDuration = 120
>                       , maxDuration = 120
>                       , sAlloted   = 360
>                       , project     = p 
>                        }
>     ss = [s]
>     exp = [Period 0 s (fromGregorian 2006 2 1 17 45 0) 120 0.0 dt False 120
>          , Period 0 s (fromGregorian 2006 2 1 19 45 0) 120 0.0 dt False 120
>          , Period 0 s (fromGregorian 2006 2 1 21 45 0) 120 0.0 dt False 120]

> test_sim_pack_starvation2 = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, c) <- simulate Pack w rs dt dur int history [] ss
>     let negScores = [p | p <- result, pScore p < 0.0]
>     assertEqual "SimulationTests_test_sim_pack_starvation2" [] negScores
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 40
>     int = 60 * 24 * 1
>     history = []
>     -- induce starvation by shortening everybody's time
>     ss = map (\s -> s {sAlloted = 10*60}) getOpenPSessions

TBF: the simulate function currently cannot handle scheduling around 
pre-scheduled periods

> test_sim_pack_around_history = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, t) <- simulate Pack w rs dt dur int history cnl ss
>     assertEqual "SimulationTests_test_sim_pack_1" True (scheduleHonorsFixed history result)
>     assertEqual "SimulationTests_test_sim_pack_2" exp result
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     cnl = []
>     ss = getOpenPSessions
>     fixed1 = Period 0 lp (fromGregorian 2006 2 1 7 30 0) 240 0.0 dt False 240
>     history = [fixed1]
>     --expSs = [gb, va, tx, tx, wv, gb, lp, tx, tx]
>     expSs = [gb, lp, tx, tx, wv, gb, lp, tx, tx]
>     dts = [ fromGregorian 2006 2 1 1 30 0
>           , fromGregorian 2006 2 1 7 30 0
>           , fromGregorian 2006 2 1 11 30 0
>           , fromGregorian 2006 2 1 15 30 0
>           , fromGregorian 2006 2 1 22 30 0
>           , fromGregorian 2006 2 2  4 30 0
>           , fromGregorian 2006 2 2  7 30 0
>           , fromGregorian 2006 2 2 12  0 0
>           , fromGregorian 2006 2 2 16  0 0 ]
>     durs = [360, 240, 240, 360, 360, 180, 270, 240, 270]
>     scores = replicate 9 0.0
>     exp = zipWith8 Period (repeat 0) expSs dts durs scores dts (repeat False) durs
>     

> test_sim_schd_pack = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, t) <- simulateScheduling Pack w rs dt dur int history cnl ss
>     assertEqual "SimulationTests_test_sim_schd_pack_1" True (scheduleHonorsFixed history result)
>     assertEqual "SimulationTests_test_sim_schd_pack_2" exp (take 6 result)
>   where
>     rs  = []
>     -- set it up to be like production 08B beta test scheduling
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 4
>     int = 60 * 24 * 2
>     history = []
>     cnl = []
>     ss = getOpenPSessions
>     expSs = [gb, va, tx, tx, wv, gb]
>     expDts = [fromGregorian 2006 2 1  1 30 0
>             , fromGregorian 2006 2 1  7 15 0
>             , fromGregorian 2006 2 1 11 30 0
>             , fromGregorian 2006 2 1 17 30 0
>             , fromGregorian 2006 2 1 22 15 0
>             , fromGregorian 2006 2 2  4 15 0
>               ]
>     expDurs = [345, 255, 360, 240, 360, 135]
>     exp = zipWith3 mkPeriod expSs expDts expDurs
>     mkPeriod s dt dur = Period 0 s dt dur 0.0 dt False dur

> test_sim_schd_pack_around_history = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, t) <- simulateScheduling Pack w rs dt dur int history1 cnl ss
>     assertEqual "SimulationTests_test_sim_schd_pack_around_history_1" True (scheduleHonorsFixed history1 result)
>     assertEqual "SimulationTests_test_sim_schd_pack_around_history_2" exp1 (take 10 result)
>     (result, t) <- simulateScheduling Pack w rs dt dur int history2 cnl ss
>     assertEqual "SimulationTests_test_sim_schd_pack_3" True (scheduleHonorsFixed history2 result)
>     assertEqual "SimulationTests_test_sim_schd_pack_4" exp2 (take 11 result)
>   where
>     rs  = []
>     -- set it up to be like production 08B beta test scheduling
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 4
>     int = 60 * 24 * 2
>     cnl = []
>     ss = getOpenPSessions
>     ds = defaultSession
>     expSs = [gb, va, tx, wv, gb, lp, cv, tx]
>     expDts = [fromGregorian 2006 2 1  1 30 0
>             , fromGregorian 2006 2 1  7 15 0
>             --, fromGregorian 2006 2 1 11 30 0
>             , fromGregorian 2006 2 1 14 00 0
>             , fromGregorian 2006 2 1 22 15 0
>             , fromGregorian 2006 2 2  4 15 0
>             , fromGregorian 2006 2 2  6 30 0
>             , fromGregorian 2006 2 2 11  0 0
>             , fromGregorian 2006 2 2 13  0 0
>               ]
>     expDurs = [345, 285, 360, 360, 135, 270, 120, 270]
>     exp' = zipWith3 mkPeriod expSs expDts expDurs
>     mkPeriod s dt dur = Period 0 s dt dur 0.0 dt False dur
>     -- outside of the simulation range
>     fixed0 = Period 0 ds {sId = 1000} (fromGregorian 2006 1 30 0 0 0) 60 0.0 dt False 60
>     -- within the simulation range
>     fixed1 = Period 0 ds {sId = 1001} (fromGregorian 2006 2 1 12 0 0) 120 0.0 dt False 120
>     -- w/ in the sim range, and spaning a strategy boundry (midnight)
>     fixed2 = Period 0 ds {sId = 1002} (fromGregorian 2006 2 2 22 0 0) 240 0.0 dt False 240
>     -- outside sim range
>     fixed3 = Period 0 ds {sId = 1003} (fromGregorian 2006 3 1 0 0 0) 60 0.0 dt False 60
>     history1 = [fixed1, fixed2]
>     exp1 = sort $ history1 ++ exp'
>     history2 = [fixed0, fixed1, fixed2, fixed3]
>     exp2 = sort $ (init history2) ++ exp'
>     

Test to make sure that our time accounting isn't screwed up by the precence 
of pre-scheduled periods (history)

> test_sim_schd_pack_exhaustive_history = TestCase $ do
>     w <- getWeather $ Just dt
>     -- first, a test where the history uses up all the time
>     (result, t) <- simulateScheduling Pack w rs dt dur int h1 cnl ss1
>     assertEqual "SimulationTests_test_sim_schd_pack_ex_hist_1" True (scheduleHonorsFixed h1 result)
>     assertEqual "SimulationTests_test_sim_schd_pack_ex_hist_2" h1 result
>     -- now, if history only takes some of the time, make sure 
>     -- that the session's time still gets used up
>     (result, t) <- simulateScheduling Pack w rs dt dur int h2 cnl ss2
>     assertEqual "SimulationTests_test_sim_schd_pack_ex_hist_3" True (scheduleHonorsFixed h2 result)
>     let observedTime = sum $ map duration result
>     assertEqual "SimulationTests_test_sim_schd_pack_ex_hist_4" True (abs (observedTime - (sAlloted s2)) <= (minDuration s2))
>   where
>     rs  = []
>     -- set it up to be like production 08B beta test scheduling
>     dt = fromGregorian 2006 2 1 0 0 0
>     dur = 60 * 24 * 7
>     int = 60 * 24 * 2
>     cnl = []
>     ds = defaultSession
>     -- a period that uses up all the sessions' time!
>     f1 = Period 0 ds {sId = sId cv} dt (sAlloted cv) 0.0 dt False (sAlloted cv)
>     h1 = [f1]
>     -- make sure that this session knows it's used up it's time
>     s1 = cv {periods = h1}
>     ss1 = [s1]
>     -- a period that uses MOST of the sessions' time!
>     f2 = Period 0 ds {sId = sId cv} (dt) (45*60) 0.0 dt False (45*60)
>     h2 = [f2]
>     -- make sure that this session knows it's used up MOST of it's time
>     s2 = cv {periods = h2}
>     ss2 = [s2]


> test_sim_timeLeft = TestCase $ do
>   assertEqual "test_timeLeft_1" True  (hasTimeSchedulable dt1 s1)
>   assertEqual "test_timeLeft_2" True  (hasTimeSchedulable dt1 s2)
>   assertEqual "test_timeLeft_3" False (hasTimeSchedulable dt1 s3)
>   assertEqual "test_timeLeft_4" False (hasTimeSchedulable dt1 s4)
>   assertEqual "test_timeLeft_5" False (hasTimeSchedulable dt2 s6) -- 09A
>   assertEqual "test_timeLeft_6" True  (hasTimeSchedulable dt1 s6) -- 09B
>     where
>       -- vanilla test
>       dt1 = fromGregorian 2009 6 2 0 0 0 -- 09B
>       s1 = defaultSession
>       -- use up some time, but not all ( 3 hrs left )
>       proj = defaultProject { pAlloted = 7 * 60 }
>       s2' = s1 { sAlloted = 7 * 60
>                , minDuration = 2 * 60
>                , project = proj }
>       dt2 = fromGregorian 2009 5 2 0 0 0
>       dt3 = fromGregorian 2009 5 3 0 0 0
>       p1 = defaultPeriod { session = s2'
>                          , startTime = dt2
>                          , duration = 2 * 60
>                          , pTimeBilled = 2 * 60
>                          }
>       p2 = p1 { startTime = dt3 }
>       s2 = makeSession s2' [] [p1,p2] 
>       -- use up some time, but too much ( 1 hr left )
>       dt4 = fromGregorian 2009 5 4 0 0 0
>       p3 = p2 { startTime = dt4 }
>       s3 = makeSession s2' [] [p1,p2,p3] 
>       -- now the session has enough time, but not the project
>       proj2' = proj { pAlloted = 4 * 60 }
>       s4' = s2 { project = proj2' }
>       proj2 = makeProject proj2' (4*60) [s4']
>       s4 = head . sessions $ proj2
>       -- now the session has enought time, depending on the semester
>       proj3' = proj { pAlloted = 6 * 60 
>                     , maxSemesterTime = 2 * 60 }
>       s5' = s1 { sAlloted = 7 * 60
>                , minDuration = 2 * 60 }
>       s5 = makeSession s5' [] [p1]
>       s6' = s5'
>       proj3 = makeProject proj3' (6*60) [s5, s6']
>       s6 = last . sessions $ proj3


> test_sim_pack_completion = TestCase $ do
>   w <- getWeather $ Just dt
>   (r, t) <- simulateScheduling Pack w [] dt dur int [] [] ss1
>   assertEqual "test_sim_pack_completion_1" True  (hasSessions r ss1)
>   (r, t) <- simulateScheduling Pack w [] dt dur int [] [] ss2
>   assertEqual "test_sim_pack_completion_2" True  (hasSessions r (drop 1 ss2))
>   assertEqual "test_sim_pack_completion_3" True  (doesNotHave r (take 1 ss2))
>   (r, t) <- simulateScheduling Pack w [] dt dur int [] [] ss3
>   assertEqual "test_sim_pack_completion_4" True  (hasSessions r (drop 2 ss2))
>   assertEqual "test_sim_pack_completion_5" True  (doesNotHave r (take 2 ss2))
>   (r2, t) <- simulateScheduling Pack w [] dt dur int [] [] ss4
>   assertEqual "test_sim_pack_completion_6" True  (length r2 == 0)
>     where
>       hasSessions ps ss = all (\x -> isJust x == True) $ map (\s -> find (==s) $ map session ps) ss 
>       doesNotHave ps ss = all (\x -> isNothing x == True) $ map (\s -> find (==s) $ map session ps) ss 
>       dt = fromGregorian 2006 1 1 0 0 0
>       dur = 24 * 60
>       int = 12 * 60
>       dt1 = fromGregorian 2005 1 1 0 0 0
>       -- setup a project with sessions - no used time
>       pr = defaultProject { pAlloted = 10 * 60 }
>       s = defaultSession { sAlloted = 5 * 60
>                          , project = pr
>                          , minDuration = 1 * 60
>                          , maxDuration = 5 * 60
>                          }
>       s1' = s { sId = 0, ra = 0.0 } 
>       s2' = s { sId = 1, ra = 8.0 }
>       s3' = s { sId = 2, ra = 16.0 }
>       pr1 = makeProject pr (pAlloted pr) [s1', s2', s3']
>       ss1 = sessions pr1
>       -- now use up the schedulable time of one of them
>       p1 = defaultPeriod { session = s1', startTime = dt, duration = 4*60 + 30, pTimeBilled = 4*60 + 30 }
>       s1'' = makeSession s1' [] [p1] 
>       pr2 = makeProject pr (pAlloted pr) [s1'', s2', s3']
>       ss2 = sessions pr2
>       -- now close one of them
>       s2'' = s2' { sClosed = True }
>       pr3 = makeProject pr (pAlloted pr) [s1'', s2'', s3']
>       ss3 = sessions pr3
>       -- now close the project
>       pr4' = pr3 { pClosed = True }
>       pr4 = makeProject pr4' (pAlloted pr4') [s1'', s2'', s3']
>       ss4 = sessions pr4

> test_schedulableSessions = TestCase $ do
>     let s = findPSessionByName "GB"
>     assertEqual "test_schedulableSessions 1" True (isTypeOpen dt s)
>     let ts = s {sType = Fixed}
>     assertEqual "test_schedulableSessions 2" False (isTypeOpen dt ts)
>     let s = findPSessionByName "LP"
>     let ts = s {sAlloted = 10*60}
>     assertEqual "test_schedulableSessions 3" True (hasTimeSchedulable dt ts)
>     let ts = s {sAlloted = 9*60}
>     assertEqual "test_schedulableSessions 4" False (hasTimeSchedulable dt ts)
>     let s = findPSessionByName "GB"
>     assertEqual "test_schedulableSessions 5" True (isSchedulableSemester dt s)
>     assertEqual "test_schedulableSessions 6" False (isSchedulableSemester early s)
>     assertEqual "test_schedulableSessions 7" True (isSchedulableSemester late s)
>     let s = findPSessionByName "TX"
>     assertEqual "test_schedulableSessions 8" True (isSchedulable dt s)
>     let ts = s {enabled = False}
>     assertEqual "test_schedulableSessions 9" False (isSchedulable dt ts)
>     let ts = s {authorized = False}
>     assertEqual "test_schedulableSessions 10" False (isSchedulable dt ts)
>     let s = findPSessionByName "CV"
>     assertEqual "test_schedulableSessions 11" True (hasObservers dt s)
>     let ts = s {project = defaultProject {observers = []}}
>     assertEqual "test_schedulableSessions 12" False (hasObservers dt ts)
>     assertEqual "test_schedulableSessions 13" 10 (length ss)
>     let sss = scoringSessions dt ss
>     assertEqual "test_schedulableSessions 14" 8 (length sss)
>     --print . length $ sss
>     --assertEqual "test_schedulableSessions 0" True True
>   where
>     ss = getOpenPSessions
>     dt = fromGregorian 2006 2 1  7 15 0
>     early = fromGregorian 2005 11 30  23 45 0
>     late = fromGregorian 2006 6 30  15 30 0

Test Utilities:

> lp = findPSessionByName "LP"
> cv = findPSessionByName "CV"
> as = findPSessionByName "AS"
> gb = findPSessionByName "GB"
> va = findPSessionByName "VA"
> tx = findPSessionByName "TX"
> wv = findPSessionByName "WV"

