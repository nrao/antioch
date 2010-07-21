> module Antioch.SimulationTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Schedule
> import Antioch.Simulate
> import Antioch.Debug
> import Antioch.Statistics (scheduleHonorsFixed)
> import Antioch.Generators (internalConflicts)
> import Data.List (sort, find)
> import Data.Maybe
> import Test.HUnit
> import System.Random

> tests = TestList [ 
>     test_simulateDailySchedule
>   , test_exhaustive_history
>   , test_honor_history
>   , test_updateHistory
>   , test_updateSessions
>                  ]

Attempt to see if the old test_sim_pack still works:

> test_simulateDailySchedule = TestCase $ do
>     w <- getWeather $ Just dt
>     (result, t) <- simulateDailySchedule rs dt packDays simDays history ss True [] []
>     -- TBF: why do we get disagreement w/ old test after the 4th period?
>     assertEqual "SimulationTests_test_sim_pack" (take 4 exp) (take 4 result)
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     simDays = 2
>     packDays = 2
>     history = []
>     cnl = []
>     ss = getOpenPSessions
>     expSs = [gb, va, tx, tx, wv, mh, cv, cv, tx]
>     dts = [ fromGregorian 2006 2 1  1 45 0
>           , fromGregorian 2006 2 1  6 30 0
>           , fromGregorian 2006 2 1 12 30 0
>           , fromGregorian 2006 2 1 17 30 0
>           , fromGregorian 2006 2 1 22 30 0
>           , fromGregorian 2006 2 2  4 30 0
>           , fromGregorian 2006 2 2 10  0 0
>           , fromGregorian 2006 2 2 12  0 0
>           , fromGregorian 2006 2 2 14 15 0
>            ]
>     durs = [285, 360, 300, 240, 360, 330, 120, 135, 360]
>     scores = replicate 10 0.0
>     exp = zipWith9 Period (repeat 0) expSs dts durs scores (repeat Pending) dts (repeat False) durs
>     

Attempt to see if old test still works:
Test to make sure that our time accounting isn't screwed up by the precence 
of pre-scheduled periods (history)

> test_exhaustive_history = TestCase $ do
>     w <- getWeather $ Just dt
>     -- first, a test where the history uses up all the time
>     (result, t) <- simulateDailySchedule rs dt packDays simDays h1 ss1 True [] []
>     assertEqual "test_sim_schd_pack_ex_hist_1" True (scheduleHonorsFixed h1 result)
>     assertEqual "test_sim_schd_pack_ex_hist_2" h1 result
>     -- now, if history only takes some of the time, make sure 
>     -- that the session's time still gets used up
>     (result, t) <- simulateDailySchedule rs dt packDays simDays h2 ss2 True [] []
>     assertEqual "test_sim_schd_pack_ex_hist_3" True (scheduleHonorsFixed h2 result)
>     let observedTime = sum $ map duration result
>     -- This will fail until we use 'updateSession' in simulate
>     assertEqual "test_sim_schd_pack_ex_hist_4" True (abs (observedTime - (sAllottedT s2)) <= (minDuration s2))
>   where
>     rs  = []
>     -- set it up to be like production 08B beta test scheduling
>     dt = fromGregorian 2006 2 1 0 0 0
>     --dur = 60 * 24 * 7
>     --int = 60 * 24 * 2
>     simDays = 7
>     packDays = 2
>     cnl = []
>     ds = defaultSession
>     -- a period that uses up all the sessions' time!
>     f1 = Period 0 ds {sId = sId cv} dt (sAllottedT cv) 0.0 Pending dt False (sAllottedT cv)
>     h1 = [f1]
>     -- make sure that this session knows it's used up it's time
>     s1 = cv {periods = h1}
>     ss1 = [s1]
>     -- a period that uses MOST of the sessions' time!
>     f2 = Period 0 ds {sId = sId cv} (dt) (45*60) 0.0 Pending dt False (45*60)
>     h2 = [f2]
>     -- make sure that this session knows it's used up MOST of it's time
>     s2 = cv {periods = h2}
>     ss2 = [s2]

Here we see if a long simulation honors pre-scheduled periods

> test_honor_history = TestCase $ do
>     -- first, a test where the history uses up all the time
>     (result, t) <- simulateDailySchedule rs dt packDays simDays h1 ss1 True [] []
>     assertEqual "test_honor_history_1" True (scheduleHonorsFixed h1 result)
>     assertEqual "test_honor_history_2" False (internalConflicts result)
>   where
>     rs  = []
>     -- set it up to be like production 08B beta test scheduling
>     dt = fromGregorian 2006 2 1 0 0 0
>     simDays = 7
>     packDays = 2
>     -- the history is made up of a bunch of regularly spaced periods
>     ds = defaultSession {sId = sId cv}
>     mkFixed start = Period 0 ds start (4*60) 0.0 Pending dt False (sAllottedT cv)
>     fixedDts = [ addMinutes (day*24*60) dt | day <- [0 .. simDays]]
>     h1 = map mkFixed fixedDts
>     ss1 = [gb, va, tx, wv, mh, lp]


Here we attempt to schedule only a single high-frequency session - if it does
get on, it has a high chance of being canceled.

> test_cancelations = TestCase $ do
>     (result, tr) <- simulateDailySchedule [] start 2 15 [] ss True [] []
>     let cs = getCanceledPeriods $ tr
>     assertEqual "test_cancelations_1" exp result
>     assertEqual "test_cancelations_2" 15 (length cs)
>   where
>     ss = [va]
>     start = fromGregorian 2006 6 1 0 0 0 -- summer time
>     p1 = defaultPeriod { session = va
>                        , startTime = fromGregorian 2006 6 15 0 30 0
>                        , duration = 255 }
>     p2 = defaultPeriod { session = va
>                        , startTime = fromGregorian 2006 6 15 22 30 0
>                        , duration = 360 }
>     exp = [p1, p2]
>     

> test_updateHistory = TestCase $ do
>     assertEqual "test_updateHistory_1" r1 (updateHistory h1 s1 []) 
>     assertEqual "test_updateHistory_2" r1 (updateHistory h1 s2 []) 
>     assertEqual "test_updateHistory_3" r3 (updateHistory h1 s3 c3) 
>   where
>     mkDts start num = map (\i->(i*dur) `addMinutes'` start) [0 .. (num-1)] 
>     mkPeriod dt = defaultPeriod { startTime = dt, duration = dur }
>     dur = 120 -- two hours
>     -- first test 
>     h1_start = fromGregorian 2006 2 1 0 0 0
>     h1 = map mkPeriod $ mkDts h1_start 5
>     s1_start = fromGregorian 2006 2 1 10 0 0
>     s1 = map mkPeriod $ mkDts s1_start 3
>     dt1 = fromGregorian 2006 2 1 10 0 0
>     r1 = h1 ++ s1
>     -- second test
>     s2 = h1 ++ s1
>     -- third test
>     s3 = (take 4 h1) ++ s1
>     c3 = [last h1]
>     r3 = s3

> test_updateSessions = TestCase $ do
>     -- test initial conditions
>     let psIds = getPeriodIds ss 
>     assertEqual "test_updateSessions_1" [1] psIds
>     -- now test an update w/ out canceled periods
>     let updatedSess = updateSessions ss new_ps []
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_2" [1,2,3] newPsIds
>     -- now test an update *with* canceled periods
>     let updatedSess = updateSessions ss new_ps canceled 
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_3" [2,3] newPsIds
>     -- now test an update *with* canceled periods, but no new periods
>     let updatedSess = updateSessions ss [] canceled 
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_4" [] newPsIds
>   where
>     lp_ps = [defaultPeriod { peId = 1, session = lp }]
>     canceled = lp_ps
>     lp' = makeSession lp [] lp_ps
>     ss = [lp', cv]
>     new_lp_period = defaultPeriod { peId = 2, session = lp }
>     new_cv_period = defaultPeriod { peId = 3, session = cv }
>     new_ps = [new_lp_period, new_cv_period]
>     getPeriodIds sess = sort $ map peId $ concatMap periods sess

Test Utilities:

> lp  = findPSessionByName "LP"
> cv  = findPSessionByName "CV"
> as  = findPSessionByName "AS"
> gb  = findPSessionByName "GB"
> mh  = findPSessionByName "MH"
> va  = findPSessionByName "VA"
> tx  = findPSessionByName "TX"
> wv  = findPSessionByName "WV"
> tw1 = findPSessionByName "TestWindowed1"
> tw2 = findPSessionByName "TestWindowed2"

