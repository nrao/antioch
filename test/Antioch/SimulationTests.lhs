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
>     test_simulateDailySchedule
>   , test_exhaustive_history
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
>     dts = [ fromGregorian 2006 2 1  1 30 0
>           , fromGregorian 2006 2 1  6 30 0
>           , fromGregorian 2006 2 1 12 30 0
>           , fromGregorian 2006 2 1 17 30 0
>           , fromGregorian 2006 2 1 22 30 0
>           , fromGregorian 2006 2 2  4 30 0
>           , fromGregorian 2006 2 2 10  0 0
>           , fromGregorian 2006 2 2 12  0 0
>           , fromGregorian 2006 2 2 14 15 0
>            ]
>     durs = [300, 360, 300, 240, 360, 330, 120, 135, 360]
>     scores = replicate 10 0.0
>     exp = zipWith9 Period (repeat 0) expSs dts durs scores (repeat Pending) dts (repeat False) durs
>     

Attempt to see if old test still works:
Test to make sure that our time accounting isn't screwed up by the precence 
of pre-scheduled periods (history)

> test_exhaustive_history = TestCase $ do
>     w <- getWeather $ Just dt
>     -- first, a test where the history uses up all the time
>     --(result, t) <- simulateScheduling Pack w rs dt dur int h1 cnl ss1
>     (result, t) <- simulateDailySchedule rs dt packDays simDays h1 ss1 True [] []
>     assertEqual "SimulationTests_test_sim_schd_pack_ex_hist_1" True (scheduleHonorsFixed h1 result)
>     assertEqual "SimulationTests_test_sim_schd_pack_ex_hist_2" h1 result
>     -- now, if history only takes some of the time, make sure 
>     -- that the session's time still gets used up
>     --(result, t) <- simulateScheduling Pack w rs dt dur int h2 cnl ss2
>     (result, t) <- simulateDailySchedule rs dt packDays simDays h2 ss2 True [] []
>     assertEqual "SimulationTests_test_sim_schd_pack_ex_hist_3" True (scheduleHonorsFixed h2 result)
>     let observedTime = sum $ map duration result
>     -- This will fail until we use 'updateSession' in simulate
>     assertEqual "SimulationTests_test_sim_schd_pack_ex_hist_4" True (abs (observedTime - (sAllottedT s2)) <= (minDuration s2))
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

