> module Antioch.SimulationTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather    (getWeatherTest)
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Simulate
> import Antioch.Debug
> import Antioch.Statistics (scheduleHonorsFixed)
> import Antioch.Generators (internalConflicts)
> import Data.List (sort, find)
> import Data.Maybe
> import Control.OldException
> import Test.HUnit
> import System.Random

> tests = TestList [ 
>     test_simulateDailySchedule
>   , test_simulateDailyScheduleWithWindows
>   , test_exhaustive_history
>   , test_honor_history
>   , test_updateHistory
>   , test_updateSessions
>   , test_findScheduledWindowPeriods
>                  ]

Attempt to see if the old test_sim_pack still works:

> test_simulateDailySchedule = TestCase $ do
>     w <- getWeatherTest $ Just dt
>     (result, t) <- simulateDailySchedule rs dt packDays simDays history ss True True [] []
>     --print $ take 4 $ map duration result
>     --print $ take 4 $ map (toSqlString . startTime) result
>     --print $ take 4 $ map (sName . session) result
>     assertEqual "SimulationTests_test_sim_pack" (take 4 exp) (take 4 result)
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     simDays = 2
>     packDays = 2
>     history = []
>     ss = getOpenPSessions
>     expSs = [gb, cv, va, tx, tx, gb, lp, cv, tx, cv, as]
>     dts = [ fromGregorian 2006 2 1  1 30 0
>           , fromGregorian 2006 2 1  6 15 0
>           , fromGregorian 2006 2 1  8 15 0
>           , fromGregorian 2006 2 1 12 15 0
>           , fromGregorian 2006 2 1 18  0 0
>           , fromGregorian 2006 2 1 22 45 0
>           , fromGregorian 2006 2 2  6 45 0
>           , fromGregorian 2006 2 2 12 45 0
>           , fromGregorian 2006 2 2 14 45 0
>           , fromGregorian 2006 2 3  5  0 0
>           , fromGregorian 2006 2 3 18 15 0
>            ]
>     durs = [285, 120, 240, 345, 240, 480, 360, 120, 285, 195, 480]
>     scores = replicate 10 0.0
>     exp = zipWith9 Period (repeat 0) expSs dts durs scores (repeat Pending) dts (repeat False) durs
>     

> test_simulateDailyScheduleWithWindows = TestCase $ do
>     w <- getWeatherTest $ Just dt1
>     (result, t) <- simulateDailySchedule rs dt1 packDays simDays history ss True False [] []
>     --print "result:"
>     --printList result
>     --print "t:"
>     --printList t
>     --  ***    No competition, expect an earlier period to be scheduled
>     -- Four scheduled periods, first is new on first day of window and
>     -- rest are defaults
>     assertEqual "test_simulateDailyScheduleWithWindows 1" 4 (length result)
>     assertEqual "test_simulateDailyScheduleWithWindows 2" (fromGregorian 2006 9 22 4 0 0) (startTime . head $ result)
>     let (win, chosen, def) = head . getWindowPeriodsFromTrace $ t
>     -- New period causes the chosen flag in the window to become true
>     --printList . map wHasChosen . concat . map windows . map session $ result
>     -- TBF knot tied in result, but not trace -- OK trace does not need it
>     --assertEqual "test_simulateDailyScheduleWithWindows 3" True (wHasChosen win)
>     -- Same new period in the result and in the trace
>     assertEqual "test_simulateDailyScheduleWithWindows 4" (Just . head $ result) chosen
>     -- The returned default period should be the same one as attached
>     -- to the session
>     assertEqual "test_simulateDailyScheduleWithWindows 5" def (head . periods . head . tail $ ss)
>     --  ***    No competition, but the scheduling range includes the
>     --         default period, so no new periods should be scheduled.
>     (result, t) <- simulateDailySchedule rs dt2 packDays simDays history ss True False [] []
>     print "result:"
>     printList result
>     print "t:"
>     printList t
>     -- Four scheduled periods, all the default periods
>     --assertEqual "test_simulateDailyScheduleWithWindows 6" 4 (length result)
>     -- The first one being the first period in session TestWindowed2
>     --assertEqual "test_simulateDailyScheduleWithWindows 7" (fromGregorian 2006 9 28 2 0 0) (startTime . head $ result)
>     let (win, chosen, def) = head . getWindowPeriodsFromTrace $ t
>     -- No new period causes the chosen flag in the window to be untouched
>     assertEqual "test_simulateDailyScheduleWithWindows 8" False (wHasChosen win)
>     -- No chosen period
>     --assertEqual "test_simulateDailyScheduleWithWindows 9" Nothing chosen
>     -- The returned default period should be the same one as attached
>     -- to the session
>     assertEqual "test_simulateDailyScheduleWithWindows 10" def (head . periods . head . tail $ ss)
>     --  ***    No competition, but the scheduling range encompasses
>     --         a  window with a previously chosen period, so no
>     --         new periods should be scheduled.
>     --w <- getWeatherTest $ Just dt3
>   where
>     rs  = []
>     dt1 = fromGregorian 2006 9 20 0 0 0
>     dt2 = fromGregorian 2006 9 21 0 0 0
>     dt3 = fromGregorian 2006 10 15 0 0 0
>     simDays = 2
>     packDays = 2
>     history = concat . map periods $ ss
>     ss = getWindowedPSessions

Attempt to see if old test still works:
Test to make sure that our time accounting isn't screwed up by the precence 
of pre-scheduled periods (history)

> test_exhaustive_history = TestCase $ do
>     w <- getWeatherTest $ Just dt
>     -- first, a test where the history uses up all the time
>     (result, t) <- simulateDailySchedule rs dt packDays simDays h1 ss1 True True [] []
>     assertEqual "test_sim_schd_pack_ex_hist_1" True (scheduleHonorsFixed h1 result)
>     assertEqual "test_sim_schd_pack_ex_hist_2" h1 result
>     -- now, if history only takes some of the time, make sure 
>     -- that the session's time still gets used up
>     (result, t) <- simulateDailySchedule rs dt packDays simDays h2 ss2 True True [] []
>     assertEqual "test_sim_schd_pack_ex_hist_3" True (scheduleHonorsFixed h2 result)
>     let observedTime = sum $ map duration result
>     -- This will fail until we use 'updateSession' in simulate
>     assertEqual "test_sim_schd_pack_ex_hist_4" True (abs (observedTime - (sAllottedT s2)) <= (minDuration s2))
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     simDays = 7
>     packDays = 2
>     ds = defaultSession { frequency = 2.0, receivers = [[Rcvr1_2]], sType = Fixed }
>     -- a period that uses up all the sessions' time (480)
>     f1 = Period 0 ds {sId = sId cv} (fromGregorian 2006 2 4 3 0 0) 480 0.0 Pending dt False 480
>     h1 = [f1]
>     -- make sure that this session knows it's used up it's time
>     s1 = makeSession (cv { sAllottedT = 480, sAllottedS = 480}) [] h1
>     ss1 = [s1]
>
>     -- a period that uses MOST of the sessions' time (375)
>     f2 = Period 0 ds {sId = sId cv} (fromGregorian 2006 2 4 3 0 0) 375 0.0 Pending dt False 375
>     h2 = [f2]
>     -- make sure that this session knows it's used up MOST of it's time
>     s2 = makeSession (cv { sAllottedT = 480, sAllottedS = 480}) [] h1
>     ss2 = [s2]

Here we see if a long simulation honors pre-scheduled periods

> test_honor_history = TestCase $ do
>     -- first, a test where the history uses up all the time
>     (result, t) <- simulateDailySchedule rs dt packDays simDays h1 ss1 True True [] []
>     assertEqual "test_honor_history_1" True (scheduleHonorsFixed h1 result)
>     assertEqual "test_honor_history_2" False (internalConflicts result)
>   where
>     rs  = []
>     -- set it up to be like production 08B beta test scheduling
>     dt = fromGregorian 2006 2 1 0 0 0
>     simDays = 7
>     packDays = 2
>     -- the history is made up of a bunch of regularly spaced periods
>     ds = defaultSession {sId = sId cv, receivers = [[Rcvr1_2]], sType = Fixed}
>     mkFixed start = Period 0 ds start (4*60) 0.0 Pending dt False (sAllottedT cv)
>     fixedDts = [ addMinutes (day*24*60) dt | day <- [0 .. simDays]]
>     h1 = map mkFixed fixedDts
>     ss1 = [gb, va, tx, wv, mh, lp]


Here we attempt to schedule only a single high-frequency session - if it does
get on, it has a high chance of being canceled.

> test_cancelations = TestCase $ do
>     (result, tr) <- simulateDailySchedule [] start 2 15 [] ss True True [] []
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
>     -- test an update w/ out canceled periods
>     let updatedSess = updateSessions ss new_ps [] []
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_2" [1,2,3] newPsIds
>     -- test an update *with* canceled periods
>     let updatedSess = updateSessions ss new_ps canceled [] 
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_3" [2,3] newPsIds
>     -- test an update *with* canceled periods, but no new periods
>     let updatedSess = updateSessions ss [] canceled []
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_4" [] newPsIds
>     -- try a non-empty windows argument
>     let updatedSess = updateSessions (tw1:ss) [chosen] [condemned] [w1]
>     -- get the windowed session from the results
>     let tw1' = head $ filter (==tw1) updatedSess
>     --    session's first period has changed
>     assertEqual "test_updateSessions_5" tw1_newPs (periods tw1')
>     --    session's first chosen flag has changed
>     assertEqual "test_updateSessions_6" [True, False] (map wHasChosen $ windows tw1')
>     --    session's windows have not changed
>     assertEqual "test_updateSessions_7" (windows tw1) (windows tw1')
>     --    session's default periods have not changed
>     assertEqual "test_updateSessions_8" [100, 101] (map wPeriodId $ windows tw1')
>     --    session's first period's is defaulted
>     assertEqual "test_updateSessions_9" [0, 101] (map peId $ periods tw1')
>     --    session's periods are referencing the session
>     assertEqual "test_updateSessions_10" [tw1', tw1'] (map session $ periods tw1')
>     --    session's windows are referencing the session
>     assertEqual "test_updateSessions_11" [tw1', tw1'] (map wSession $ windows tw1')
>   where
>     lp_ps = [defaultPeriod { peId = 1, session = lp }]
>     canceled = lp_ps
>     lp' = makeSession lp [] lp_ps
>     ss = [lp', cv]
>     new_lp_period = defaultPeriod { peId = 2, session = lp }
>     new_cv_period = defaultPeriod { peId = 3, session = cv }
>     new_ps = [new_lp_period, new_cv_period]
>     getPeriodIds sess = sort $ map peId $ concatMap periods sess
>     w1 = (head . windows $ tw1) {wHasChosen = True}
>     condemned = head . periods $ tw1
>     chosen = defaultPeriod {
>                 session = tw1
>               , startTime = fromGregorian 2006 10 2 12 15 0
>               , duration = 4*60
>               , pDuration = 4*60
>                }
>     tw1_newPs = [chosen, last . periods $ tw1]

> --catchIt :: Exception -> Maybe ()
> --catchIt (AssertionFailed "Bad windowed session") = Just ()
> --catchIt _                                        = Nothing

> --handler _ = putStrLn = "assertion failed"

> --test_updateSession' = TestCase $ do
>  --  updateSession' s 
>   --  assertEqual "test_updateSessions_5" tw1_newPs (periods tw1')
> 
> test_findScheduledWindowPeriods = TestCase $ do
>     -- no periods
>     let result = findScheduledWindowPeriods []
>     assertEqual "test_findScheduledWindowPeriods_1" ([],[]) result
>     -- open session period
>     let result = findScheduledWindowPeriods ps_lp
>     assertEqual "test_findScheduledWindowPeriods_2" ([],[]) result
>     -- new period for window
>     let result = findScheduledWindowPeriods [newPs1] 
>     assertEqual "test_findScheduledWindowPeriods_3" ([head ps_tw1],[head ws_tw1]) result
>     -- new periods for windows
>     let result = findScheduledWindowPeriods [newPs1, newPs2] 
>     assertEqual "test_findScheduledWindowPeriods_4" ([head ps_tw1, last ps_tw2],[head ws_tw1, last ws_tw2]) result
>
>     -- Testing for detection of illegal pre-conditions
>     {-
>     let performCall newPeriods testN = do
>         evaluate (findScheduledWindowPeriods newPeriods)
>         assertFailure ("test_findScheduledWindowPeriods_" ++ (show 5))
>     -- no default period
>     --let result = findScheduledWindowPeriods [lonePs1]
>     --assertEqual "test_findScheduledWindowPeriods_5" ([],[]) result
>     handleJust assertions (\_ -> return ()) (performCall [lonePs1] 5)
>     --   old period from old window
>     --let result = findScheduledWindowPeriods ps_tw1
>     --assertEqual "test_findScheduledWindowPeriods_6" ([],[]) result
>     handleJust assertions (\_ -> return ()) (performCall ps_tw1 6)
>     -- new period after default period
>     --let result = findScheduledWindowPeriods [badPs1]
>     --assertEqual "test_findScheduledWindowPeriods_7" ([],[]) result
>     handleJust assertions (\_ -> return ()) (performCall [badPs1] 7)
>     assertEqual "" True True
>     -}
>   where
>     ps_lp = periods lp
>     ps_tw1 = periods tw1
>     ws_tw1 = windows tw1
>     ps_tw2 = periods tw2
>     ws_tw2 = windows tw2
>     newPs1 = defaultPeriod {
>                 session = tw1
>               , startTime = fromGregorian 2006 10 3 17 15 0
>               , duration = 4*60
>               , pDuration = 4*60
>                }
>     newPs2 = defaultPeriod {
>                 session = tw2
>               , startTime = fromGregorian 2006 10 16 10 0 0
>               , duration = 3*60
>               , pDuration = 3*60
>                }
>     -- later than default
>     badPs1 = defaultPeriod {
>                 session = tw1
>               , startTime = fromGregorian 2006 10 5 10 45 0
>               , duration = 4*60
>               , pDuration = 4*60
>                }
>     -- missing default period
>     bad_tw = tw1 { periods = tail . periods $ tw1 }
>     lonePs1 = defaultPeriod {
>                 session = bad_tw
>               , startTime = fromGregorian 2006 10 3 17 15 0
>               , duration = 4*60
>               , pDuration = 4*60
>                }


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

TBF try:

Prelude Data.IORef> z <- newIORef (\x -> x)
Prelude Data.IORef> y <- newIORef (\x -> x)
Prelude Data.IORef> z == z
True
Prelude Data.IORef> z == y
False

http://stackoverflow.com/questions/1717553/pointer-equality-in-haskell

> isSessionKnotted :: Session -> Bool
> isSessionKnotted s = all (\f -> f s) [pknots]
>   where
>     pknots s = all (pknot s) [0 .. ((length . periods $ s) - 1)]
>     pknot s i = s == (session ((periods s) !! i))
>     -- wknots, etc

