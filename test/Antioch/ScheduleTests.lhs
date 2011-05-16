> module Antioch.ScheduleTests where

> import Antioch.DateTime
> import Antioch.PProjects
> import Antioch.ReceiverTemperatures
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Weather
> import Antioch.Utilities
> import Data.List            
> import Control.Monad.Trans  (lift, liftIO)
> import Control.Monad        (liftM)
> import Test.HUnit

> tests = TestList [
>     test_best
>   , test_constrain
>   , test_schedule_open
>   , test_schedMinDuration
>   , test_schedMinDuration_starvation
>   , test_disobeyLSTExclusion
>   , test_disobeySessionAlloted
>   , test_disobeyTimeBetween
>   , test_disobeyTransit
>   , test_disobeyLowRFI'
>   ]

This test of this strategy should have results that are a subset of the
similar test in SimulationTests.

> test_schedMinDuration = TestCase $ do
>     w <- getWeatherTest $ Just wdt
>     rt <- getReceiverTemperatures
>     result <- runScoring w rs rt $ do
>         sf <- genScore dt ss
>         scheduleMinDuration sf dt dur history ss
>     assertEqual "ScheduleTests_test_schedMinDuration" exp result
>   where
>     rs  = []
>     dt  = fromGregorian 2006 2  1  0 0 0
>     wdt = fromGregorian 2006 1 31 12 0 0
>     dur = 60 * 24 * 1
>     history = []
>     ss' = filter (\s -> (sName s) /= "MH") getOpenPSessions
>     ss = filter timeLeft ss'
>     timeLeft s = ((sAllottedT s) - (sCommittedT s)) > (minDuration s)
>     cv = findPSessionByName "CV"
>     lp = findPSessionByName "LP"
>     va = findPSessionByName "VA"
>     tx = findPSessionByName "TX"
>     expSs = [cv, lp, va, tx, tx]
>     dts = [ fromGregorian 2006 2 1 3 15 0
>           , fromGregorian 2006 2 1 5 15 0
>           , fromGregorian 2006 2 1 9 15 0
>           , fromGregorian 2006 2 1 13 15 0
>           , fromGregorian 2006 2 1 17 15 0]
>     durs = [120, 240, 240, 240, 240]
>     scores = replicate 5 0.0
>     exp = zipWith9 Period (repeat 0) expSs dts durs scores (repeat Pending) dts (repeat False) durs

> test_schedMinDurationWithHistory = TestCase $ do
>     w <- getWeatherTest $ Just wdt
>     rt <- getReceiverTemperatures
>     result <- runScoring w rs rt $ do
>         sf <- genScore dt ss
>         scheduleMinDuration sf dt dur history ss
>     assertEqual "ScheduleTests_test_schedMinDuration_with_history" exp result
>   where
>     rs  = []
>     dt  = fromGregorian 2006 2  1  0 0 0
>     wdt = fromGregorian 2006 1 31 12 0 0
>     dur = 60 * 24 * 1
>     history = [Period 0 tx (fromGregorian 2006 2 1 2 30 0) 120 0.0 Pending dt False 120]
>     ss' = getOpenPSessions
>     ss = filter timeLeft ss'
>     timeLeft s = ((sAllottedT s) - (sCommittedT s)) > (minDuration s)
>     cv = findPSessionByName "CV"
>     lp = findPSessionByName "LP"
>     va = findPSessionByName "VA"
>     tx = findPSessionByName "TX"
>     expSs = [cv, lp, va, tx, tx] 
>     dts = [ fromGregorian 2006 2 1 3 15 0
>           , fromGregorian 2006 2 1 5 15 0
>           , fromGregorian 2006 2 1 9 15 0
>           , fromGregorian 2006 2 1 13 15 0
>           , fromGregorian 2006 2 1 17 15 0]
>     durs = [120, 240, 240, 240, 240]
>     scores = replicate 5 0.0
>     exp = zipWith9 Period (repeat 0) expSs dts durs scores (repeat Pending) dts (repeat False) durs

> test_schedMinDuration_starvation = TestCase $ do
>     rt <- getReceiverTemperatures
>     w <- getWeatherTest $ Just wdt
>     result <- runScoring w rs rt $ do
>         sf <- genScore dt ss
>         scheduleMinDuration sf dt dur history ss
>     assertEqual "ScheduleTests_test_schedMinDuration_starvation" exp result
>   where
>     rs  = []
>     dt  = fromGregorian 2006 2  1  0 0 0
>     wdt = fromGregorian 2006 1 31 12 0 0
>     dur = 60 * 24 * 2
>     history = []
>     s = (findPSessionByName "CV") {minDuration = 120, sAllottedT = 240}
>     ss = [s]
>     exp = [Period 0 s (fromGregorian 2006 2 1 3 15 0) 120 0.0 Pending dt False 120
>          , Period 0 s (fromGregorian 2006 2 1 5 15 0) 120 0.0 Pending dt False 120]

> test_best = TestCase $ do
>       rt <- getReceiverTemperatures
>       w      <- getWeatherTest . Just $ dt
>       (s, score) <- runScoring w [] rt $ do
>           sf <- genScore dt sess
>           best (averageScore sf dt2) sess 
>       assertEqual "ScheduleTests_test_best1" expSession1 s
>       assertEqual "ScheduleTests_test_best2" expScore1 score
>       -- make sure it can handle just one session
>       (s, score) <- runScoring w [] rt $ do
>           sf <- genScore dt sess
>           best (averageScore sf dt2) [(head sess)] 
>       assertEqual "ScheduleTests_test_best3" expSession2 s
>       assertEqual "ScheduleTests_test_best4" expScore2 score
>       -- does not handle no sessions, this produces and error
>       --(s, score) <- runScoring w [] rt $ do
>       --    sf <- genScore dt sess
>       --    (best (averageScore sf dt2) []) 
>       --assertEqual "ScheduleTests_test_best" True True
>   where
>     sess = getOpenPSessions
>     expSession1 = sess !! 1
>     expScore1 = 4.102497
>     expSession2 = sess !! 0
>     expScore2 = 0.0
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
>     assertEqual "ScheduleTests_test_constrain_7" (almostBookedSession:ss) (constrain [Period 0 s' dt 1 0.0 Pending dt False 1] (almostBookedSession:ss))
>     
>   where
>     dt  = fromGregorian 2006 2 1 0 0 0
>     dt2 = fromGregorian 2006 2 1 1 0 0
>     ss = getOpenPSessions
>     cv = findPSessionByName "CV"
>     ssMinusCV = ss \\ [cv]
>     s' = defaultSession {sId = 1000, sAllottedT = 2, minDuration = 1}
>     almostBookedSession = s' {periods = [Period 0 s' dt 1 0.0 Pending dt False 1]}
>     bookedSession = s' {periods = [Period 0 s' dt 1 0.0 Pending dt False 1, Period 0 s' dt2 1 0.0 Pending dt False 1]}
>     p1 = Period 0 cv dt (minDuration cv) 0.0 Pending dt False (minDuration cv)
>     maxNumTPs = (sAllottedT cv) `div` (minDuration cv)
>     maxTPs' = replicate maxNumTPs p1
>     dts = [(hr*60) `addMinutes` dt | hr <- [0..maxNumTPs]] --replicate maxNumTPs dt 
>     maxTPs = zipWith adjustPeriod maxTPs' dts
>       where
>         adjustPeriod p dt = p {startTime = dt}

> test_schedule_open = TestCase $ do
>       w      <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>       rt <- getReceiverTemperatures
>       result <- runScoring w rs rt $ do
>           sf <- genScore dt ss
>           pack sf dt dur history ss
>       assertEqual "test_schedule_open" expected result
>   where
>       rs       = []
>       ss       = concatMap sessions pTestProjects
>       dt       = fromGregorian 2006 9 2 8 0 0
>       dur      = 24*60
>       history  = []
>       expected = [
>           defaultPeriod {
>               session = findPSessionByName "CV"
>             , startTime = fromGregorian 2006 9 2 13 0 0
>             , duration = 195
>             , pDuration = 195
>             }
>         , defaultPeriod {
>               session = findPSessionByName "LP"
>             , startTime = fromGregorian 2006 9 2 16 15 0
>             , duration = 240
>             , pDuration = 240
>           }
>         , defaultPeriod {
>               session = findPSessionByName "AS"
>             , startTime = fromGregorian 2006 9 2 20 15 0
>             , duration = 360
>             , pDuration = 360
>           }
>         ]


> test_disobeyLSTExclusion = TestCase $ do
>   assertEqual "test_disobeyLSTExclusion_1" True $ anyOverlappingLSTs (dt1, dt2) [badLSTrange]  
>   assertEqual "test_disobeyLSTExclusion_2" False $ anyOverlappingLSTs (dt1, dt2) [safeLSTrange]  
>   assertEqual "test_disobeyLSTExclusion_3" False $ disobeyLSTExclusion' p1  
>   assertEqual "test_disobeyLSTExclusion_4" True $ disobeyLSTExclusion' p2  
>   assertEqual "test_disobeyLSTExclusion_5" True $ disobeyLSTExclusion' p3  
>   assertEqual "test_disobeyLSTExclusion_6" False $ anyOverlappingLSTs (dt4, dt5) [lstRange2]  
>   assertEqual "test_disobeyLSTExclusion_7" False $ anyOverlappingLSTs (dt6, dt7) [lstRange3]  
>   assertEqual "test_disobeyLSTExclusion_8" True $ disobeyLSTExclusion' p4  
>   assertEqual "test_disobeyLSTExclusion_9" False $ disobeyLSTExclusion' p5  
>     where
>       dt1 = fromGregorian 2009 6 5 17 0 0
>       dt2 = fromGregorian 2009 6 5 17 3 0
>       dt3 = fromGregorian 2009 6 3 2 0 0
>       dt4 = fromGregorian 2006 3 24 19 45 0 
>       dt5 = fromGregorian 2006 3 24 21 30 0 
>       dt6 = fromGregorian 2006 3 28 17 0  0 
>       dt7 = fromGregorian 2006 3 28 21 15 0 
>       dt8 = fromGregorian 2009 6 5 17 45 0
>       dt9 = fromGregorian 2009 6 5 18 0 0
>       lst1 = utc2lstHours dt1
>       lst2 = utc2lstHours dt2
>       badLSTrange = (lst1 - 1.0, lst2 + 1.0)
>       safeLSTrange = (lst2 - 1.0, lst2 - 0.1)
>       lstRange = (15.0, 21.0)
>       lstRange2 = (4.5722694,9.58269)
>       lstRange3 = (4.4721766,8.396873)
>       reverseLSTRange = (14.0, 9.0)
>       s1 = defaultSession { lstExclude = [lstRange] }
>       p1 = defaultPeriod { session = s1, startTime = dt1, duration = 180, pDuration = 180 }
>       s2 = defaultSession { lstExclude = [badLSTrange] }
>       p2 = defaultPeriod { session = s2, startTime = dt1, duration = 180, pDuration = 180 }
>       s3 = defaultSession { lstExclude = [reverseLSTRange] }
>       p3 = defaultPeriod { session = s3, startTime = dt3, duration = 120, pDuration = 120 }
>       p4 = defaultPeriod { session = s2, startTime = dt8, duration = 180, pDuration = 180 }
>       p5 = defaultPeriod { session = s2, startTime = dt9, duration = 180, pDuration = 180 }

> test_disobeyTransit = TestCase $ do
>   {-
>   print "p7:"
>   let transit = rad2hrs . ra . session $ p7
>   let startLst = utc2lstHours . startTime $ p7
>   let endLst = utc2lstHours . periodEndTime $ p7
>   print startLst
>   print (toGregorian . startTime $ p7)
>   print transit
>   print (toGregorian . periodEndTime $ p7)
>   print endLst
>   -}
>   assertEqual "test_disobeyTransit_1" False $ disobeyTransit' p1  
>   assertEqual "test_disobeyTransit_2" True  $ disobeyTransit' p2  
>   assertEqual "test_disobeyTransit_3" False $ disobeyTransit' p3  
>   assertEqual "test_disobeyTransit_4" True  $ disobeyTransit' p4  
>   assertEqual "test_disobeyTransit_5" False $ disobeyTransit' p5  
>   assertEqual "test_disobeyTransit_6" True  $ disobeyTransit' p6  
>   assertEqual "test_disobeyTransit_7" True  $ disobeyTransit' p7  
>   assertEqual "test_disobeyTransit_8" False $ disobeyTransit' p8  
>   assertEqual "test_disobeyTransit_9" True  $ disobeyTransit' p9  
>     where
>       dt1 = fromGregorian 2009 6 5 17 0 0
>       dt2 = fromGregorian 2009 6 6  8 30 0
>       dt3 = fromGregorian 2006 2 4  0 30 0 
>       dt4 = fromGregorian 2006 2 20 3 30 0 
>       dt5 = fromGregorian 2006 2 20 1 15 0 
>       dt6 = fromGregorian 2006 2 20 1 30 0 
>       p1 = defaultPeriod -- session: transit == Optional
>       s1 = defaultSession { ra = 3.14, transit = Partial }
>       p2 = p1 { session = s1, startTime = dt1, duration = 2 *60 } 
>       p3 = p1 { session = s1, startTime = dt1, duration = 12*60 } 
>       p4 = p1 { session = s1, startTime = dt2, duration = 12*60 } 
>       p5 = p1 { session = s1, startTime = dt2, duration = 20*60 } 
>       s2 = defaultSession { ra = 0.8807137, transit = Partial }
>       p6 = p1 { session = s2, startTime = dt3, duration = 2 *60 } 
>       s3 = defaultSession { ra = 1.6393563 , transit = Partial }
>       p7 = p1 { session = s3, startTime = dt4, duration = 3 *60 } 
>       p8 = p1 { session = s3, startTime = dt5, duration = 3 *60 } 
>       p9 = p1 { session = s3, startTime = dt6, duration = 3 *60 } 

> test_disobeyLowRFI' = TestCase $ do
>   -- Note that rfi day starts at 13:00 UT and ends at 01:00 UT 
>   -- period starts inside RFI day's end
>   let p = testPeriod {session = sNightTime
>                     , startTime = fromGregorian 2008 1 1 0 30 0}
>   assertEqual "test_disobeyLowRFI_1" True (disobeyLowRFI' p)
>   -- period starts only 15 minutes inside RFI day's end
>   let p = testPeriod {session = sNightTime
>                     , startTime = fromGregorian 2008 1 1 0 45 0}
>   assertEqual "test_disobeyLowRFI_2" False (disobeyLowRFI' p)
>   -- period overlaps RFI day's start
>   let p = testPeriod {session = sNightTime
>                     , startTime = fromGregorian 2008 1 1 9 15 0}
>   assertEqual "test_disobeyLowRFI_3" True (disobeyLowRFI' p)
>   -- period ends just at RFI day's start
>   let p = testPeriod {session = sNightTime
>                     , startTime = fromGregorian 2008 1 1 9 0 0}
>   assertEqual "test_disobeyLowRFI_4" False (disobeyLowRFI' p)
>   -- period starts inside RFI day's end but don't care
>   let p = testPeriod {session = sAnyTime
>                     , startTime = fromGregorian 2008 1 1 0 30 0}
>   assertEqual "test_disobeyLowRFI_5" False (disobeyLowRFI' p)
>     where
>       sAnyTime = findPSessionByName "CV"
>       sNightTime = sAnyTime { lowRFI = True }
>       testPeriod = defaultPeriod { duration = 4*60 }

> test_disobeyTimeBetween = TestCase $ do
>   assertEqual "test_disobeyTimeBetween_1" 0 $ length . disobeyTimeBetween $ []
>   assertEqual "test_disobeyTimeBetween_2" 0 $ length . disobeyTimeBetween $ ps1 
>   assertEqual "test_disobeyTimeBetween_3" badTb1 $ disobeyTimeBetween $ ps2
>   assertEqual "test_disobeyTimeBetween_4" 0 $ length . disobeyTimeBetween $ ps3 
>   assertEqual "test_disobeyTimeBetween_5" badTb2 $ disobeyTimeBetween $ ps4
>     where
>       --sps = groupBy sameSession ps
>       sameSession p1 p2 = (session p1) == (session p2)
>       -- set up allowed adjacent periods
>       s1 = defaultSession { sId = 0, timeBetween = 0 }
>       dt1 = fromGregorian 2006 1 1 0 0 0
>       dt2 = fromGregorian 2006 1 1 1 0 0
>       ps1 = map (mkPeriod s1) [dt1, dt2]
>       mkPeriod s dt = defaultPeriod { session = s, startTime = dt, duration = 60, pDuration = 60 }
>       -- now disallow them
>       s2 = defaultSession { sId = 1, timeBetween = 1 * 60 }
>       ps2 = map (mkPeriod s2) [dt1, dt2]
>       badTb1 = [(15, ((ps2!!0), (ps2!!1)))]
>       -- more complex, but allowed
>       dt3 = fromGregorian 2006 1 1 2 0 0
>       dt4 = fromGregorian 2006 1 1 3 0 0
>       dt5 = fromGregorian 2006 1 1 4 0 0
>       dt6 = fromGregorian 2006 1 1 5 0 0
>       ps3_1 = map (mkPeriod s2) [dt1, dt3, dt5]
>       ps3_2 = map (mkPeriod s1) [dt2, dt4, dt6]
>       ps3 = sort $ ps3_1 ++ ps3_2
>       -- now break it
>       ps4_1 = map (mkPeriod s2) [dt1, dt3, dt4]
>       ps4_2 = map (mkPeriod s1) [dt2, dt5, dt6]
>       ps4 = sort $ ps4_1 ++ ps4_2
>       badTb2 = [(15, ((ps4_1!!1), (ps4_1!!2)))]
>       

> test_disobeySessionAlloted = TestCase $ do
>   assertEqual "test_disobeySAlloted_1" [] $ disobeySessionAlloted $ []
>   assertEqual "test_disobeySAlloted_2" [] $ disobeySessionAlloted $ periods s1 
>   assertEqual "test_disobeySAlloted_3" [] $ disobeySessionAlloted $ periods s2 
>   assertEqual "test_disobeySAlloted_4" [s3] $ disobeySessionAlloted $ periods s3 
>     where
>       proj = defaultProject { pAllottedT = 2*60 }   -- 0
>       sess = defaultSession { sAllottedT = 2*60, project = proj }
>       mkPeriod s dt = defaultPeriod { session = s, startTime = dt, pDuration = 60 }
>       dt1 = fromGregorian 2006 1 1 3 0 0
>         -- plenty of time
>       p1 = mkPeriod sess dt1 
>       ps1 = [p1]
>       s1' = makeSession sess [] ps1 
>       pr1 = makeProject proj (pAllottedT proj) (pAllottedS proj) [s1']
>       s1 = head . sessions $ pr1
>       -- use up exactly the alloted time
>       dt2 = fromGregorian 2006 1 1 1 0 0
>       p2 = mkPeriod sess dt2 
>       ps2 = [p1, p2]
>       s2' = makeSession sess [] ps2
>       pr2 = makeProject proj (pAllottedT proj) (pAllottedS proj) [s2']
>       s2 = head . sessions $ pr2
>       -- use too much time
>       dt3 = fromGregorian 2006 1 1 2 0 0
>       p3 = mkPeriod sess dt3 
>       ps3 = [p1, p2, p3]
>       s3' = makeSession sess [] ps3
>       pr3 = makeProject proj (pAllottedT proj) (pAllottedS proj) [s3']
>       s3 = head . sessions $ pr3
