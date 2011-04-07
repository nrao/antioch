> module Antioch.FilterTests where

> import Antioch.DateTime
> import Antioch.Types
> --import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> --import Antioch.Schedule
> import Antioch.Filters
> --import Antioch.Statistics (scheduleHonorsFixed)
> import Data.List (sort, find)
> import Data.Maybe
> import Test.HUnit
> import System.Random

> tests = TestList [
>      test_sim_timeLeft
>    , test_schedulableSessions
>    , test_clearWindowedTimeBilled
>    , test_isSchedulableType
>    , test_activeWindows
>    , test_adjustWindowSessionDuration
>    , test_projectBlackedOut
>    , test_filterDisabledPeriods
>   ]

> test_projectBlackedOut = TestCase $ do
>   -- no blackouts
>   assertEqual "test_projectBlackedOut_1" True (pb dt dur s)
>   -- dur not covered by blackouts
>   assertEqual "test_projectBlackedOut_2" True (pb dt dur s2)
>   -- dur covered by blackouts
>   assertEqual "test_projectBlackedOut_3" False (pb dt dur2 s2)
>   -- dur not covered by blackouts
>   assertEqual "test_projectBlackedOut_4" True (pb dt dur s3)
>   -- dur not covered by blackouts at all
>   assertEqual "test_projectBlackedOut_5" True (pb dt2 dur2 s3)
>     where
>       pb = projectNotBlackedOut
>       dt  = fromGregorian 2006 2 1  0 0 0
>       dur = 3*24*60 -- 3 days ~scheduling range
>       dur2 = 1 -- *23*60 
>       dt2 = fromGregorian 2006 2 7  0 0 0
>       dt3 = fromGregorian 2006 2 11 0 0 0
>       s   = defaultSession
>       p   = defaultProject { pBlackouts = bs }
>       s2  = defaultSession { project = p }
>       bs  = [(fromGregorian 2006 1 31 0 0 0, fromGregorian 2006 2 2 0 0 0)]
>       bs2 = [(fromGregorian 2006 2 10 0 0 0, fromGregorian 2006 2 12 0 0 0)]
>       p2  = defaultProject { pBlackouts = bs ++ bs2 }
>       s3  = defaultSession { project = p2}

> getTestPeriods :: [Period]
> getTestPeriods = [p1, p2, p3]
>     where
>       mproj = defaultProject { pName = "Maintenance" }
>       proj  = defaultProject { pName = "test project" }
>       msess = defaultSession { project = mproj, oType = Maintenance}
>       sess  = defaultSession { project = proj, oType = SpectralLine, enabled = False}
>       p1    = defaultPeriod { session = sess
>                             , startTime = fromGregorian 2011 4 1 0 0 0
>                             , duration  = 2 * 60
>                             , pDuration = 2 * 60
>                              }
>       p2    = defaultPeriod { session = msess
>                             , startTime = fromGregorian 2011 4 1 2 0 0
>                             , duration  = 8 * 60
>                             , pDuration = 8 * 60
>                              }
>       p3    = defaultPeriod { session = sess
>                             , startTime = fromGregorian 2011 4 1 10 0 0
>                             , duration  = 2 * 60
>                             , pDuration = 2 * 60
>                              }

> test_filterMaintenancePeriods = TestCase $ do
>   result <- filterMaintenancePeriods [p1, p2, p3]
>   assertEqual "test_filterMaintenancePeriods" [p1, p3] result
>     where
>       (p1: p2: p3: []) = getTestPeriods

> test_filterDisabledPeriods = TestCase $ do
>   result <- filterDisabledPeriods [p1, p2, p3]
>   assertEqual "test_filterDisabledPeriod" [p2] result
>     where
>       (p1: p2: p3: []) = getTestPeriods

> test_sim_timeLeft = TestCase $ do
>   -- dt1 => 09B, dt* => 09A
>   assertEqual "test_timeLeft_1" False (hasTimeSchedulable dt1 undefined s1)
>   assertEqual "test_timeLeft_2" True  (hasTimeSchedulable dt1 undefined s2)
>   assertEqual "test_timeLeft_3" False  (hasTimeSchedulable dt1 undefined s3)
>   assertEqual "test_timeLeft_4" False (hasTimeSchedulable dt1 undefined s4)
>   assertEqual "test_timeLeft_5" True (hasTimeSchedulable dt2 undefined s6)
>   assertEqual "test_timeLeft_6" True  (hasTimeSchedulable dt1 undefined s6)
>     where
>       -- vanilla test
>       dt1 = fromGregorian 2009 6 2 0 0 0 -- 09B
>       s1 = defaultSession
>       -- use up some time, but not all ( 3 hrs left )
>       proj = defaultProject { pAllottedT = 7*60, pAllottedS = 7*60 }
>       s2' = s1 { sAllottedT = 7*60
>                , sAllottedS = 7*60
>                , minDuration = 2 * 60
>                , project = proj }
>       dt2 = fromGregorian 2009 5 2 0 0 0
>       dt3 = fromGregorian 2009 5 3 0 0 0
>       p1 = defaultPeriod { session = s2'
>                          , startTime = dt2
>                          , duration = 2 * 60
>                          , pDuration = 2 * 60
>                          }
>       p2 = p1 { startTime = dt3 }
>       s2 = makeSession s2' [] [p1,p2] 
>       -- use up some time, but too much ( 1 hr left )
>       dt4 = fromGregorian 2009 5 4 0 0 0
>       p3 = p2 { startTime = dt4 }
>       s3 = makeSession s2' [] [p1,p2,p3] 
>       -- now the session has enough time, but not the project
>       proj2' = proj { pAllottedT = 4*60, pAllottedS = 1*60 }
>       s4' = s2 { project = proj2' }
>       proj2 = makeProject proj2' (4*60) (1*60) [s4']
>       s4 = head . sessions $ proj2
>       -- now the session has enought time, depending on the semester
>       proj3' = proj { pAllottedT = 6 * 60 
>                     , pAllottedS = 2 * 60 }
>       s5' = s1 { sAllottedT = 7 * 60
>                , sAllottedS = 7 * 60
>                , minDuration = 2 * 60 }
>       s5 = makeSession s5' [] [p1]
>       s6' = s5'
>       proj3 = makeProject proj3' (6*60) (2*60) [s5, s6']
>       s6 = last . sessions $ proj3

> test_schedulableSessions = TestCase $ do
>     let s = findPSessionByName "GB"
>     assertEqual "test_schedulableSessions 1" True (isTypeOpen dt undefined s)
>     let ts = s {sType = Fixed}
>     assertEqual "test_schedulableSessions 2" False (isTypeOpen dt undefined ts)
>     let s = findPSessionByName "LP"
>     let ts = s {sAllottedT = 10*60, sAllottedS = 10*60}
>     assertEqual "test_schedulableSessions 3" True (hasTimeSchedulable dt undefined ts)
>     let ts = s {sAllottedT = 9*60, sAllottedS = 9*60}
>     assertEqual "test_schedulableSessions 4" False (hasTimeSchedulable dt undefined ts)
>     let s = findPSessionByName "GB"
>     assertEqual "test_schedulableSessions 5" True (isSchedulableSemester dt undefined s)
>     assertEqual "test_schedulableSessions 6" False (isSchedulableSemester early undefined s)
>     assertEqual "test_schedulableSessions 7" True (isSchedulableSemester late undefined s)
>     let s = findPSessionByName "TX"
>     assertEqual "test_schedulableSessions 8" True (isApproved dt undefined s)
>     let ts = s {enabled = False}
>     assertEqual "test_schedulableSessions 9" False (isApproved dt undefined ts)
>     let ts = s {authorized = False}
>     assertEqual "test_schedulableSessions 10" False (isApproved dt undefined ts)
>     let s = findPSessionByName "CV"
>     assertEqual "test_schedulableSessions 11" True (hasObservers dt undefined s)
>     let ts = s {project = defaultProject {observers = []}}
>     assertEqual "test_schedulableSessions 12" False (hasObservers dt undefined ts)
>     assertEqual "test_schedulableSessions 13" 10 (length ss)
>     let sss = scoringSessions dt undefined ss
>     assertEqual "test_schedulableSessions 14" 10 (length sss)
>     --print . length $ sss
>   where
>     ss = getOpenPSessions
>     dt = fromGregorian 2006 10 1  7 15 0
>     dt1 = fromGregorian 2006 10 1  7 15 0
>     early = fromGregorian 2005 11 30  23 45 0
>     late = fromGregorian 2006 6 30  15 30 0

> test_clearWindowedTimeBilled = TestCase $ do
>     let s = tw2
>     let s' = clearWindowedTimeBilled s
>     -- because pDuration is not checked in session equivalence
>     assertEqual "test_clearWindowedTimeBilled 1" s s'
>     --assertEqual "test_clearWindowedTimeBilled 2" 180 (pDuration . fromJust . wPeriod . head . windows $ s)
>     assertEqual "test_clearWindowedTimeBilled 3" 0 (pDuration . fromJust . wPeriod . head . windows $ s')
>     -- should be nop
>     let cv' = clearWindowedTimeBilled cv
>     assertEqual "test_clearWindowedTimeBilled 4" cv cv'

> test_isSchedulableType = TestCase $ do
>     -- session is Open, who cares about windows?
>     assertEqual "test_isSchedulableType 1" True  (isSchedulableType undefined undefined cv)
>     -- session is Windowed, has no windows inside the scheduling range
>     -- but has a chosen period
>     assertEqual "test_isSchedulableType 2" False (isSchedulableType oct (24*60) tw2)
>     -- session is Windowed with a window inside the scheduling range,
>     -- and is complete
>     assertEqual "test_isSchedulableType 3" False  (isSchedulableType oct (6*24*60) tw2)
>     -- session is Windowed with a window inside the scheduling range,
>     -- but with the window's period also in the scheduling range
>     -- and is complete
>     assertEqual "test_isSchedulableType 4" False (isSchedulableType oct (8*24*60) tw2)
>     -- session is Windowed, but no windows inside the scheduling range
>     assertEqual "test_isSchedulableType 5" False (isSchedulableType sep (2*24*60) tw2)
>     -- session is Windowed with a window inside the scheduling range
>     -- but no default period win the scheduling range
>     assertEqual "test_isSchedulableType 6" True  (isSchedulableType sep (5*24*60) tw2)
>     -- session is Windowed with a window inside the scheduling range,
>     -- but with the window's default period also in the scheduling range
>     assertEqual "test_isSchedulableType 7" False (isSchedulableType sep (9*24*60) tw2)
>       where
>         sep = fromGregorian 2006  9 20  0 0 0 -- incomplete window
>         oct = fromGregorian 2006 10 13  0 0 0 --   complete window

> test_stuff = TestCase $ do
>     print ""
>     print win1s
>     assertEqual "test_activeWindows_1" True True
>       where
>         dt1 = fromGregorian 2006  9 29  0 0 0
>         dt2 = fromGregorian 2006  9 20  0 0 0
>         win1s = windows tw1
>         win2s = windows tw2

> test_activeWindows = TestCase $ do
>     -- no overlap
>     let wins' = activeWindows dt1 (2*24*60) win1s
>     assertEqual "test_activeWindows_1" [] wins'
>     -- some overlap
>     let wins' = activeWindows dt1 (4*24*60) win1s
>     assertEqual "test_activeWindows_2" [head win1s] wins'
>     -- too much overlap
>     let wins' = activeWindows dt1 (8*24*60) win1s
>     assertEqual "test_activeWindows_3" [] wins'
>     -- overlap into second window
>     let wins' = activeWindows dt1 (26*24*60) win1s
>     assertEqual "test_activeWindows_4" [head . tail $ win1s] wins'
>     -- overlap into second window's default
>     let wins' = activeWindows dt1 (31*24*60) win1s
>     assertEqual "test_activeWindows_5" [] wins'
>     -- no overlap
>     let wins' = activeWindows dt2 (2*24*60) win2s
>     assertEqual "test_activeWindows_6" [] wins'
>     -- some overlap
>     let wins' = activeWindows dt2 (8*24*60) win2s
>     assertEqual "test_activeWindows_7" [head win2s] wins'
>     -- too much overlap
>     let wins' = activeWindows dt2 (10*24*60) win2s
>     assertEqual "test_activeWindows_8" [] wins'
>     -- full overlap, but first covers the default and second
>     -- is completed
>     let wins' = activeWindows dt2 (60*24*60) win2s
>     assertEqual "test_activeWindows_9" [] wins'
>       where
>         dt1 = fromGregorian 2006  9 29  0 0 0
>         dt2 = fromGregorian 2006  9 20  0 0 0
>         win1s = windows tw1
>         win2s = windows tw2

> test_adjustWindowSessionDuration = TestCase $ do
>     -- no active windows, no adjustment
>     let s = adjustWindowSessionDuration dt1 (2*24*60) tw
>     assertEqual "test_adjustWindowSessionDuration_1" tw s
>     assertEqual "test_adjustWindowSessionDuration_2" tt (minDuration s)
>     assertEqual "test_adjustWindowSessionDuration_3" tt (maxDuration s)
>     assertEqual "test_adjustWindowSessionDuration_3" at (sAllottedS s)
>     assertEqual "test_adjustWindowSessionDuration_3" at (sAllottedT s)
>     -- first window is active, causes an adjustment
>     let s = adjustWindowSessionDuration dt1 (5*24*60) tw
>     assertEqual "test_adjustWindowSessionDuration_4" ntt (minDuration s)
>     assertEqual "test_adjustWindowSessionDuration_5" ntt (maxDuration s)
>     assertEqual "test_adjustWindowSessionDuration_5" ntt (sAllottedS s)
>     assertEqual "test_adjustWindowSessionDuration_5" ntt (sAllottedT s)
>     -- include default, first window is inactive, no adjustment
>     let s = adjustWindowSessionDuration dt1 (6*24*60) tw
>     assertEqual "test_adjustWindowSessionDuration_6" tt (minDuration s)
>     assertEqual "test_adjustWindowSessionDuration_7" tt (maxDuration s)
>     assertEqual "test_adjustWindowSessionDuration_7" at (sAllottedS s)
>     assertEqual "test_adjustWindowSessionDuration_7" at (sAllottedT s)
>     -- second window is active, no adjustment
>     let s = adjustWindowSessionDuration dt1 (28*24*60) tw
>     assertEqual "test_adjustWindowSessionDuration_8"  tt (minDuration s)
>     assertEqual "test_adjustWindowSessionDuration_9" tt (maxDuration s)
>     -- last part of first window, first part of second window
>     -- (no defaults), adjust according to the first active window
>     let s = adjustWindowSessionDuration dt2 (25*24*60) tw
>     assertEqual "test_adjustWindowSessionDuration_10" ntt (minDuration s)
>     assertEqual "test_adjustWindowSessionDuration_11" ntt (maxDuration s)
>     assertEqual "test_adjustWindowSessionDuration_11" ntt (sAllottedS s)
>     assertEqual "test_adjustWindowSessionDuration_11" ntt (sAllottedT s)
>       where
>         -- test session where first window's total time is
>         -- less than the session's min/max durations
>         at   = 8*60 -- allotted time of session
>         tt   = 4*60 -- total time of each window
>         ntt  = 2*60 -- new total time, i.e. remaining time of window!
>         win1 = (head . windows $ tw1) { wTotalTime = ntt }
>         win2 = head . tail . windows $ tw1
>         tw   =  tw1 { windows = [win1, win2] }
>         dt1  = fromGregorian 2006  9 29  0 0 0
>         dt2  = fromGregorian 2006 10  5  0 0 0

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

