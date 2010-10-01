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
>   ]
>

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
>     assertEqual "test_isSchedulableType 2" False (isSchedulableType dt1 (24*60) tw2)
>     -- session is Windowed with a window inside the scheduling range,
>     -- and has a chosen period
>     assertEqual "test_isSchedulableType 3" False  (isSchedulableType dt1 (6*24*60) tw2)
>     -- session is Windowed with a window inside the scheduling range,
>     -- but with the window's period also in the scheduling range
>     -- and has a chosen period
>     assertEqual "test_isSchedulableType 4" False (isSchedulableType dt1 (8*24*60) tw2)
>     -- session is Windowed, but no windows inside the scheduling range
>     assertEqual "test_isSchedulableType 5" False (isSchedulableType dt2 (2*24*60) tw2)
>     -- session is Windowed with a window inside the scheduling range
>     assertEqual "test_isSchedulableType 6" True  (isSchedulableType dt2 (5*24*60) tw2)
>     -- session is Windowed with a window inside the scheduling range,
>     -- but with the window's period also in the scheduling range
>     assertEqual "test_isSchedulableType 7" False (isSchedulableType dt2 (9*24*60) tw2)
>       where
>         dt1 = fromGregorian 2006 10 13  0 0 0
>         dt2 = fromGregorian 2006  9 20  0 0 0

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

