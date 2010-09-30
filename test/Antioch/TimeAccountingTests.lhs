> module Antioch.TimeAccountingTests where

> import Antioch.DateTime
> import Antioch.PProjects
> import Antioch.Score
> import Antioch.Filters           (isNotComplete)
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities
> import Test.HUnit

> tests = TestList [
>     test_sCommittedT
>   , test_sUsedT
>   , test_sAvailT
>   , test_sAvailS
>   , test_sAvail2
>   , test_sAvail3
>   , test_sComplete
>   , test_sComplete2
>     ]

> test_sCommittedT = TestCase $ do
>   assertEqual "test_sCommittedT_1" (1*60) (sCommittedT s1) 
>   assertEqual "test_sCommittedT_2" (2*60) (sCommittedT s2) 
>   assertEqual "test_sCommittedT_3" (3*60) (sCommittedT s3) 
>   assertEqual "test_sCommittedT_4" (3*60) (sCommittedT s4) 
>   assertEqual "test_sCommittedT_5" (3*60) (sCommittedT s5) 

> test_sUsedT = TestCase $ do
>   assertEqual "test_sUsedT_1" (0*60) (sUsedT s1) 
>   assertEqual "test_sUsedT_2" (0*60) (sUsedT s2) 
>   assertEqual "test_sUsedT_3" (1*60) (sUsedT s3) 
>   assertEqual "test_sUsedT_4" (0*60) (sUsedT s4) 
>   assertEqual "test_sUsedT_5" (3*60) (sUsedT s5) 

> test_sAvailT = TestCase $ do
>   assertEqual "test_sAvailT_1" (1*60) (sAvailT s1) 
>   assertEqual "test_sAvailT_2" (0*60) (sAvailT s2) 
>   assertEqual "test_sAvailT_3" (-1*60)  (sAvailT s3) 
>   assertEqual "test_sAvailT_4" (-1*60)  (sAvailT s4) 
>   assertEqual "test_sAvailT_5" (1*60)  (sAvailT s5) 

> test_sAvailS = TestCase $ do
>   assertEqual "test_sAvailS_1" (1*60) (sAvailS sem s1)
>   assertEqual "test_sAvailS_2" (1*60) (sAvailS sem s2)
>   assertEqual "test_sAvailS_3" (1*60)  (sAvailS sem s3)
>   assertEqual "test_sAvailS_4" (1*60)  (sAvailS sem s4)
>   assertEqual "test_sAvailS_5" (3*60)  (sAvailS sem s5)

> test_sAvail2 = TestCase $ do
>   assertEqual "test_sAvail2_1" (1*60) (pAvailS sem pr1)
>   assertEqual "test_sAvail2_1" (1*60) (pAvailS sem pr2)
>   assertEqual "test_sAvail2_1" (1*60) (pAvailS sem pr3)
>   assertEqual "test_sAvail2_2" (-1*60) (pAvailS sem pr4)
>   assertEqual "test_sAvail2_3" (-1*60) (pAvailS sem (project s4))
>   assertEqual "test_sAvail2_4" (60) (pAllottedT (project s4)) 

> test_sAvail3 = TestCase $ do
>   assertEqual "test_sAvail3_1" (3*60) (sCommittedT s5) 
>   assertEqual "test_sAvail3_2" (1*60) (sAvailT s5) 
>   assertEqual "test_sAvail3_3" (3*60) (pCommittedT pr5) 
>   assertEqual "test_sAvail3_3" (3*60) (pCommittedT pr3) 
>   assertEqual "test_sAvail3_4" (1*60) (pAvailT pr5) 
>   assertEqual "test_sAvail3_5" (1*60)  (pAvailS sem pr5)
>   assertEqual "test_sAvail3_6" (2*60) (pAvailS "09C" pr5)
>   assertEqual "test_sAvail3_7" (3*60)  (sAvailS sem s5)
>   assertEqual "test_sAvail3_8" (4*60) (sAvailS "09C" s5)

> test_sComplete = TestCase $ do
>   assertEqual "test_sComplete_1"  False (sComplete s1) 
>   assertEqual "test_sComplete_2"  False (pComplete pr1) 
>   assertEqual "test_sComplete_3"  True  (sComplete s2) 
>   assertEqual "test_sComplete_4"  True  (pComplete pr2) 
>   assertEqual "test_sComplete_5"  True  (sComplete s3) 
>   assertEqual "test_sComplete_6"  True  (pComplete pr3) 
>   assertEqual "test_sComplete_7"  True  (sComplete s4) 
>   assertEqual "test_sComplete_8"  True  (pComplete pr4) 
>   assertEqual "test_sComplete_9"  False (sComplete s5) 
>   assertEqual "test_sComplete_10" False (pComplete pr5) 
>   assertEqual "test_sComplete_11" True  (isNotComplete dt undefined s1) 
>   assertEqual "test_sComplete_12" False (isNotComplete dt undefined s2) 
>     where
>   dt = fromGregorian 2006 2 1  7 15 0

> test_sComplete2 = TestCase $ do
>   assertEqual "test_sComplete2_1"  True  (sComplete s6) 
>   assertEqual "test_sComplete2_2"  False (pComplete pr6) 
>   assertEqual "test_sComplete2_3"  True  (sComplete s7) 
>   assertEqual "test_sComplete2_4"  True  (pComplete pr7) 
>   assertEqual "test_sComplete2_5"  False (isNotComplete dt undefined s6) 
>     where
>       -- proj completeness doesn't depend on session completeness
>       s6'' = sess { sClosed = True }
>       s6' = makeSession s6'' [] ps1
>       pr6 = makeProject proj (pAllottedT proj) (pAllottedS proj) [s6']
>       s6 = head . sessions $ pr6
>       -- but sess completeness DOES depend on project completeness
>       s7' = makeSession sess [] ps1 -- sComplete sess == False
>       pr7' = proj { pClosed = True }
>       pr7 = makeProject pr7' (pAllottedT pr7') (pAllottedS pr7') [s7']
>       s7 = head . sessions $ pr7
>       dt = fromGregorian 2006 2 1  7 15 0

Utilities:

Construct periods, sessions, projects, then tie the knots!

> proj = defaultProject { pAllottedT = 2*60, pAllottedS = 2*60 }   -- 0
> sess = defaultSession { sAllottedT = 2*60, sAllottedS = 2*60, project = proj }
> mkPeriod s dt = defaultPeriod { session = s, startTime = dt, pDuration = 60, pState = Pending }
> mkPeriod' s dt = defaultPeriod { session = s, startTime = dt, pDuration = 60, pState = Scheduled }
> dt1 = fromGregorian 2006 2 1 3 0 0
> sem = dt2semester dt1

>   -- plenty of time
> p1 = mkPeriod sess dt1 
> ps1 = [p1]
> s1' = makeSession sess [] ps1 
> pr1 = makeProject proj (pAllottedT proj) (pAllottedS proj) [s1']
> s1 = head . sessions $ pr1

> -- use up exactly the alloted time
> dt2 = fromGregorian 2006 1 1 1 0 0
> p2 = mkPeriod sess dt2 
> ps2 = [p1, p2]
> s2' = makeSession sess [] ps2
> pr2 = makeProject proj (pAllottedT proj) (pAllottedS proj) [s2']
> s2 = head . sessions $ pr2

> -- use too much time
> dt3 = fromGregorian 2006 1 1 2 0 0
> p3 = mkPeriod' sess dt3 
> ps3 = [p1, p2, p3]
> s3' = makeSession sess [] ps3
> pr3 = makeProject proj (pAllottedT proj) (pAllottedS proj) [s3']
> s3 = head . sessions $ pr3

> -- use too much time, especailly project time
> proj2 = defaultProject { pAllottedT = 60 }
> sess2 = sess { project = proj2 }
> ps4 = map (mkPeriod sess2) [dt1, dt2, dt3] -- use 3 * 60
> s4' = makeSession sess2 [] ps4 -- sess overbooked by -60
> pr4 = makeProject proj2 (pAllottedT proj2) (pAllottedS proj2) [s4'] -- proj overbooked by -120
> s4 = head . sessions $ pr4

> -- use too much time for one semester
> proj3 = defaultProject { pAllottedT = 4*60, pAllottedS = 2*60 }
> sess3 = defaultSession { sAllottedT = 4*60, sAllottedS = 4*60, project = proj3 }
> ps5 = map (mkPeriod' sess3) [dt1, dt2, dt3] -- use 3 * 60
> s5' = makeSession sess3 [] ps5 -- sess still has 1*60 left

> -- project overbooked for 09B by -60, but has 60 avail in other semester
> pr5 = makeProject proj3 (pAllottedT proj3) (pAllottedS proj3) [s5'] 
> s5 = head . sessions $ pr5
