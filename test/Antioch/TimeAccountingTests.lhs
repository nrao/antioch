> module Antioch.TimeAccountingTests where

> import Antioch.DateTime
> import Antioch.PProjects
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities
> import Test.HUnit

> tests = TestList [
>     test_sUsed
>   , test_sAvailTotal
>   , test_sAvail
>   , test_sAvail2
>   , test_sAvail3
>     ]

> test_sUsed = TestCase $ do
>   assertEqual "test_sUsed_1" (1*60) (sUsed s1) 
>   assertEqual "test_sUsed_2" (2*60) (sUsed s2) 
>   assertEqual "test_sUsed_3" (3*60) (sUsed s3) 

> test_sAvailTotal = TestCase $ do
>   assertEqual "test_sAvailTotal_1" (1*60) (sAvailTotal s1) 
>   assertEqual "test_sAvailTotal_2" (0*60) (sAvailTotal s2) 
>   assertEqual "test_sAvailTotal_3" (-60)  (sAvailTotal s3) 

> test_sAvail = TestCase $ do
>   assertEqual "test_sAvail_1" (1*60) (sAvail s1 sem) 
>   assertEqual "test_sAvail_2" (0*60) (sAvail s2 sem) 
>   assertEqual "test_sAvail_3" (-60)  (sAvail s3 sem) 

> test_sAvail2 = TestCase $ do
>   assertEqual "test_sAvail2_1" 0 (pAvail pr2 sem) 
>   assertEqual "test_sAvail2_2" (-120) (pAvail pr4 sem) -- should be same 
>   assertEqual "test_sAvail2_3" (-120) (pAvail (project s4) sem) -- as this! 
>   assertEqual "test_sAvail2_4" (60) (pAlloted (project s4)) 
>   assertEqual "test_sAvail2_5" (-60)  (sAvailTotal s4) 
>   assertEqual "test_sAvail2_6" (-120) (sAvail s4 sem) 

> test_sAvail3 = TestCase $ do
>   assertEqual "test_sAvail3_1" (3*60) (sUsed s5) 
>   assertEqual "test_sAvail3_2" (1*60) (sAvailTotal s5) 
>   assertEqual "test_sAvail3_3" (3*60) (pUsed pr5) 
>   assertEqual "test_sAvail3_4" (1*60) (pAvailTotal pr5) 
>   assertEqual "test_sAvail3_5" (-60)  (pSemesterRemainingTime pr5 sem) 
>   assertEqual "test_sAvail3_6" (2*60) (pSemesterRemainingTime pr5 "09C") 
>   assertEqual "test_sAvail3_7" (-60)  (pAvail pr5 sem) 
>   assertEqual "test_sAvail3_8" (1*60) (pAvail pr5 "09C") 
>   assertEqual "test_sAvail3_9" (-60)  (sAvail s5 sem) 
>   assertEqual "test_sAvail3_10" (1*60) (sAvail s5 "09C") 

Utilities:

Construct periods, sessions, projects, then tie the knots!

> proj = defaultProject { pAlloted = 2*60 }   -- 0
> sess = defaultSession { sAlloted = 2*60, project = proj }
> mkPeriod s dt = defaultPeriod { session = s, startTime = dt, duration = 60 }
> dt1 = fromGregorian 2006 1 1 3 0 0
> sem = dt2semester dt1

>   -- plenty of time
> p1 = mkPeriod sess dt1 
> ps1 = [p1]
> s1' = makeSession sess ps1 
> pr1 = makeProject proj (pAlloted proj) [s1']
> s1 = head . sessions $ pr1

> -- use up exactly the alloted time
> dt2 = fromGregorian 2006 1 1 1 0 0
> p2 = mkPeriod sess dt2 
> ps2 = [p1, p2]
> s2' = makeSession sess ps2
> pr2 = makeProject proj (pAlloted proj) [s2']
> s2 = head . sessions $ pr2

> -- use too much time
> dt3 = fromGregorian 2006 1 1 2 0 0
> p3 = mkPeriod sess dt3 
> ps3 = [p1, p2, p3]
> s3' = makeSession sess ps3
> pr3 = makeProject proj (pAlloted proj) [s3']
> s3 = head . sessions $ pr3

> -- use too much time, especailly project time
> proj2 = defaultProject { pAlloted = 60 }
> sess2 = sess { project = proj2 }
> ps4 = map (mkPeriod sess2) [dt1, dt2, dt3] -- use 3 * 60
> s4' = makeSession sess2 ps4 -- sess overbooked by -60
> pr4 = makeProject proj2 (pAlloted proj2) [s4'] -- proj overbooked by -120
> s4 = head . sessions $ pr4

> -- use too much time for one semester
> proj3 = defaultProject { pAlloted = 4*60, maxSemesterTime = 2*60 }
> sess3 = defaultSession { sAlloted = 4*60, project = proj3 }
> ps5 = map (mkPeriod sess3) [dt1, dt2, dt3] -- use 3 * 60
> s5' = makeSession sess3 ps5 -- sess still has 1*60 left
> -- project overbooked for 09B by -60, but has 60 avail in other semester
> pr5 = makeProject proj3 (pAlloted proj3) [s5'] 
> s5 = head . sessions $ pr5
