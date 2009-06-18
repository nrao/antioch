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

TBF: Can't get this to work!

> test_sAvail2 = TestCase $ do
>   print . sessions $ pr4
>   print $ map periods (sessions pr4)
>   print . pAlloted $ pr4
>   print . pAlloted . project $ s4
>   assertEqual "test_sAvail2_1" 0 (pAvail pr2 sem) 
>   assertEqual "test_sAvail2_11" (-120) (pAvail pr4 sem) -- should be same 
>   assertEqual "test_sAvail2_12" (-120) (pAvail (project s4) sem) -- as this! 
>   assertEqual "test_sAvail2_13" (60) (pAlloted (project s4)) 
>   assertEqual "test_sAvail2_2" (-60)  (sAvailTotal s4) 
>   assertEqual "test_sAvail2_3" (-120) (sAvail s4 sem) 

Utilities:

> proj = defaultProject { pAlloted = 2*60 }   -- 0
> sess = defaultSession { sAlloted = 2*60, project = proj }
> mkPeriod s dt = defaultPeriod { session = s, startTime = dt, duration = 60 }
> dt1 = fromGregorian 2006 1 1 0 0 0
> sem = dt2semester dt1

>   -- plenty of time
> p1 = mkPeriod sess dt1 
> ps1 = [p1]
> s1 = makeSession sess ps1 
> pr1 = makeProject proj (pAlloted proj) [s1]

> -- use up exactly the alloted time
> dt2 = fromGregorian 2006 1 1 1 0 0
> p2 = mkPeriod sess dt2 
> ps2 = [p1, p2]
> s2 = makeSession sess ps2
> pr2 = makeProject proj (pAlloted proj) [s2]

> -- use too much time
> dt3 = fromGregorian 2006 1 1 2 0 0
> p3 = mkPeriod sess dt3 
> ps3 = [p1, p2, p3]
> s3 = makeSession sess ps3
> pr3 = makeProject proj (pAlloted proj) [s3]

> -- use too much time, especailly project time
> proj2 = defaultProject { pAlloted = 60 }
> sess2 = sess { project = proj2 }
> ps4 = map (mkPeriod sess2) [dt1, dt2, dt3] -- use 3 * 60
> s4 = makeSession sess2 ps4 -- sess overbooked by -60
> pr4 = makeProject proj2 (pAlloted proj2) [s4] -- proj overbooked by -120
