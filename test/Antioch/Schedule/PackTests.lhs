> {-# OPTIONS -XParallelListComp #-}

> module Antioch.Schedule.PackTests where

> import Antioch.Schedule.Pack 
> import Antioch.DateTime
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Score
> import Antioch.Weather
> import Antioch.Generators (generateTestSessions)
> import Antioch.PProjects
> import Antioch.Utilities
> import Antioch.ReceiverTemperatures
> import Test.HUnit
> import Control.Monad.Reader
> import Data.List (sort)
> import Data.Maybe (fromMaybe)

> tests = TestList [
>     test_NumSteps
>   , test_Unwind1
>   , test_Unwind2
>   , test_Unwind3
>   -- , test_Unwind4 -- TBF: scoring pre-scheduled periods wrong!
>   , test_candidates1
>   , test_candidates2
>   , test_candidates3
>   , test_candidates4
>   , test_Candidates1
>   , test_Candidates2
>   , test_Best
>   , test_filterCandidate
>   , test_filterCandidate_timeBetween
>   , test_filterCandidate_timeAvail
>   , test_transitCheck
>   , test_GetBest1
>   , test_GetBest1'
>   , test_GetBest2
>   , test_GetBestTr
>   , test_GetBest2'
>   , test_GetBest3
>   , test_GetBest3'
>   , test_GetBest4
>   , test_GetBest4' 
>   , test_queryPast
>   , test_inFixed
>   , test_Madd1
>   , test_Madd2
>   , test_step
>   , test_Pack_overlapped_fixed
>   , test_Pack1
>   , test_PackTransit1
>   , test_PackTransit2
>   --, test_PackBt
>   , test_Pack2
>   , test_Pack3
>   , test_Pack4
>   , test_Pack5
>   , test_Pack6
>   , test_Pack7
>   , test_Pack8
>   , test_PackWorker'1
>   , test_PackWorker'3
>   , test_PackWorker'5
>   , test_PackWorker'6
>   , test_PackWorker'6_1
>   , test_PackWorker'6_2
>   , test_PackWorker'6_3
>   , test_PackWorker'6_4
>   , test_getBest_for_PackWorker'6_3
>   , test_PackWorker'Simple
>   , test_PackWorker'Simple2
>   , test_PackWorker1
>   , test_PackWorker2
>   , test_PackWorker3
>   , test_PackWorker4
>   -- , test_PackWorker5 -- TBF: related to test_Unwind5
>   , test_PackWorkerSimple
>   , test_RandomScore
>   , test_RandomScore2
>   , test_restoreBnd
>   , test_TestPack_pack1
>   , test_TestPack_pack1withHistory
>   , test_TestPack_pack2
>   , test_TestPack_pack3
>   , test_TestPack_pack8
>   , test_ToCandidate
>   , test_ToItem
>   , test_ToItem2
>   , test_deriveTransits
>   , test_restoreFixedScore_replace
>   , test_restoreFixedScore_not_replace
>   , test_ToPeriod
>   , test_ToSchedule
>   , test_ToSchedule2
>   ]

Simplified interfaces to Item data struct:

> enoughTime = 10000

> item id min max future past = Item id 0 min max enoughTime enoughTime 0 Optional [] future past

> dItem = Item {
>     iId = 0
>   , iProj = 0
>   , iMinDur = 0
>   , iMaxDur = 0
>   , iSTimAv = enoughTime 
>   , iPTimAv = enoughTime
>   , iTimeBt = 0
>   , iTrType = Optional
>   , iTrnsts = []
>   , iFuture = []
>   , iPast   = []
>   }

Begin tests:

> test_inFixed = TestCase $ do
>   assertEqual "test_inFixed_1" 0 (length $ inFixed dt1 fixed)
>   assertEqual "test_inFixed_2" 0 (length $ inFixed dt2 fixed)
>   assertEqual "test_inFixed_3" 1 (length $ inFixed dt3 fixed)
>   assertEqual "test_inFixed_4" 0 (length $ inFixed dt4 fixed)
>     where
>       dt1 = fromGregorian 2006 1 1 0 0 0
>       dt2 = fromGregorian 2006 1 1 3 0 0
>       dt3 = fromGregorian 2006 1 1 1 0 0
>       dt4 = fromGregorian 2006 1 1 9 0 0
>       f1 = defaultPeriod {startTime = dt1, duration = 2*60, pDuration = 2*60}
>       f2 = defaultPeriod {startTime = dt2, duration = 2*60, pDuration = 2*60}
>       fixed = [f1, f2]
>       rst = inFixed dt1 fixed

> test_restoreBnd = TestCase $ do
>   assertEqual "test_restoreBnd_0" [f1] (inFixed dt1 fixed)
>   assertEqual "test_restoreBnd_1" [f1, p2, p3, p4] (restoreBnd dt1 True fixed ps)
>   assertEqual "test_restoreBnd_2" [f3] (inFixed dt5 fixed)
>   assertEqual "test_restoreBnd_3" [p1, p2, p3, f3] (restoreBnd dt5 False fixed ps)
>   assertEqual "test_restoreBnd_4" ps (restoreBnd dt5 False [f1, p2] ps)
>     where
>       dt0 = fromGregorian 2006 1 1 0 0 0 
>       dt1 = fromGregorian 2006 1 1 1 0 0 -- start
>       dt2 = fromGregorian 2006 1 1 3 0 0
>       dt3 = fromGregorian 2006 1 1 5 0 0
>       dt4 = fromGregorian 2006 1 1 9 0 0 
>       dt5 = fromGregorian 2006 1 1 11 0 0 -- end
>       -- schedule produced by PackWorker
>       p1 = defaultPeriod {startTime = dt1, duration = 2*60, pDuration = 2*60}
>       p2 = defaultPeriod {startTime = dt2, duration = 2*60, pDuration = 2*60}
>       p3 = defaultPeriod {startTime = dt3, duration = 4*60, pDuration = 4*60}
>       p4 = defaultPeriod {startTime = dt4, duration = 2*60, pDuration = 2*60}
>       ps = [p1, p2, p3, p4]
>       -- what the original 
>       f1 = defaultPeriod {startTime = dt0, duration = 3*60, pDuration = 3*60} -- before start!
>       f3 = defaultPeriod {startTime = dt4, duration = 6*60, pDuration = 6*60} -- after end!
>       fixed = [f1, p2, f3]

> test_NumSteps = TestCase . assertEqual "test_NumSteps" 192 . numSteps $ 48 * 60

> test_Unwind1 = TestCase . assertEqual "test_Unwind1" xs . unwind $ ys
>   where
>     xs = [Candidate 1 0 0 2 1.0, Candidate 2 0 2 2 1.0]
>     ys = [Just (Candidate 2 0 0 2 2.0), Nothing, Just (Candidate 1 0 0 2 1.0), Nothing, Nothing]

> test_Unwind2 = TestCase . assertEqual "test_Unwind2" xs . unwind $ ys
>   where
>     xs = [Candidate 1 0 0 1 1.0, Candidate 2 0 1 2 1.0, Candidate 3 0 3 3 1.0]
>     ys = [Just (Candidate 3 0 0 3 3.0), Nothing, Nothing, Just (Candidate 2 0 0 2 2.0), Nothing, Just (Candidate 1 0 0 1 1.0), Nothing]

> test_Unwind3 = TestCase . assertEqual "test_Unwind3" xs . unwind $ ys
>   where
>     xs = [Candidate 1 0 0 4 4.0]
>     ys = [Just (Candidate 1 0 0 4 4.0), Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]

TBF: this test may be exposing the bug wherein the fixed candiate gets a 
negative score.

> test_Unwind4 = TestCase . assertEqual "test_Unwind4" xs . unwind $ ys
>   where
>     xs = [Candidate "B" 0 0 3 0.75, Candidate "F1" 0 3 1 0.0]
>     ys = [     Just (Candidate "F1" 0 0 1 1.0)
>              , Just (Candidate "B" 0  0 3 0.75)
>              , Just (Candidate "B" 0  0 2 0.50)
>              , Nothing
>              , Nothing
>          ]

> test_Candidates1 = TestCase . assertEqual "test_Candidates1" xs . candidates $ ys
>   where
>     xs = [Nothing, Just (Candidate 1 0 0 2 1.0), Just (Candidate 1 0 0 3 2.0), Just (Candidate 1 0 0 4 3.0)]
>     ys = item 1 2 4 [] (replicate 6 1.0)

> test_Candidates2 = TestCase . assertEqual "test_Candidates2" xs . candidates $ ys
>   where
>     xs = []
>     ys = item 1 2 4 [] (0.0 : replicate 5 1.0)

> test_Best = TestCase . assertEqual "test_Best" xs . best $ ys
>   where
>     xs = Just (Candidate 1 0 0 4 4.0)
>     ys = [Nothing, Nothing, Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 4 4.0)]

> test_Madd1 = TestCase . assertEqual "test_Madd1" xs . best . zipWith madd ys $ zs
>   where
>     xs = Just (Candidate 1 0 0 4 4.0)
>     ys = [Nothing, Nothing, Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 4 4.0)]
>     zs = replicate 4 Nothing

> test_Madd2 = TestCase . assertEqual "test_Madd2" xs . best . zipWith madd ys $ zs
>   where
>     xs = Nothing
>     ys = [Nothing, Nothing, Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 4 4.0)]
>     zs = replicate 2 Nothing

> test_step = TestCase $ do
>     assertEqual "test_step 1" ([1.0, 2.0, 3.0], []) ((iFuture item1), (iPast item1))
>     assertEqual "test_step 2" ([2.0, 3.0], [1.0]) ((iFuture item2), (iPast item2))
>     assertEqual "test_step 3" ([3.0], [2.0, 1.0]) ((iFuture item3), (iPast item3))
>     assertEqual "test_step 4" ([], [3.0, 2.0, 1.0]) ((iFuture item4), (iPast item4))
>     assertEqual "test_step 5" ([], [0.0, 3.0, 2.0, 1.0]) ((iFuture item5), (iPast item5))
>     assertEqual "test_step 6" ([], [0.0, 0.0, 3.0, 2.0, 1.0]) ((iFuture item6), (iPast item6))
>       where
>     item1 = item 1 8 12 [1.0, 2.0, 3.0] []
>     item2 = step item1
>     item3 = step item2
>     item4 = step item3
>     item5 = step item4
>     item6 = step item5

> testItem1 = item 1 2 4 (replicate 6 1.0) []
> testItem2 = item 2 2 4 [0.0,0.0,2.0,2.0,2.0,2.0] []
> testItems = [testItem1, testItem2]

> test_GetBest1 = TestCase . assertEqual "test_getBest1" xs . getBest 0 past $ sessions 
>   where
>     xs = Nothing -- Just (Candidate 1 0 0 4 4.0)
>     past = [Nothing]
>     sessions = map step [testItem1]

> test_GetBest1' = TestCase . assertEqual "test_getBest1'" xs . getBest 0 past $ sessions 
>   where
>     xs = Nothing -- Just (Candidate 1 0 0 4 4.0)
>     past = [Nothing]
>     sessions = map step testItems

> test_GetBest2 = TestCase . assertEqual "test_getBest2" xs . getBest 0 past $ sessions 
>   where
>     xs = Just (Candidate 1 0 0 2 1.0)
>     past = [Nothing, Nothing]
>     sessions = map (step . step) [testItem1]

> test_GetBestTr = TestCase . assertEqual "test_getBestTr" xs . getBest 0 past $ sessions 
>   where
>     xs = Just (Candidate 1 0 0 2 1.0)
>     past = [Nothing, Nothing]
>     sessions = map (step . step) [testItem1 {iTrType = Partial, iTrnsts = [0]}]

> test_GetBest2' = TestCase . assertEqual "test_getBest2'1" xs . getBest 0 past $ sessions 
>   where
>     xs = Just (Candidate 1 0 0 2 1.0)
>     past = [Nothing, Nothing]
>     sessions = map (step . step) testItems 

> test_GetBest3 = TestCase . assertEqual "test_getBest3" xs . getBest 0 past $ sessions 
>   where
>     xs = Just (Candidate 1 0 0 3 2.0)
>     past = [Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     sessions = map (step . step . step) [testItem1] 

> test_GetBest3' = TestCase . assertEqual "test_getBest3'" xs . getBest 0 past $ sessions 
>   where
>     xs = Just (Candidate 1 0 0 3 2.0)
>     past = [Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     sessions = map (step . step . step) testItems

> test_GetBest4 = TestCase . assertEqual "test_getBest4" xs . getBest 0 past $ sessions 
>   where
>     xs = Just (Candidate 1 0 0 4 3.0)
>     past = [Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     sessions = map (step . step . step . step) [testItem1]

> test_GetBest4' = TestCase . assertEqual "test_getBest4'" xs . getBest 0 past $ sessions 
>   where
>     xs = Just (Candidate 2 0 0 2 4.0)
>     past = [Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     sessions = map (step . step . step . step) testItems

> test_queryPast = TestCase $ do
>   assertEqual "test_queryPast101" (0, 0, 0, []) (queryPast testItem1 (drop 6 past) 1)
>   assertEqual "test_queryPast201" (0, 0, 0, []) (queryPast testItem2 (drop 6 past) 1)
>   assertEqual "test_queryPast111" (0, 0, 1, [0]) (queryPast testItem1 (drop 5 past) 1)
>   assertEqual "test_queryPast211" (0, 0, 1, [0]) (queryPast testItem2 (drop 5 past) 1)
>   assertEqual "test_queryPast121" (2, 2, 0, [1])  (queryPast testItem1 (drop 4 past) 1)
>   assertEqual "test_queryPast221" (0, 2, 0, [1]) (queryPast testItem2 (drop 4 past) 1)
>   assertEqual "test_queryPast131" (3, 3, 0, [2]) (queryPast testItem1 (drop 3 past) 1)
>   assertEqual "test_queryPast231" (0, 3, 0, [2]) (queryPast testItem2 (drop 3 past) 1)
>   assertEqual "test_queryPast141" (2, 4, 2, [1,1]) (queryPast testItem1 (drop 2 past) 1)
>   assertEqual "test_queryPast241" (2, 4, 0, [1,1]) (queryPast testItem2 (drop 2 past) 1)
>   assertEqual "test_queryPast151" (2, 5, 3, [2,1]) (queryPast testItem1 (drop 1 past) 1)
>   assertEqual "test_queryPast251" (3, 5, 0, [2,1]) (queryPast testItem2 (drop 1 past) 1)
>   assertEqual "test_queryPast161" (2, 6, 4, [3,1]) (queryPast testItem1 past 1)
>   assertEqual "test_queryPast261" (4, 6, 0, [3,1]) (queryPast testItem2 past 1)
>   assertEqual "test_queryPast122" (0, 0, 1, [0])  (queryPast testItem1 (drop 4 past) 2)
>   assertEqual "test_queryPast222" (0, 0, 1, [0]) (queryPast testItem2 (drop 4 past) 2)
>   assertEqual "test_queryPast132" (2, 2, 0, [1]) (queryPast testItem1 (drop 3 past) 2)
>   assertEqual "test_queryPast232" (0, 2, 0, [1]) (queryPast testItem2 (drop 3 past) 2)
>   assertEqual "test_queryPast142" (3, 3, 0, [2]) (queryPast testItem1 (drop 2 past) 2)
>   assertEqual "test_queryPast242" (0, 3, 0, [2]) (queryPast testItem2 (drop 2 past) 2)
>   assertEqual "test_queryPast152" (2, 4, 2, [1,1]) (queryPast testItem1 (drop 1 past) 2)
>   assertEqual "test_queryPast252" (2, 4, 0, [1,1]) (queryPast testItem2 (drop 1 past) 2)
>   assertEqual "test_queryPast162" (2, 5, 3, [2,1]) (queryPast testItem1 past 2)
>   assertEqual "test_queryPast262" (3, 5, 0, [2,1]) (queryPast testItem2 past 2)
>   assertEqual "test_queryPast153" (3, 3, 0, [2]) (queryPast testItem1 (drop 1 past) 3)
>   assertEqual "test_queryPast253" (0, 3, 0, [2]) (queryPast testItem2 (drop 1 past) 3)
>   assertEqual "test_queryPast163" (2, 4, 2, [1,1]) (queryPast testItem1 past 3)
>   assertEqual "test_queryPast263" (2, 4, 0, [1,1]) (queryPast testItem2 past 3)
>   assertEqual "test_queryPast1hole" (2, 4, 3, [0,1,1]) (queryPast testItem1 hole 1)
>   assertEqual "test_queryPast2hole" (2, 4, 1, [0,1,1]) (queryPast testItem2 hole 1)
>     where
>       past = [Just (Candidate 2 0 0 4 10.0), Just (Candidate 2 0 0 3 8.0)
>              ,Just (Candidate 2 0 0 2 6.0),  Just (Candidate 1 0 0 3 3.0)
>              ,Just (Candidate 1 0 0 2 2.0),  Nothing, Nothing]
>       hole = [Nothing, Just (Candidate 2 0 0 2 6.0),  Just (Candidate 1 0 0 3 3.0)
>              ,Just (Candidate 1 0 0 2 2.0),  Nothing, Nothing]

Happy Path tests for filterCandidate: all candidates should be accepted

> test_filterCandidate = TestCase $ do
>   assertEqual "test_filterCandidate_1" cn (filterCandidate i1 past cn)
>   assertEqual "test_filterCandidate_2" c1 (filterCandidate i1 past c1)
>   assertEqual "test_filterCandidate_3" c2 (filterCandidate i1 past c2)
>   assertEqual "test_filterCandidate_4" cn (filterCandidate i2 past cn)
>   assertEqual "test_filterCandidate_5" c1 (filterCandidate i2 past c1)
>   assertEqual "test_filterCandidate_6" c2 (filterCandidate i2 past c2)
>     where
>       i1 = testItem1
>       i2 = testItem2
>       past = [Just (Candidate 2 0 0 4 10.0), Just (Candidate 2 0 0 3 8.0)
>              ,Just (Candidate 2 0 0 2 6.0),  Just (Candidate 1 0 0 3 3.0)
>              ,Just (Candidate 1 0 0 2 2.0),  Nothing, Nothing]
>       cn = Nothing
>       c1' = Candidate 1 0 0 2 2.0
>       c1  = Just c1'
>       c2' = Candidate 2 0 0 2 2.0
>       c2  = Just c2'

Non-Happy Path tests for filterCandidate: some candidates get filtered

> test_filterCandidate_timeBetween = TestCase $ do
>   -- for a candidate of duration 1, see the affect of decreasing item's tb
>   assertEqual "test_filterCandidate2_1" cn   (filterCandidate i1_100 p c1_1)
>   assertEqual "test_filterCandidate2_2" cn   (filterCandidate i1_5   p c1_1)
>   assertEqual "test_filterCandidate2_3" c1_1 (filterCandidate i1_4   p c1_1)
>   assertEqual "test_filterCandidate2_4" c1_1 (filterCandidate i1_3   p c1_1)
>   assertEqual "test_filterCandidate2_5" c1_1 (filterCandidate i1_2   p c1_1)
>   -- now observe the same pattern, but w/ a candidate of duration 2
>   assertEqual "test_filterCandidate2_6"  cn   (filterCandidate i1_100 p c1_2)
>   assertEqual "test_filterCandidate2_7"  cn   (filterCandidate i1_5   p c1_2)
>   assertEqual "test_filterCandidate2_8"  cn   (filterCandidate i1_4   p c1_2)
>   assertEqual "test_filterCandidate2_9"  c1_2 (filterCandidate i1_3   p c1_2)
>   assertEqual "test_filterCandidate2_10" c1_2 (filterCandidate i1_2   p c1_2)
>   -- even when the item's tb is large, doesn't matter for other candidates
>   assertEqual "test_filterCandidate2_11" c2_1 (filterCandidate i1_100 p c2_1)
>   -- try the same test for item 2 and it's candidates
>   assertEqual "test_filterCandidate2_12"  cn   (filterCandidate i2_100 p c2_1)
>   assertEqual "test_filterCandidate2_13"  cn   (filterCandidate i2_3   p c2_1)
>   assertEqual "test_filterCandidate2_14"  cn   (filterCandidate i2_2   p c2_1)
>   assertEqual "test_filterCandidate2_15"  cn   (filterCandidate i2_1   p c2_1)
>   assertEqual "test_filterCandidate2_16"  c2_1 (filterCandidate i2_0   p c2_1)
>   
>     where
>       i1_100 = testItem1 { iTimeBt = 100 } -- no way
>       i1_5 = testItem1 { iTimeBt = 5 } 
>       i1_4 = testItem1 { iTimeBt = 4 } 
>       i1_3 = testItem1 { iTimeBt = 3 }
>       i1_2 = testItem1 { iTimeBt = 2 }
>       i2_100 = testItem2 { iTimeBt = 100 } -- no way
>       i2_3 = testItem2 { iTimeBt = 3 }
>       i2_2 = testItem2 { iTimeBt = 2 }
>       i2_1 = testItem2 { iTimeBt = 1 }
>       i2_0 = testItem2 { iTimeBt = 0 }
>       cn   = Nothing
>       c1_1 = Just $ Candidate 1 0 0 1 1.0
>       c1_2 = Just $ Candidate 1 0 0 2 2.0
>       c2_1 = Just $ Candidate 2 0 0 1 1.0
>       p    = [Just (Candidate 2 0 0 4 10.0), Just (Candidate 2 0 0 3 8.0)
>              ,Just (Candidate 2 0 0 2 6.0),  Just (Candidate 1 0 0 3 3.0)
>              ,Just (Candidate 1 0 0 2 2.0),  Nothing, Nothing]

Non-Happy Path tests for filterCandidate: some candidates get filtered

> test_filterCandidate_timeAvail = TestCase $ do
>   -- candidate of duration 1, see it get filtered till it has more time av.
>   assertEqual "test_filterCandidate3_1" cn   (filterCandidate i1_0 p c1_1)
>   assertEqual "test_filterCandidate3_2" cn   (filterCandidate i1_1 p c1_1)
>   assertEqual "test_filterCandidate3_3" cn   (filterCandidate i1_2 p c1_1)
>   assertEqual "test_filterCandidate3_4" c1_1 (filterCandidate i1_3 p c1_1)
>   assertEqual "test_filterCandidate3_5" c1_1 (filterCandidate i1_4 p c1_1)
>   -- candidate of duration 4, see it get filtered till it has more time av.
>   assertEqual "test_filterCandidate3_6" cn   (filterCandidate i1_0 p c1_4)
>   assertEqual "test_filterCandidate3_7" cn   (filterCandidate i1_1 p c1_4)
>   assertEqual "test_filterCandidate3_8" cn   (filterCandidate i1_2 p c1_4)
>   assertEqual "test_filterCandidate3_9" cn   (filterCandidate i1_3 p c1_4)
>   assertEqual "test_filterCandidate3_10" cn  (filterCandidate i1_4 p c1_4)
>   assertEqual "test_filterCandidate3_11" cn  (filterCandidate i1_5 p c1_4)
>   assertEqual "test_filterCandidate3_12" cn  (filterCandidate i1_6 p c1_4)
>   assertEqual "test_filterCandidate3_13" c1_4 (filterCandidate i1_7 p c1_4)
>   -- same pattern, different session
>   assertEqual "test_filterCandidate3_14" cn  (filterCandidate i2_0 p c2_1)
>   assertEqual "test_filterCandidate3_15" cn  (filterCandidate i2_1 p c2_1)
>   assertEqual "test_filterCandidate3_16" cn  (filterCandidate i2_2 p c2_1)
>   assertEqual "test_filterCandidate3_17" cn  (filterCandidate i2_3 p c2_1)
>   assertEqual "test_filterCandidate3_18" cn  (filterCandidate i2_4 p c2_1)
>   assertEqual "test_filterCandidate3_19" c2_1 (filterCandidate i2_5 p c2_1)
>     where
>       i1_0 = testItem1 { iSTimAv = 0, iTimeBt = 0 } -- no way buddy
>       i1_1 = testItem1 { iSTimAv = 1, iTimeBt = 0 } 
>       i1_2 = testItem1 { iSTimAv = 2, iTimeBt = 0 } 
>       i1_3 = testItem1 { iSTimAv = 3, iTimeBt = 0 } 
>       i1_4 = testItem1 { iSTimAv = 4, iTimeBt = 0 } 
>       i1_5 = testItem1 { iSTimAv = 5, iTimeBt = 0 } 
>       i1_6 = testItem1 { iSTimAv = 6, iTimeBt = 0 } 
>       i1_7 = testItem1 { iSTimAv = 7, iTimeBt = 0 } 
>       i2_0 = testItem2 { iSTimAv = 0, iTimeBt = 0 } 
>       i2_1 = testItem2 { iSTimAv = 1, iTimeBt = 0 } 
>       i2_2 = testItem2 { iSTimAv = 2, iTimeBt = 0 } 
>       i2_3 = testItem2 { iSTimAv = 3, iTimeBt = 0 } 
>       i2_4 = testItem2 { iSTimAv = 4, iTimeBt = 0 } 
>       i2_5 = testItem2 { iSTimAv = 5, iTimeBt = 0 } 
>       cn   = Nothing
>       c1_1 = Just $ Candidate 1 0 0 1 1.0
>       c1_4 = Just $ Candidate 1 0 0 4 1.0
>       c2_1 = Just $ Candidate 2 0 0 1 1.0
>       p    = [Just (Candidate 2 0 0 4 10.0), Just (Candidate 2 0 0 3 8.0)
>              ,Just (Candidate 2 0 0 2 6.0),  Just (Candidate 1 0 0 3 3.0)
>              ,Just (Candidate 1 0 0 2 2.0),  Nothing, Nothing]
>

> test_transitCheck = TestCase $ do
>   assertEqual "transitCheck optional" (cnd 0) (transitCheck dItem 8 (Just defaultCandidate))
>   -- vanilla, non-pathological numbers
>   assertEqual "transitCheck  0" Nothing (transitCheck pItem 10 (cnd 8))
>   assertEqual "transitCheck  1" Nothing (transitCheck pItem 11 (cnd 8))
>   assertEqual "transitCheck  2" (cnd 8) (transitCheck pItem 12 (cnd 8))
>   assertEqual "transitCheck  3" (cnd 8) (transitCheck pItem 13 (cnd 8))
>   assertEqual "transitCheck  4" (cnd 8) (transitCheck pItem 14 (cnd 8))
>   assertEqual "transitCheck  5" (cnd 8) (transitCheck pItem 15 (cnd 8))
>   assertEqual "transitCheck  6" Nothing (transitCheck pItem 16 (cnd 8))
>   assertEqual "transitCheck  7" Nothing (transitCheck pItem 17 (cnd 8))
>   assertEqual "transitCheck  8" Nothing (transitCheck pItem 18 (cnd 8))
>   -- transit type center implies only one good candidate per transit
>   assertEqual "transitCheck  9" Nothing (transitCheck cItem 15 (cnd 12))
>   assertEqual "transitCheck 10" (cnd 12) (transitCheck cItem 16 (cnd 12))
>   assertEqual "transitCheck 11" Nothing (transitCheck cItem 17 (cnd 12))
>   -- odd duration better put the middle quarter on the transit
>   assertEqual "transitCheck 12" Nothing (transitCheck cItem 15 (cnd 13))
>   assertEqual "transitCheck 13" (cnd 13) (transitCheck cItem 16 (cnd 13))
>   assertEqual "transitCheck 14" Nothing (transitCheck cItem 17 (cnd 13))
>   assertEqual "transitCheck 15" Nothing (transitCheck cItem 18 (cnd 13))
>   -- short durations
>   assertEqual "transitCheck 16" Nothing (transitCheck cItem 12 (cnd 2))
>   assertEqual "transitCheck 17" (cnd 2) (transitCheck cItem 11 (cnd 2))
>   assertEqual "transitCheck 18" Nothing (transitCheck cItem 10 (cnd 2))
>   assertEqual "transitCheck 19" Nothing (transitCheck cItem 11 (cnd 1))
>   assertEqual "transitCheck 20" (cnd 1) (transitCheck cItem 10 (cnd 1))
>   assertEqual "transitCheck 21" Nothing (transitCheck cItem  9 (cnd 1))
>   -- spot check longer durations
>   assertEqual "transitCheck 22" Nothing (transitCheck pItem 19 (cnd 12))
>   assertEqual "transitCheck 23" (cnd 12) (transitCheck pItem 18 (cnd 12))
>   assertEqual "transitCheck 24" (cnd 12) (transitCheck pItem 13 (cnd 12))
>   assertEqual "transitCheck 25" Nothing (transitCheck pItem 12 (cnd 12))
>   assertEqual "transitCheck 26" Nothing (transitCheck cItem 17 (cnd 12))
>   assertEqual "transitCheck 27" (cnd 12) (transitCheck cItem 16 (cnd 12))
>   assertEqual "transitCheck 28" Nothing (transitCheck cItem 15 (cnd 12))
>   -- second transit should also work
>   assertEqual "transitCheck 29" Nothing (transitCheck pItem 20 (cnd 8))
>   assertEqual "transitCheck 30" Nothing (transitCheck pItem 21 (cnd 8))
>   assertEqual "transitCheck 31" (cnd 8) (transitCheck pItem 22 (cnd 8))
>   assertEqual "transitCheck 33" (cnd 8) (transitCheck pItem 23 (cnd 8))
>   assertEqual "transitCheck 33" (cnd 8) (transitCheck pItem 24 (cnd 8))
>   assertEqual "transitCheck 34" (cnd 8) (transitCheck pItem 25 (cnd 8))
>   assertEqual "transitCheck 35" Nothing (transitCheck pItem 26 (cnd 8))
>   assertEqual "transitCheck 36" Nothing (transitCheck pItem 27 (cnd 8))
>   assertEqual "transitCheck 37" Nothing (transitCheck pItem 28 (cnd 8))
>     where
>   cItem = dItem {iTrType = Center,  iTrnsts = [10, 20]}
>   pItem = dItem {iTrType = Partial, iTrnsts = [10, 20]}
>   cnd d = Just (defaultCandidate {cDuration = d})

Test against python unit tests from beta test code:

> test_PackWorker'6 = TestCase . assertEqual "test_PackWorker'6" xs . packWorker' 0 ys zs $ ws
>   where
>     -- result, list of best solutions starting for 60 minutes, then 45,
>     -- 30, and then 15 (none) followed by the sentinel.
>     xs = [Just (Candidate 2 0 0 2 3.0), Just (Candidate 1 0 0 3 2.0)
>          ,Just (Candidate 1 0 0 2 1.0), Nothing, Nothing]
>     -- future, i.e., nothing pre-scheduled
>     ys = replicate 4 Nothing
>     -- past, i.e., start scheduling first quarter
>     zs = [Nothing]
>     -- input, i.e., things (with scores) to be scheduled
>     ws = map step [item 1 2 4 (replicate 6 1.0) []
>                  , item 2 2 4 [0.0,0.0,2.0,2.0,2.0,2.0] []]

> test_PackWorker'6_1 = TestCase . assertEqual "test_PackWorker'6_1" xs . packWorker' 0 ys zs $ ws
>   where
>     xs = [Just (Candidate 2 0 0 2 3.0), Just (Candidate 1 0 0 3 2.0)
>          ,Just (Candidate 1 0 0 2 1.0), Nothing, Nothing]
>     ys = replicate 3 Nothing
>     zs = [Nothing, Nothing]
>     ws = map step [item 1 2 4 [1.0, 1.0, 1.0, 1.0, 1.0] [1.0]
>                  , item 2 2 4 [0.0, 2.0, 2.0, 2.0, 2.0] [0.0]]

> test_PackWorker'6_2 = TestCase . assertEqual "test_PackWorker'6_2" xs . packWorker' 0 ys zs $ ws
>   where
>     xs = [Just (Candidate 2 0 0 2 4.0), Just (Candidate 1 0 0 3 2.0)
>          ,Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     ys = replicate 2 Nothing
>     zs = [Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     ws = map step [item 1 2 4 [1.0, 1.0, 1.0, 1.0] [1.0, 1.0]
>                  , item 2 2 4 [2.0, 2.0, 2.0, 2.0] [0.0, 0.0]]

> test_PackWorker'6_3 = TestCase . assertEqual "test_PackWorker'6_3" xs . packWorker' 0 ys zs $ ws
>   where
>     xs = [Just (Candidate 2 0 0 2 4.0), Just (Candidate 1 0 0 3 3.0)
>          ,Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     ys = replicate 1 Nothing
>     zs = [Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 2 2.0)
>          ,Nothing, Nothing]
>     ws = map step [item 1 2 4 [1.0, 1.0, 1.0] [1.0, 1.0, 1.0]
>                  , item 2 2 4 [2.0, 2.0, 2.0] [2.0, 0.0, 0.0]]

> test_getBest_for_PackWorker'6_3 = TestCase . assertEqual "test_best_for_PackWorker'6_3" result . getBest 0 zs $ ws
>   where
>     result = Just (Candidate {cId = 2, cProj = 0, cStart = 0, cDuration = 2, cScore = 4.0})
>     zs = [Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 2 2.0)
>          ,Nothing, Nothing]
>     ws = map step [item 1 2 4 [1.0, 1.0, 1.0] [1.0, 1.0, 1.0]
>                  , item 2 2 4 [2.0, 2.0, 2.0] [2.0, 0.0, 0.0]]

> test_PackWorker'6_4 = TestCase . assertEqual "test_PackWorker'6_4" xs . packWorker' 0 ys zs $ ws
>   where
>     xs = [Just (Candidate 2 0 0 2 6.0), Just (Candidate 1 0 0 3 3.0)
>          ,Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     ys = []
>     zs = xs
>     ws = map step [item 1 2 4 [1.0, 1.0] [1.0, 1.0, 1.0, 1.0]
>                  , item 2 2 4 [2.0, 2.0] [2.0, 2.0, 0.0, 0.0]]

> test_PackWorker'1 = TestCase . assertEqual "test_PackWorker'1" xs . packWorker' 0 ys zs $ ws
>   where
>     xs = [Just (Candidate 1 0 0 4 3.0), Just (Candidate 1 0 0 3 2.0), Just (Candidate 1 0 0 2 1.0), Nothing, Nothing]
>     ys = replicate 4 Nothing
>     zs = [Nothing]
>     ws = map step [item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker'3 = TestCase . assertEqual "test_PackWorker'3" xs . packWorker' 0 ys zs $ ws
>   where
>     xs = [Just (Candidate 2 0 0 1 1.0), Just (Candidate 1 0 0 2 2.1), Nothing, Just (Candidate 3 0 0 1 1.1), Nothing]
>     ys = [Just (Candidate 3 0 0 1 1.1), Nothing, Nothing, Just (Candidate 2 0 0 1 1.0)]
>     zs = [Nothing]
>     ws = map step [item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker1 = TestCase . assertEqual "test_PackWorker1" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 1 0 0 4 3.0]
>     ys = replicate 4 Nothing  -- nothing prescheduled
>     ws = [item 1 2 4 (replicate 6 1.0) []] -- scores 1.0 for 6 units

> test_PackWorker2 = TestCase . assertEqual "test_PackWorker2" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 1 0 0 3 2.0, Candidate 2 0 3 1 2.0]
>     ys = replicate 3 Nothing ++ [Just (Candidate 2 0 0 1 4.0)]
>     ws = [item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker3 = TestCase . assertEqual "test_PackWorker3" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 3 0 0 1 1.1, Candidate 1 0 1 2 0.9999999, Candidate 2 0 3 1 2.0]
>     ys = [Just (Candidate 3 0 0 1 1.1), Nothing, Nothing, Just (Candidate 2 0 0 1 4.1)]
>     ws = [item 1 2 4 (replicate 6 1.0) []]

This next test `test_PackWorker4` highlights a few different
attributes of the packing algorithm:

  1. The pre-scheduled/fixed sessions do not get lost or overwritten
     in spite of all the open sessions performing better in those time
     slots.

  2. The gap between the two fixed sessions stays empty due to
     min-duration constraints.

  3. We see session D compete with and overtake session C at the end
     of the day.

  4. A scheduled session will not include any zero periods.

> test_PackWorker4 =
>     TestCase . assertEqual "test_PackWorker4" result . packWorker fixed $ open
>   where
>     result = [ Candidate "A" 0  0 2 1.0 
>              , Candidate "F1" 0 2 2 1.0 -- unwind mangles this score
>              , Candidate "F2" 0 5 2 2.0 -- these were wrong in beta!
>              , Candidate "C" 0  7 2 1.0
>              , Candidate "D" 0  9 2 1.0
>              ]
>     fixed  = [ Nothing                        --  0
>              , Nothing                        --  1
>              , Just (Candidate "F1" 0 0 1 1.0)  --  2
>              , Just (Candidate "F1" 0 0 2 2.0)  --  3
>              , Nothing                        --  4
>              , Just (Candidate "F2" 0 0 1 1.0)  --  5
>              , Just (Candidate "F2" 0 0 2 2.0)  --  6
>              , Nothing                        --  7
>              , Nothing                        --  8
>              , Nothing                        --  9
>              , Nothing                        -- 10
>              ]
>     --                       0    1    2    3    4    5    6    7    8    9    10
>     open   = [ item "A" 2 8 [1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 0.0, 0.0, 0.0] []
>              , item "B" 2 8 [0.0, 0.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 0.0, 0.0, 0.0] []
>              , item "C" 2 8 [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 0.5] []
>              , item "D" 2 8 [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0] []
>              ]

> test_ToItem = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     -- create an item without a mask, i.e. no scoring
>     item' <- runScoring w [] rt $ do
>         fs <- genScore starttime [testSession]
>         toItem starttime 0 fs [] testSession
>     assertEqual "test_ToItem_1" result1 item'
>     assertEqual "test_ToItem_2" 0 (length . iFuture $ item')
>     -- now try it with the mask (dts)
>     item' <- runScoring w [] rt $ do
>         fs <- genScore starttime [testSession]
>         toItem starttime 0 fs dts testSession
>     assertEqual "test_ToItem_4" 48 (length . iFuture $ item') 
>     assertEqual "test_ToItem_3" result2 item'
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     duration = 12 * 60
>     -- the 'mask' is just a list of datetimes to score at
>     dts' = quarterDateTimes starttime duration 
>     dts = [(Just dt) | dt <- dts']
>     result1 = dItem {iId = testSession
>                    , iMinDur = 8
>                    , iMaxDur = 24
>                    , iSTimAv = numSteps $ 24 * 60  
>                    , iPTimAv = numSteps $ 24 * 60
>                    , iFuture = []
>                    , iPast = []
>                    }
>     result2 = result1 {iFuture = defaultPackSessionScores}


Same as test above, now just checking the affect of pre-scheduled periods:

> test_ToItem2 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     result <- runScoring w [] rt $ do
>         fs <- genScore starttime [sess]
>         toItem starttime 0 fs dts sess
>     assertEqual "test_ToItem2" expected result
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     duration = 12 * 60
>     dts' = quarterDateTimes starttime duration 
>     fixed1 = Period 0 defaultSession starttime 30 0.0 Pending starttime False 30
>     ft2 = (11*60) `addMinutes'` starttime
>     fixed2 = Period 0 defaultSession ft2 30 0.0 Pending starttime False 30
>     dts = mask dts' (toSchedule dts' [fixed1, fixed2])
>     sess = testSession
>     scores = (take 44 defaultPackSessionScores) ++
>              --[0.0, 0.0, 3.187729, 3.1933162]
>              [0.0, 0.0, 3.1832325,3.18881187]
>     expected = dItem { iId = sess
>                    , iMinDur = 8
>                    , iMaxDur = 24
>                    , iSTimAv = numSteps $ 24 * 60 
>                    , iPTimAv = numSteps $ 24 * 60
>                    , iFuture = scores 
>                    , iPast = []
>                    }

> test_deriveTransits = TestCase . assertEqual "test_deriveTransits" [54, 149] . deriveTransits 1245661800 2880 $ defaultSession {ra = 3.0}

> fixedPeriods = [ defaultPeriod { session = defaultSession { sId = 1 }
>                                , pScore = 1.0
>                                }
>                , defaultPeriod { session = defaultSession { sId = 2 }
>                                , pScore = 1.0
>                                }
>                , defaultPeriod { session = defaultSession { sId = 3 }
>                                , pScore = 1.0
>                                }
>                ]

> test_restoreFixedScore_replace = TestCase $ do
>      assertEqual "test_restoreFixedScore_replace_score" before_score . pScore . restoreFixedScore fixed $ after
>      assertEqual "test_restoreFixedScore_replace_session" after . restoreFixedScore fixed $ after
>      where
>        before_score = 20.0
>        after_score = 25.0
>        after = before { pScore = after_score }
>        fixed = fixedPeriods ++ [before]
>        before =  defaultPeriod { session = defaultSession { sId = 4 }
>                                , pScore = before_score
>                                }

> test_restoreFixedScore_not_replace = TestCase $ do
>      assertEqual "test_restoreFixedScore_not_replace_score" after_score . pScore . restoreFixedScore fixed $ after
>      assertEqual "test_restoreFixedScore_not_replace_session" after . restoreFixedScore fixed $ after
>      where
>        before_score = 20.0
>        after_score = 25.0
>        after = before { pScore = after_score }
>        fixed = fixedPeriods
>        before =  defaultPeriod { session = defaultSession { sId = 4 }
>                                , pScore = before_score
>                                }

> test_ToCandidate = TestCase $ do
>     assertEqual "test_ToCandidate" expected result
>   where
>     i = dItem { iId = defaultSession
>              , iMinDur = 2 -- quarters
>              , iMaxDur = 6 -- quarters
>              , iFuture = []
>              , iPast   = []
>              }
>     ss = [10.0, 10.1 .. 11.6]
>     ms = map Just ss
>     result = toCandidate i ms
>     expected = [Just defaultCandidate { cId = i
>                                       , cDuration = d
>                                       , cScore = s}
>                 | d <- [1..]
>                 | s <- ss]

> test_candidates1 = candidate_tests "test_candidates1" [0.5, 1.0 .. ] [0.0,0.0,0.0,3.0,5.0,7.5,10.5,14.0]

> test_candidates2 = candidate_tests "test_candidates2" [4.0,3.5,3.0,2.5,2.0,0.00001,1.0,0.5] [0.0,0.0,0.0,10.5,13.0]

> test_candidates3 = candidate_tests "test_candidates3" [4.0] []

> test_candidates4 = candidate_tests "test_candidates4" [] []

> candidate_tests name iPast expected = TestCase $ do
>     assertEqual name expected (map getCScore result)
>   where
>     i = dItem { iId = defaultSession
>              , iMinDur = 4 -- quarters
>              , iMaxDur = 8 -- quarters
>              , iFuture = []
>              , iPast   = iPast
>              }
>     result = candidates i
>     getCScore = cScore . fromMaybe defaultCandidate {cId = defaultSession}

> test_ToPeriod = TestCase $ do
>     assertEqual "test_ToPeriod" expected result
>   where
>     dt = fromGregorian 2006 11 8 12 0 0
>     dt1 = (8*quarter) `addMinutes'` dt
>     c = defaultCandidate { cId = defaultSession
>                           , cStart = 8 -- quarters
>                           , cDuration = 12 -- quarters
>                           , cScore = 20.0
>                           }
>     result = toPeriod dt dt1 c
>     expected = defaultPeriod { session = defaultSession
>                              , startTime = dt1
>                              , duration = quarter * 12
>                              , pScore = 20.0
>                              , pDuration = quarter * 12
>                              , pForecast = dt1
>                              }

TBF: are the candidate values for cStart & cDur correct?

> test_ToSchedule = TestCase $ do
>     assertEqual "test_ToSchedule" expected result
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     ft = 120 `addMinutes'` starttime
>     fixed = Period 0 defaultSession ft 30 0.0 Pending ft False 30
>     dts = quarterDateTimes starttime (8*60)
>     result = toSchedule dts [fixed]
>     -- TBF: are the candidate values for cStart & cDur correct?
>     candidates = map (\d -> Just $ Candidate defaultSession 0 0 d 0.0) [1, 2]
>     expected = (replicate 8 Nothing) ++ candidates ++ (replicate 22 Nothing) 

Same test, >1 fixed

> test_ToSchedule2 = TestCase $ do
>     assertEqual "test_ToSchedule" expected result
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     ft = 120 `addMinutes'` starttime
>     fixed1 = Period 0 defaultSession ft 30 0.0 Pending ft False 30
>     ft2 = 270 `addMinutes'` starttime
>     fixed2 = Period 0 defaultSession ft2 30 0.0 Pending ft2 False 30
>     dts = quarterDateTimes starttime (8*60)
>     result = toSchedule dts [fixed1,fixed2]
>     -- TBF: are the candidate values for cStart & cDur correct?
>     candidates1 = map (\d -> Just $ Candidate defaultSession 0 0 d 0.0) [1, 2]
>     candidates2 = map (\d -> Just $ Candidate defaultSession 0 0 d 0.0) [1, 2]
>     expected = (replicate 8 Nothing) ++ candidates1 ++ (replicate 8 Nothing)
>                                      ++ candidates2 ++ (replicate 12 Nothing)

Simplest test case of high-level 'pack': schedule a single candidate.

> test_Pack1 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate]
>         pack fs starttime duration [] [candidate]
>     assertEqual "test_Pack1_1" 1 (length periods')
>     assertEqual "test_Pack1_2" expPeriod (head periods')
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     duration = 12*60
>     candidate = defaultSession { sName = "singleton"
>                                , sAllottedT = 24*60
>                                , sAllottedS = 24*60
>                                , minDuration = 2*60
>                                , maxDuration = 6*60
>                                , frequency = 2.0
>                                , receivers = [[Rcvr1_2]]
>                                , project = testProject
>                                }
>     expStartTime = fromGregorian 2006 11 8 22 0 0
>     expPeriod = Period 0 candidate expStartTime 120 2.5292797 Pending expStartTime False 120

> test_PackTransit1 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate]
>         pack fs starttime duration [] [candidate]
>     assertEqual "test_PackTransit1_1" 2 (length periods')
>     assertEqual "test_PackTransit1_2" expPeriod1 (head periods')
>     assertEqual "test_PackTransit1_3" expPeriod2 (head . tail $ periods')
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     duration = 48*60
>     candidate = defaultSession { sName = "singleton"
>                                , sAllottedT = 24*60
>                                , sAllottedS = 24*60
>                                , minDuration = 2*60
>                                , maxDuration = 6*60
>                                , frequency = 2.0
>                                , receivers = [[Rcvr1_2]]
>                                , project = testProject
>                                , ra = 1.8
>                                , transit = Partial
>                                }
>     expStartTime1 = fromGregorian 2006 11 9 7 0 0
>     expStartTime2 = fromGregorian 2006 11 10 4 45 0
>     expPeriod1 = Period 0 candidate expStartTime1 360 3.105591 Pending expStartTime1 False 360
>     expPeriod2 = Period 0 candidate expStartTime2 360 3.1426687 Pending expStartTime2 False 360

> test_PackTransit2 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate]
>         pack fs starttime duration [] [candidate]
>     assertEqual "test_PackTransit2_1" 1 (length periods')
>     assertEqual "test_PackTransit2_2" expPeriod (head periods')
>   where
>     starttime = fromGregorian 2006 2 19 19 0 0
>     duration = 24*60
>     candidate = defaultSession { sName = "s3"
>                                , sAllottedT = 24*60
>                                , sAllottedS = 24*60
>                                , minDuration = 2*60
>                                , maxDuration = 6*60
>                                , frequency = 2.0
>                                , receivers = [[Rcvr1_2]]
>                                , project = testProject
>                                , ra = 1.6393563
>                                , transit = Partial
>                                }
>     expStartTime = fromGregorian 2006 2 19 22 30 0
>     expPeriod = Period 0 candidate expStartTime 360 3.0550473 Pending expStartTime False 360

TBF Is this just a random test that the results can be changed, or
is it testing some specific aspect of packing, i.e., what deos Bt
stand for?

> test_PackBt = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods1' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate1]
>         pack fs starttime duration [] [candidate1]
>     assertEqual "test_PackBt_1" 1 (length periods1')
>     assertEqual "test_PackBt_2" expPeriod1 (head periods1')
>     periods2' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate2]
>         pack fs starttime duration [] [candidate2]
>     assertEqual "test_PackBt_3" 4 (length periods2')
>     assertEqual "test_PackBt_4" expPeriod2_1 (head periods2')
>     assertEqual "test_PackBt_5" expPeriod2_2 (head . tail $ periods2')
>     periods3' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate3]
>         pack fs starttime duration [] [candidate3]
>     assertEqual "test_PackBt_6" 1 (length periods3')
>     assertEqual "test_PackBt_7" expPeriod3 (head periods3')
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     duration = 14*60
>     --duration = 12*60
>     candidate1 = defaultSession {sName = "singleton1"
>                                , sAllottedT = 24*60
>                                , sAllottedS = 24*60
>                                , minDuration = 135
>                                , maxDuration = 135
>                                , frequency = 2.0
>                                , receivers = [[Rcvr1_2]]
>                                , timeBetween = 0
>                                , project = testProject
>                                }
>     candidate2 = defaultSession {sName = "singleton2"
>                                , sAllottedT = 24*60
>                                , sAllottedS = 24*60
>                                , minDuration = 60
>                                , maxDuration = 60
>                                , timeBetween = 0
>                                , frequency = 2.0
>                                , receivers = [[Rcvr1_2]]
>                                , project = testProject
>                                }
>     candidate3 = defaultSession {sName = "singleton3"
>                                , sAllottedT = 24*60
>                                , sAllottedS = 24*60
>                                , minDuration = 60
>                                , maxDuration = 60
>                                , frequency = 2.0
>                                , receivers = [[Rcvr1_2]]
>                                , timeBetween = duration
>                                , project = testProject
>                                }
>     expStartTime1 = fromGregorian 2006 11 8 23 45 0
>     expStartTime2 = fromGregorian 2006 11 8 22 45 0
>     expStartTime3 = fromGregorian 2006 11 8 22 30 0
>     expStartTime4 = fromGregorian 2006 11 8 22  0 0
>     expPeriod1 = Period 0 candidate1 expStartTime1 135 2.8371732 Pending expStartTime1 False 135
>     expPeriod2_1 = Period 0 candidate2 expStartTime4 60 2.0238378 Pending expStartTime1 False 60
>     expPeriod2_2 = Period 0 candidate2 expStartTime2 60 3.1227193 Pending expStartTime2 False 60
>     expPeriod3 = Period 0 candidate3 expStartTime3 60 2.3257873 Pending expStartTime3 False 60

Create a long schedule from a reproducable randomly created set of sessions.
The main value of this test is to catch changes in the packing algorithm that 
produce changes in the final result.

> test_Pack2 = TestCase $ do
>     rt <- getRT
>     w <- getWeather . Just $ starttime 
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime sess
>         pack fs starttime dur [] sess
>     -- TBF: how to use 
>     assertEqual "test_Pack2" expPeriods periods'
>   where
>     sess = getOpenPSessions 
>     starttime = fromGregorian 2006 11 8 12 0 0
>     dur = 24*60
>     expPeriods = zipWith9 Period (repeat 0) ss times durs scores (repeat Pending) times (repeat False) durs
>       where
>         names = ["CV", "AS", "GB"]
>         ids = map getPSessionId names
>         ss  = map (\i -> defaultSession {sId = i}) ids
>         durs = [225,360,420]
>         times = [ starttime
>                 , fromGregorian 2006 11 8  12  0 0
>                 , fromGregorian 2006 11 8  15 45 0
>                 , fromGregorian 2006 11 9   5  0 0 ]
>         scores = [3.3544624, 3.2704132, 3.0974262]

Same test, but this time, stick some fixed periods in there.
TBF: the pre-scheduled periods scores are getting mangled in the final schedule.

Build up to this case with the simplest examples possible:

> test_PackWorkerSimple =
>     TestCase . assertEqual "test_PackWorkerSimple" result . packWorker fixed $ open
>   where
>     result = [ Candidate "B" 0 0 4 0.75
>              ]
>     fixed = replicate 4 Nothing
>     open  =  [item  "B" 2 4 (replicate 4 0.25) []]

Test PackWorker' w/ the input provided by test_PackWorkerSimple above:
These results are then used in test_Unwind3.

> test_PackWorker'Simple =
>     TestCase . assertEqual "test_PackWorker'Simple" result . packWorker' 0 fixed past $ open
>   where
>     result = [ Just (Candidate "B" 0 0 4 0.75)
>              , Just (Candidate "B" 0 0 3 0.50)
>              , Just (Candidate "B" 0 0 2 0.25)
>              , Nothing
>              , Nothing
>              ]
>     past  = [Nothing]
>     fixed = replicate 4 Nothing
>     open'  =  [item  "B" 2 4 (replicate 4 0.25) []]
>     open   = map step open'

Same as above, but with one time segment pre-scheduled:
TBF: These results are then used in test_Unwind4, which doesn't pass!!!!

> test_PackWorker'Simple2 =
>     TestCase . assertEqual "test_PackWorkerSimple2" result . packWorker' 0 fixed past $ open
>   where
>     result = [ Just (Candidate "F1" 0 0 1 0.0)
>              , Just (Candidate "B"  0 0 3 0.50)
>              , Just (Candidate "B"  0 0 2 0.25)
>              , Nothing
>              , Nothing
>              ]
>     past  = [Nothing]
>     fixed = [ Nothing
>             , Nothing
>             , Nothing --Just (Candidate "F1" 0 0 1 0.0)
>             , Just (Candidate "F1" 0 0 1 0.0)
>             ]
>     open'  =  [item  "B" 2 4 (replicate 4 0.25) []]
>     open   = map step open'

TBF: Scores not right due to negative score for F1 !!!

> test_PackWorker5 =
>     TestCase . assertEqual "test_PackWorker5" result . packWorker fixed $ open
>   where
>     result = [ Candidate "B"  0 0 8 1.925
>              , Candidate "F1" 0 8 2 0.0 -- bug: -1.925
>              ]
>     fixed  = [ Nothing                         --  0
>              , Nothing                         --  1
>              , Nothing                         --  2
>              , Nothing                         --  3
>              , Nothing                         --  4
>              , Nothing                         --  5
>              , Nothing                         --  6
>              , Nothing                         --  7
>              , Just (Candidate "F1" 0 0 1 0.0) --  8 bug: durrs = 1 & 2?
>              , Just (Candidate "F1" 0 0 2 0.0) --  9 
>              , Nothing                         --  10
>              , Nothing                         --  11
>              ]
>     open   = [ item "A" 12 24 (replicate 12 0.0) [] 
>              , item "B"  8 28 ((replicate 8 0.275)++[0.0,0.0,0.275,0.275]) []
>              , item "C"  9 28 ((replicate 8 0.257)++[0.0,0.0,0.0,0.0]) []
>              ]

> test_PackWorker'5 =
>     TestCase . assertEqual "test_PackWorker'5" result  . packWorker' 0 future past $ sessions
>   where
>     result = [  Nothing
>               , Nothing
>               , Just (Candidate "F1" 0 0 2 0.0)
>               , Just (Candidate "F1" 0 0 1 0.0)
>               , Just (Candidate "B" 0  0 8 1.925)
>               ] ++ replicate 8 Nothing 
>     future  = [Nothing                         --  0
>              , Nothing                         --  1
>              , Nothing                         --  2
>              , Nothing                         --  3
>              , Nothing                         --  4
>              , Nothing                         --  5
>              , Nothing                         --  6
>              , Nothing                         --  7
>              , Just (Candidate "F1" 0 0 1 0.0) --  8 bug: durrs = 1 & 2?
>              , Just (Candidate "F1" 0 0 2 0.0) --  9 
>              , Nothing                         --  10
>              , Nothing                         --  11
>              ]
>     past   = [Nothing]
>     open   = [ item "A" 12 24 (replicate 12 0.0) []
>              , item "B"  8 28 ((replicate 8 0.275)++[0.0,0.0,0.275,0.275]) []
>              , item "C"  9 28 ((replicate 8 0.257)++[0.0,0.0,0.0,0.0]) []
>              ]
>     sessions = map step open

> test_Pack3 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods <- runScoring w [] rt $ do
>         fs <- genScore starttime sess
>         pack fs starttime duration [fixed] sess
>     assertEqual "test_Pack3" expPeriods periods  
>   where
>     sess = concatMap sessions pTestProjects
>     starttime = fromGregorian 2006 11 8 12 0 0
>     ft1 = (4*60)  `addMinutes'` starttime
>     fixed = Period 0 defaultSession {sId = 0} ft1 60 0.0 Pending ft1 False 60
>     duration = 5*60
>     p1 = Period 0 defaultSession {sId = getPSessionId "CV"} starttime (4*60) 62.88887 Pending starttime False (4*60)
>     expPeriods  = [p1, fixed]

This is the original test that exposed many of the bugs with packing
around fixed periods.

> test_Pack4 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime sess
>         pack fs starttime duration fixed sess
>     assertEqual "test_Pack4" expPeriods periods'  
>   where
>     sess = getOpenPSessions --concatMap sessions pTestProjects 
>     ds = defaultSession
>     starttime = fromGregorian 2006 11 8 12 0 0
>     ft1 = (4*60)  `addMinutes'` starttime
>     ft2 = (10*60) `addMinutes'` starttime
>     dur1 = 2*60
>     dur2 = 4*60
>     fixed1 = Period 0 ds {sId = 1000, sName = "1000"} ft1 dur1 0.0 Pending ft1 False dur1
>     fixed2 = Period 0 ds {sId = 1001, sName = "1001"} ft2 dur2 0.0 Pending ft2 False dur2
>     fixed = [fixed1, fixed2]
>     duration = 24*60
>     expPeriods = zipWith9 Period (repeat 0) ss times durs scores (repeat Pending) times (repeat False) durs
>       where
>         names = ["CV", "WV", "GB"]
>         ids' = map getPSessionId names
>         ids  = [head ids']++[1000,1001]++(tail ids')
>         ss  = map (\i -> ds {sId = i}) ids
>         durs = [240, dur1, dur2, 360, 225]
>         --times = scanl (\dur dt -> addMinutes' dt dur) starttime durs
>         times = [starttime, ft1, ft2
>                , fromGregorian 2006 11 9 2 15 0
>                , fromGregorian 2006 11 9 8 15 0] 
>         -- TBF: don't tie pack tests to numerical scores
>         -- TBF: bug - second score should be zero!!!!
>         scores = [3.551355, 0.0, 0.0, 12.851959,6.554545]

Same as above, but with even more fixed periods

> test_Pack5 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime sess
>         pack fs starttime duration fixed sess
>     --assertEqual "test_Pack5" expPeriods periods'  
>     -- check that unsorted fixed periods are not a problem
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime sess
>         pack fs starttime duration unsortedFixed sess
>     --assertEqual "test_Pack5_unsorted" expPeriods periods'  
>     -- check that the score is an accurate accumulation
>     let scoreAt dt =  runScoring w [] rt $ do
>         sf <- genScore starttime sess
>         fs <- sf dt sCV
>         return $ eval fs
>     expScores <- mapM scoreAt dts
>     let expScore = (sum . tail $ expScores) / (fromIntegral $ length dts)
>     let epsilon = abs $ expScore - (pScore . head $ periods')
>     assertEqual "test_Pack5_score" True (epsilon < 1.0e-4)
>   where
>     sess = getOpenPSessions 
>     ds = defaultSession
>     starttime = fromGregorian 2006 11 8 12 0 0
>     ft1 = (4*60)  `addMinutes'` starttime
>     ft2 = (10*60) `addMinutes'` starttime
>     ft3 = (22*60) `addMinutes'` starttime
>     d1 = (2*60)
>     d2 = (4*60)
>     d3 = (2*60)
>     fixed1 = Period 0 ds {sId = 1000, sName = "1000"} ft1 d1 0.0 Pending ft1 False d1
>     fixed2 = Period 0 ds {sId = 1001, sName = "1001"} ft2 d2 0.0 Pending ft2 False d2
>     fixed3 = Period 0 ds {sId = 1002, sName = "1002"} ft3 d3 0.0 Pending ft3 False d3
>     fixed = [fixed1, fixed2, fixed3]
>     unsortedFixed = [fixed3, fixed1, fixed2]
>     duration = 24*60
>     open1 = Period 0 (ds {sId =  getPSessionId "CV"}) starttime 240 3.5829883 Pending starttime False 240
>     open2 = Period 0 (ds {sId = getPSessionId "WV"}) (fromGregorian 2006 11 9 3 45 0) 360 10.263064 Pending starttime False 360
>     expPeriods = [open1, fixed1, fixed2, open2, fixed3]
>     sCV = findPSessionByName "CV"
>     dts = [(i*quarter) `addMinutes'` starttime | i <- [0..((240 `div` quarter)-1)]]

revealed a bug where scores are turning negative in pact.

> test_Pack6 = TestCase $ do
>     rt <- getRT
>     w <- getWeather . Just $ starttime 
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime ss
>         pack fs starttime duration [] ss
>     let negScores = [p | p <- periods', pScore p < 0.0]
>     assertEqual "test_Pack6" [] negScores --expPeriods periods'  
>   where
>     starttime = fromGregorian 2006 1 1 0 0 0
>     duration = 24*60
>     s19 = defaultSession {sId = 19, sName = "19", periods = [], sAllottedT = 690, minDuration = 345, maxDuration = 435, timeBetween = 0, frequency = 8.378224, ra = 1.2237936, dec = 0.81245035, backup = False, receivers = [[Rcvr8_10]], enabled = False, authorized = False, grade = 4.0, band = X}
>     s3 =  defaultSession {sId = 3, sName = "3", periods = [], sAllottedT = 630, minDuration = 315, maxDuration = 450, timeBetween = 0, frequency = 14.540758, ra = 4.53959, dec = 3.422137e-2, backup = False, receivers = [[Rcvr12_18]], enabled = False, authorized = False, grade = 4.0, band = U}
>     ss = [s3, s19]

> test_Pack7 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime ss
>         pack fs starttime duration fixed ss
>     assertEqual "test_Pack6" 3 (numFixed periods') --expPeriods periods'  
>   where
>     starttime = fromGregorian 2006 10 6 3 0 0
>     duration = (20*60) + 30
>     ds = defaultSession {sId = 0, sName = "fixed"}
>     ss = getOpenPSessions
>     fixed1 = Period 0 ds (fromGregorian 2006 10 6  3  0 0) 255 0.0 Pending starttime False 255
>     fixed2 = Period 0 ds (fromGregorian 2006 10 6  9 45 0) 270 0.0 Pending starttime False 270
>     fixed3 = Period 0 ds (fromGregorian 2006 10 6 16 30 0) 255 0.0 Pending starttime False 255
>     fixed = [fixed1, fixed2, fixed3]
>     numFixed ps = length $ filter (\p -> ("fixed" == (sName . session $ p))) ps

Same as test_Pack1 except only 2 hours of sAllottedT instead of 24

> test_Pack8 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate]
>         pack fs starttime duration [] [candidate]
>     assertEqual "test_Pack8_1" 1 (length periods')
>     assertEqual "test_Pack8_2" expPeriod (head periods')
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     duration = 12*60
>     candidate = defaultSession { sName = "singleton"
>                                , sAllottedT = 2*60
>                                , sAllottedS = 2*60
>                                , minDuration = 2*60
>                                , maxDuration = 6*60
>                                , timeBetween = 24*60
>                                , frequency = 2.0
>                                , receivers = [[Rcvr1_2]]
>                                , project = testProject
>                                }
>     expStartTime = fromGregorian 2006 11 8 21 45 0
>     expPeriod = Period 0 candidate expStartTime 120 1.5167294 Pending expStartTime False 120

> test_Pack_overlapped_fixed = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime sess
>         pack fs starttime dur fixed sess
>     let expDurs = map duration expPeriods
>     let gotDurs = map duration periods'
>     assertEqual "test_Pack_overlapped_fixed_1" expDurs gotDurs
>     let expStarts = map (toSqlString . startTime) expPeriods
>     let gotStarts = map (toSqlString . startTime) periods'
>     assertEqual "test_Pack_overlapped_fixed_2" expStarts gotStarts
>     assertEqual "test_Pack" expPeriods periods'  
>   where
>     sess = getOpenPSessions 
>     ds = defaultSession
>     starttime = fromGregorian 2006 11 8 12 0 0
>     dur = 24*60
>     ft1 = ((-4)*60)  `addMinutes'` starttime -- -outside range
>     ft2 = (10*60) `addMinutes'` starttime -- inside range
>     ft3 = (22*60) `addMinutes'` starttime -- overlaps end boundary
>     ft4 = (24*60*3) `addMinutes'` starttime -- outside range
>     d = (4*60)
>     fixed1 = Period 0 ds {sId = 1000, sName = "1000"} ft1 d 0.0 Pending starttime False d
>     fixed2 = Period 0 ds {sId = 1001, sName = "1001"} ft2 d 0.0 Pending starttime False d
>     fixed3 = Period 0 ds {sId = 1002, sName = "1002"} ft3 d 0.0 Pending starttime False d
>     fixed4 = Period 0 ds {sId = 1003, sName = "1003"} ft4 d 0.0 Pending starttime False d
>     fixed = [fixed1, fixed2, fixed3, fixed4]
>     open1 = Period 0 (ds {sName = "CV", sId =  getPSessionId "CV"}) starttime 225 3.5666382 Pending starttime False 225
>     open2 = Period 0 (ds {sName = "AS", sId = getPSessionId "AS"}) (fromGregorian 2006 11 8 15 45 0) 360 3.373124 Pending starttime False 360
>     --open3 = Period 0 (ds {sName = "WV", sId = getPSessionId "WV"}) (fromGregorian 2006 11 9 2 15 0) 345 11.547832 Pending starttime False 360
>     open4 = Period 0 (ds {sName = "GB", sId = getPSessionId "GB"}) (fromGregorian 2006 11 9 5 0 0) 300 2.252018 Pending starttime False 300
>     expPeriods = [open1, open2, fixed2, open4, fixed3]

The beta test code runs packing using TScore, which is essentially a
random score generator.  So to match the two code bases we have to choose:
   1) find some way to use a test scorer here in haskell that produces the same scoring results
      * one way to do this is to add the list of desired scores (from the
        python code) to a funtion of time.  This function could then be used
        by a test scoring factor, which replaces 'score genScore'.
   2) write new unit tests in the beta code and try and match those results

Setup framework for duplicating python unit test; We'll try option 1) from above.

Here is how they are used in python's TScore:

> pythonTestStarttime = fromGregorian 2006 11 8 12 0 0

> getRandomScore    :: DateTime -> Score
> getRandomScore dt = cycle randomList !! hour
>   where
>     hour = (dt `diffMinutes` pythonTestStarttime) `div` 60

Now we can create our actual scoring factor

> randomScoreFactor :: ScoreFunc
> randomScoreFactor dt _ = factor "randomScore" . Just $ getRandomScore dt

> randomScore = score [randomScoreFactor]

Now we can use it in a test:

> test_RandomScore = TestCase $ do
>     w <- getWeather . Just $ dt
>     rt <- getRT
>     [(_, Just result)] <- runScoring w [] rt (randomScoreFactor dt defaultSession)
>     assertEqual "test_RandomScore" hr1Score result
>     [(_, Just result)] <- runScoring w [] rt (randomScoreFactor dt1 defaultSession)
>     assertEqual "test_RandomScore" hr1Score result
>     [(_, Just result)] <- runScoring w [] rt (randomScoreFactor dt2 defaultSession)
>     assertEqual "test_RandomScore" hr2Score result
>     [(_, Just result)] <- runScoring w [] rt (randomScoreFactor dt3 defaultSession)
>     assertEqual "test_RandomScore" hr3Score result
>   where
>     dt = pythonTestStarttime 
>     dt1 = 59 `addMinutes'` dt
>     dt2 = 61 `addMinutes'` dt
>     dt3 = 121 `addMinutes'` dt
>     hr1Score = 7.1331340485018409
>     hr2Score = 2.4934096782883213 
>     hr3Score = 7.6572318406256947 
> 

and test again:

> test_RandomScore2 = TestCase $ do
>     w <- getWeather . Just $ dt
>     rt <- getRT
>     scores <- mapM (score' w rt) times
>     assertEqual "test_RandomScore2" expScores scores
>   where
>     dt = pythonTestStarttime
>     times = [(15*q) `addMinutes'` dt | q <- [0..23]]
>     score' w rt dt = do
>         [(_, Just result)] <- runScoring w [] rt (randomScoreFactor dt defaultSession)
>         return result
>     expScores = concat [(replicate 4 x) | x <- (take 6 randomList)]

The next three tests are packing a single session into a duration.  The main
difference between tests is the packing duration:

Here, packing duration (6 hrs) == session maxDur (6 hrs)

> test_TestPack_pack1 = TestCase $ do
>     let periods = pack randomScore starttime duration [] [testSession]
>     w <- getWeather Nothing
>     rt <- getRT
>     periods' <- runScoring w [] rt $ periods
>     assertEqual "test_TestPack_pack1" [expPeriod] periods'
>   where
>     starttime = pythonTestStarttime --fromGregorian 2006 11 8 12 0 0
>     duration = 6*60
>     expScore = 133.01317 -- 5.542216 * (6*4) python: mean, here: sum
>     expPeriod = Period 0 testSession starttime  (6*60) expScore Pending starttime False (6*60)

> test_TestPack_pack1withHistory = TestCase $ do
>     let periods = pack randomScore starttime duration [fixed] [testSession]
>     w <- getWeather Nothing
>     rt <- getRT
>     periods' <- runScoring w [] rt $ periods
>     assertEqual "test_Pack1_history_2" [fixed,p2] periods'
>   where
>     starttime = pythonTestStarttime --fromGregorian 2006 11 8 12 0 0
>     duration = 6*60
>     fixedSession = defaultSession {sId = 1001}
>     fixed = Period 0 fixedSession starttime (3*60) 0.0 Pending starttime False (3*60)
>     p2 = Period 0 testSession ((3*60) `addMinutes'` starttime) (3*60) 0.0 Pending starttime False (3*60)
>     expScore = 133.01317 -- 5.542216 * (6*4) python: mean, here: sum


Here, packing duration (9 hrs) > session maxDur (6 hrs)

> test_TestPack_pack2 = TestCase $ do
>     let periods = pack randomScore starttime duration [] [testSession]
>     w <- getWeather Nothing
>     rt <- getRT
>     periods' <- runScoring w [] rt $ periods
>     assertEqual "test_TestPack_pack2" expPeriods periods'
>   where
>     starttime = pythonTestStarttime
>     starttime2 = (6*60) `addMinutes'` pythonTestStarttime 
>     duration = 9*60
>     expScore1 = 5.2450013
>     expScore2 = 4.7073417
>     expPeriod1 = Period 0 testSession starttime  (6*60) expScore1 Pending starttime False (6*60)
>     expPeriod2 = Period 0 testSession starttime2 (3*60) expScore2 Pending starttime False (3*60)
>     expPeriods = [expPeriod1, expPeriod2]

Here, packing duration (7 hrs) > session maxDur (6 hrs)

> test_TestPack_pack3 = TestCase $ do
>     let periods = pack randomScore starttime duration [] [testSession]
>     w <- getWeather Nothing
>     rt <- getRT
>     periods' <- runScoring w [] rt $ periods
>     assertEqual "test_TestPack_pack3" expPeriods periods'
>   where
>     starttime = pythonTestStarttime
>     starttime2 = (5*60) `addMinutes'` pythonTestStarttime 
>     duration = 7*60
>     expScore1 = 5.478371 -- 4.8132718634 * (2 * 4) python: mean, here: sum
>     expScore2 = 2.891944 -- 5.2704045983 * (5 * 4) 
>     expPeriod1 = Period 0 testSession starttime  (5*60) expScore1 Pending starttime False (5*60)
>     expPeriod2 = Period 0 testSession starttime2 (2*60) expScore2 Pending starttime False (2*60)
>     expPeriods = [expPeriod1, expPeriod2]

Now, we change the test by packing using TWO sessions:
Why are these resuts different from python? 
Basically, the expected solution has a total value
of 253.086512 whereas pack produces a solution with a value of
253.0865... the difference of 0.000012 is smaller than the new
epsilon, and so is "correct".

> test_TestPack_pack8 = TestCase $ do
>     let periods = pack randomScore starttime duration [] sessions 
>     w <- getWeather Nothing
>     rt <- getRT
>     periods' <- runScoring w [] rt $ periods
>     assertEqual "test_TestPack_pack8" 2 (length periods')
>     assertEqual "test_TestPack_pack8" expPeriods periods'
>   where
>     starttime = pythonTestStarttime
>     starttime2 = (6*60) `addMinutes'` pythonTestStarttime 
>     duration = 12*60
>     sessions = [testSession, testSession2]
>     expPeriod1 = Period 0 testSession2 starttime  (6*60) 5.2450013 Pending starttime False (6*60)
>     expPeriod2 = Period 0 testSession2 starttime2 (6*60) 4.889503 Pending starttime False (6*60)
>     expPeriods = [expPeriod1, expPeriod2]

Session data to pack:

> testProject = defaultProject {
>                                pAllottedT = 24*60
>                              , pAllottedS = 24*60
>                              }

> testSession  = defaultSession { sName = "singleton"
>                               , sAllottedT = 24*60
>                               , sAllottedS = 24*60
>                               , minDuration = 2*60
>                               , maxDuration = 6*60
>                               , project = testProject
>                               , frequency = 2.0
>                               , receivers = [[Rcvr1_2]]
>                              }

> testSession2 = defaultSession { sName = "second"
>                               , sAllottedT = 24*60
>                               , sAllottedS = 24*60
>                               , minDuration = 4*60
>                               , maxDuration = 8*60
>                               , project = testProject
>                               , frequency = 2.0
>                               , receivers = [[Rcvr1_2]]
>                               }

This expected result for the scoring of the session in 15-min
increments starting at starttime is taken from the ScoreTests.lhs

> --defaultPackSessionScores = (replicate 39 0.0) ++ 
> --                [3.2114944,3.2196305,3.2241328,2.8470442,3.0492089
> --                ,3.1139324,3.140008,3.187729,3.1933162]
> defaultPackSessionScores = (replicate 39 0.0) ++ 
>     [3.207287,3.2154124,3.219909,2.9261482,3.0864375,3.1245124,3.1356096,3.1832325,3.1888118]

This is the list of random numbers generated on the python side:

> randomList :: [Score]
> randomList = [7.1331340485018409, 2.4934096782883213, 7.6572318406256947, 5.046714456152789, 6.8446511584066618, 4.0781524926716983, 2.7252730440470305, 4.9143871264557122, 7.1636843840447568, 6.9446361985339973, 4.8230123064175849, 3.4473390258899297, 6.3350439397544198, 2.8207298844712874, 5.1058061299127466, 2.4985974189931035, 7.7080423642050198, 7.158122187895521, 2.5448732889679264, 5.0495207342152231, 2.6078672746394629, 4.5245768464312714, 4.6630376376658127, 4.9814692299184458, 3.9230995086978351, 3.124772317749299, 4.3545291190078173, 3.9156803332050671, 4.7649071147900779, 3.2957866086525902, 2.5266837648837353, 4.1279381958049832, 2.846086056357267, 7.9501503718916222, 5.0040843232701224, 6.2997134589932822, 2.8066033004458157, 3.3695805540586292, 7.1911605255609041, 5.1902010664882869, 6.0641085042114264, 3.1763244030347106, 5.5648306304235842, 4.8999056732443051, 4.8385202083992347, 7.821359353269389, 6.8195409456787983, 6.5591857654180128, 6.0411887011958951, 7.3687373406644578, 3.925478958851746, 6.1593368290906056, 6.2553947135435362, 2.9056687203569784, 2.0240197872208707, 7.0209407927591698, 7.5301119472143458, 6.5565343260879541, 7.4360080633805605, 5.5085736431979573, 3.2467669017752971, 2.4987826901996266, 2.5630089003230587, 2.7377288186642774, 5.1937658979896675, 3.8563605554932829, 4.4845133909067876, 2.130284284547066, 2.9602689950032728, 5.0062212541991116, 5.9676442585520162, 2.2570001356632856, 6.8411971054101093, 2.7563438298968426, 4.7276830627264941, 3.582367067990142, 3.9523405698149894, 6.8553413853738157, 5.0858901299373809, 4.1812254209649007, 7.2192209080032637, 6.4402617123341059, 6.6274389533438569, 6.3186576885368311, 4.6516827521820217, 4.0545997777170779, 6.865594825435954, 6.4993202696106422, 5.6206213173954378, 4.597663643263302, 5.3082458395844654, 6.4621121691512515, 2.8828921454728942, 2.8137617782918687, 4.6148063504374415, 3.3878648645377645, 5.3193346648162638, 2.1265679616606326, 4.3173508768876703, 2.477299227172681]
