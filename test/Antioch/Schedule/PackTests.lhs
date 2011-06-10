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
>   , test_Unwind4 
>   , test_Unwind_Big
>   , test_takeWhilen
>   , test_candidates1
>   , test_candidates2
>   , test_candidates3
>   , test_candidates4
>   , test_candidates5
>   , test_candidates6
>   , test_candidates7
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
>   , test_queryPast1
>   , test_queryPast2
>   , test_QueryPast_Big
>   , test_inFixed
>   , test_Madd1
>   , test_Madd2
>   , test_step
>   , test_Pack_overlapped_fixed
>   , test_Pack1
>   , test_Pack1'
>   , test_PackTransit1
>   , test_PackTransit2
>   , test_pack_timeBetween
>   , test_Pack2
>   , test_Pack3
>   , test_Pack4
>   , test_Pack5
>   , test_Pack6
>   , test_Pack7
>   , test_Pack8
>   , test_Pack9
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
>   , test_packWorker_timeBetween
>   , test_packWorker_example
>   , test_PackWorker5 
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

> item id min max future past = Item id 0 0 min max 1 enoughTime enoughTime 0 Optional [] future past

> dItem = Item {
>     iId = 0
>   , iSessId = 0
>   , iProj = 0
>   , iMinDur = 0
>   , iMaxDur = 0
>   , iOvhdDur = 1
>   , iSTimAv = enoughTime 
>   , iPTimAv = enoughTime
>   , iTimeBt = 0
>   , iTrType = Optional
>   , iTrnsts = []
>   , iFuture = []
>   , iPast   = []
>   }

Begin tests:

> test_packWorker_example = TestCase $ do
>     let cs = packWorker schedule items
>     assertEqual "test_packWorker_example" exp cs
>   where
>     i1 = item 1 2 4 [1.0, 5.0, 1.0, 1.0] []
>     i2 = item 2 2 4 [1.0, 1.0, 1.0, 2.0] []
>     schedule = take 4 $ repeat Nothing
>     items = [i1, i2]
>     c1 = Candidate 1 0 0 2 5.0
>     c2 = Candidate 2 0 2 2 2.0
>     exp = [c1, c2]

> test_packWorker_timeBetween = TestCase $ do
>     let dt = fromGregorian 2006 6 1 0 0 0
>     let dur = 3 * 24 * 60
>     let dts = quarterDateTimes dt dur
>     let sched = toSchedule dts . sort $ [] 
>     -- pack!
>     let cs = packWorker sched badItems 
>     -- none of the sessions should get packed more then once
>     -- or they're violating their timebetween
>     let itemIds = map (\i -> (sId . iId $ i)) badItems
>     --printList $ map (\c -> (sId . cId $ c, cStart c, cDuration c)) cs
>     assertEqual "test_pw_tb no repeating sessions" [] (filter (\id -> ((length $ filter (\c -> (sId . cId $ c) == id) cs)> 1)) itemIds)

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

> test_Unwind4 = TestCase . assertEqual "test_Unwind4" xs . unwind $ ys
>   where
>     xs = [Candidate "B" 0 0 3 0.75, Candidate "F1" 0 3 1 0.25]
>     ys = [     Just (Candidate "F1" 0 0 1 1.0)
>              , Just (Candidate "B" 0  0 3 0.75)
>              , Just (Candidate "B" 0  0 2 0.50)
>              , Nothing
>              , Nothing
>          ]

> test_Unwind_Big = TestCase $ do
>     let cs = unwind unwoundPackResult
>     assertEqual "test_Unwind_Bug" expCs cs
>   where
>     expCs = [Candidate {cId = 1, cProj = 1, cStart = 153, cDuration = 23, cScore = 504.36798}
>       ,Candidate {cId = 92, cProj = 92, cStart = 176, cDuration = 9, cScore = 19.291687}
>       ,Candidate {cId = 92, cProj = 92, cStart = 187, cDuration = 9, cScore = 19.232422}
>       ,Candidate {cId = 23, cProj = 23, cStart = 206, cDuration = 29, cScore = 83.62292}]


> unwoundPackResult :: [Maybe (Candidate Integer)]
> unwoundPackResult = n1s ++ [c23_235, c23_234] ++ n2s ++ [c92_198, c92_197, c92_196] ++ n3s ++ [c1_191, c1_190, c1_189, c1_188, c1_187] ++ c92_186_178 ++ c1_177_169 ++ n4s
>   where
>     n1s = take (289 - 235) $ repeat Nothing
>     n2s = take (233 - 198) $ repeat Nothing
>     n3s = take (195 - 191) $ repeat Nothing
>     n4s = take (169 - 0)   $ repeat Nothing
>     c23 = defaultCandidate {cId = 23, cProj = 23}
>     c23_235 = Just $ c23 {cDuration = 29, cScore = 83.62292}
>     c23_234 = Just $ c23 {cDuration = 28, cScore = 83.10764}
>     c92 = defaultCandidate {cId = 92, cProj = 92}
>     c92_198 = mkC c92 9 84.54625 
>     c92_197 = mkC c92 9 84.61264 
>     c92_196 = mkC c92 9 84.66811
>     c1 = defaultCandidate {cId = 1, cProj = 1}
>     c1_191  = mkC c1 23 65.248695
>     c1_190  = mkC c1 23 65.30844 
>     c1_189  = mkC c1 23 65.35883
>     c1_188  = mkC c1 23 65.40089
>     c1_187  = mkC c1 23 65.43569
>     c92_186_178 = map (mkC c92 9) c92scores
>     c92scores = [84.752,523.65967,520.6449,538.81995,84.592896,534.9577,84.45759,84.370804,529.3361]
>     c1_177_169 = map (mkC c1 23) c1scores
>     c1scores = [65.4588, 504.36798, 501.35605, 519.53546, 65.31374, 515.6858, 65.1952, 65.119965, 510.09985]
>     mkC c dur score = Just $ c {cDuration = dur, cScore = score}

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
>     xs = Just (Candidate 2 0 0 2 2.0)
>     past = [Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     sessions = map (step . step . step) testItems

> test_GetBest4 = TestCase . assertEqual "test_getBest4" xs . getBest 0 past $ sessions 
>   where
>     xs = Just (Candidate 1 0 0 4 3.0)
>     past = [Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     sessions = map (step . step . step . step) [testItem1]

> test_GetBest4' = TestCase . assertEqual "test_getBest4'" xs . getBest 0 past $ sessions 
>   where
>     xs = Just (Candidate 2 0 0 3 4.0)
>     past = [Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     sessions = map (step . step . step . step) testItems

> test_queryPast1 = TestCase $ do
>   --                         (sUsed pUsed separate, steps)
>   assertEqual "test_queryPast1_101" (0, 0, (-1), []) (queryPast testItem1 (drop 6 past) 1)
>   assertEqual "test_queryPast1_201" (0, 0, (-1), []) (queryPast testItem2 (drop 6 past) 1)
>   assertEqual "test_queryPast1_111" (0, 0, (-1), [1]) (queryPast testItem1 (drop 5 past) 1)
>   assertEqual "test_queryPast1_211" (0, 0, (-1), [1]) (queryPast testItem2 (drop 5 past) 1)
>   assertEqual "test_queryPast1_121" (2, 2, 0, [2])  (queryPast testItem1 (drop 4 past) 1)
>   assertEqual "test_queryPast1_221" (0, 2, (-1), [2]) (queryPast testItem2 (drop 4 past) 1)
>   assertEqual "test_queryPast1_131" (3, 3, 0, [3]) (queryPast testItem1 (drop 3 past) 1)
>   assertEqual "test_queryPast1_231" (0, 3, (-1), [3]) (queryPast testItem2 (drop 3 past) 1)
>   assertEqual "test_queryPast1_141" (2, 4, 2, [2,2]) (queryPast testItem1 (drop 2 past) 1)
>   assertEqual "test_queryPast1_241" (2, 4, 0, [2,2]) (queryPast testItem2 (drop 2 past) 1)
>   assertEqual "test_queryPast1_151" (2, 5, 3, [2,3]) (queryPast testItem1 (drop 1 past) 1)
>   assertEqual "test_queryPast1_251" (3, 5, 0, [2,3]) (queryPast testItem2 (drop 1 past) 1)
>   assertEqual "test_queryPast1_161" (2, 6, 4, [2,4]) (queryPast testItem1 past 1)
>   assertEqual "test_queryPast1_261" (4, 6, 0, [2,4]) (queryPast testItem2 past 1)
>   -- Notice how tests 122 ... 263 == 111 ... 261 (the previous ones).
>   -- This is because queryPast really only worries about the past, not the fact that we're
>   -- changing the duration of the item being tested, i.e., the duration only determines
>   -- where in the past we start the traversal.
>   assertEqual "test_queryPast1_122" (0, 0, (-1), [1])  (queryPast testItem1 (drop 4 past) 2)
>   assertEqual "test_queryPast1_222" (0, 0, (-1), [1]) (queryPast testItem2 (drop 4 past) 2)
>   assertEqual "test_queryPast1_132" (2, 2, 0, [2]) (queryPast testItem1 (drop 3 past) 2)
>   assertEqual "test_queryPast1_232" (0, 2, (-1), [2]) (queryPast testItem2 (drop 3 past) 2)
>   assertEqual "test_queryPast1_142" (3, 3, 0, [3]) (queryPast testItem1 (drop 2 past) 2)
>   assertEqual "test_queryPast1_242" (0, 3, (-1), [3]) (queryPast testItem2 (drop 2 past) 2)
>   assertEqual "test_queryPast1_152" (2, 4, 2, [2,2]) (queryPast testItem1 (drop 1 past) 2)
>   assertEqual "test_queryPast1_252" (2, 4, 0, [2,2]) (queryPast testItem2 (drop 1 past) 2)
>   assertEqual "test_queryPast1_162" (2, 5, 3, [2,3]) (queryPast testItem1 past 2)
>   assertEqual "test_queryPast1_262" (3, 5, 0, [2,3]) (queryPast testItem2 past 2)
>   assertEqual "test_queryPast1_153" (3, 3, 0, [3]) (queryPast testItem1 (drop 1 past) 3)
>   assertEqual "test_queryPast1_253" (0, 3, (-1), [3]) (queryPast testItem2 (drop 1 past) 3)
>   assertEqual "test_queryPast1_163" (2, 4, 2, [2,2]) (queryPast testItem1 past 3)
>   assertEqual "test_queryPast1_263" (2, 4, 0, [2,2]) (queryPast testItem2 past 3)
>   --
>   assertEqual "test_queryPast1_A" (3, 7, 0, [2,4,1]) (queryPast testItem1 past2 1)
>   assertEqual "test_queryPast1_B" (4, 7, 1, [2,4,1]) (queryPast testItem2 past2 1)
>   --
>   assertEqual "test_queryPast1_C" (2, 6, 5, [2,4,1]) (queryPast testItem1 past3 1)
>   assertEqual "test_queryPast1_D" (4, 6, 1, [2,4,1]) (queryPast testItem2 past3 1)
>   --
>   assertEqual "test_queryPast1_E" (2, 6, 5, [2,4,1]) (queryPast testItem1 past4 1)
>   assertEqual "test_queryPast1_F" (4, 6, 1, [2,4,1]) (queryPast testItem2 past4 1)
>   --
>   assertEqual "test_queryPast1_1hole" (2, 4, 3, [2,2,1]) (queryPast testItem1 hole 1)
>   assertEqual "test_queryPast1_2hole" (2, 4, 1, [2,2,1]) (queryPast testItem2 hole 1)
>     where
>       past = [Just (Candidate 2 0 0 4 10.0), Just (Candidate 2 0 0 3 8.0)
>              ,Just (Candidate 2 0 0 2 6.0),  Just (Candidate 1 0 0 3 3.0)
>              ,Just (Candidate 1 0 0 2 2.0),  Nothing, Nothing]
>       past2 = (Just (Candidate 1 0 0 1 12.0)):past
>       past3 = (Just (Candidate 1 0 0 1 9.0)):past
>       past4 = (Just (Candidate 1 0 0 2 9.0)):past
>       hole = [Nothing, Just (Candidate 2 0 0 2 6.0),  Just (Candidate 1 0 0 3 3.0)
>              ,Just (Candidate 1 0 0 2 2.0),  Nothing, Nothing]

> test_queryPast2 = TestCase $ do
>   --                           (sUsed pUsed separate, steps)
>   assertEqual "test_queryPast2_1_2_1" (1, 6, 5, [1,5]) (queryPast testItem1 [a2, Nothing, Nothing, Nothing, Nothing, b1, Nothing] 1)
>   assertEqual "test_queryPast2_1_1_2" (5, 6, 0, [1,5]) (queryPast testItem1 [a1, Nothing, Nothing, Nothing, Nothing, b2, Nothing] 1)
>   assertEqual "test_queryPast2_1N2_1" (1, 6, 6, [1,5,1]) (queryPast testItem1 [Nothing, a2, Nothing, Nothing, Nothing, Nothing, b1, Nothing] 1)
>   assertEqual "test_queryPast2_1N1_2" (5, 6, 1, [1,5,1]) (queryPast testItem1 [Nothing, a1, Nothing, Nothing, Nothing, Nothing, b2, Nothing] 1)
>   assertEqual "test_queryPast2_1_2N1" (1, 6, 6, [1,1,5]) (queryPast testItem1 [a2, Nothing, Nothing, Nothing, Nothing, Nothing, b1, Nothing] 1)
>   assertEqual "test_queryPast2_1_1N2" (5, 6, 0, [1,1,5]) (queryPast testItem1 [a1, Nothing, Nothing, Nothing, Nothing, Nothing, b2, Nothing] 1)
>   assertEqual "test_queryPast2_1N2N1" (1, 6, 7, [1,1,5,1]) (queryPast testItem1 [Nothing, a2, Nothing, Nothing, Nothing, Nothing, Nothing, b1, Nothing] 1)
>   assertEqual "test_queryPast2_1N1N2" (5, 6, 1, [1,1,5,1]) (queryPast testItem1 [Nothing, a1, Nothing, Nothing, Nothing, Nothing, Nothing, b2, Nothing] 1)
>   -- test with a following candidate with a larger score
>   assertEqual "test_queryPast2_1_2_1" (1, 6, 5, [1,5]) (queryPast testItem1 [a2, Nothing, Nothing, Nothing, Nothing, b1, Nothing] 1)
>   assertEqual "test_queryPast2_1_1_2" (1, 2, 6, [1,1,1,1,1,1,1]) (queryPast testItem1 [a1, b3, Nothing, Nothing, Nothing, Nothing, b1, Nothing] 1)
>     where
>       a1 = Just (Candidate 1 0 0 5 4.0)
>       a2 = Just (Candidate 2 0 0 5 4.0)
>       b1 = Just (Candidate 1 0 0 1 4.0)
>       b2 = Just (Candidate 2 0 0 1 4.0)
>       b3 = Just (Candidate 2 0 0 1 5.0)

> test_QueryPast_Big = TestCase $ do
>     -- see how session 92 does at a certain point 
>     let i = defaultItem {iId = 92}
>     let past = drop (289 - 195) unwoundPackResult
>     let dur = 9 -- the Candidate we are querying for is of duration 9
>     -- so, past that is queryed should look like:
>     -- Candidate 1 ...
>     -- Candidate 1 ...
>     -- Candidate 92 ...
>     -- ...
>     let (sUsed, pUsed, sep, vs) = queryPast i past dur
>     -- with the way queryPast traverses the past, it doesn't see
>     -- the second item 92, thus sUsed == 0
>     assertEqual "test_QueryPast_Big_1" (9, 0, 2) (sUsed, pUsed, sep)
>     

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
>     xs = [Just (Candidate 2 0 0 3 4.0), Just (Candidate 2 0 0 2 2.0)
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
>     xs = [Just (Candidate 2 0 0 3 4.0), Just (Candidate 2 0 0 2 2.0)
>          ,Just (Candidate 1 0 0 2 1.0), Nothing, Nothing]
>     ys = replicate 3 Nothing
>     zs = [Nothing, Nothing]
>     ws = map step [item 1 2 4 [1.0, 1.0, 1.0, 1.0, 1.0] [1.0]
>                  , item 2 2 4 [0.0, 2.0, 2.0, 2.0, 2.0] [0.0]]

> test_PackWorker'6_2 = TestCase . assertEqual "test_PackWorker'6_2" xs . packWorker' 0 ys zs $ ws
>   where
>     xs = [Just (Candidate 2 0 0 3 4.0), Just (Candidate 2 0 0 2 2.0)
>          ,Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     ys = replicate 2 Nothing
>     zs = [Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     ws = map step [item 1 2 4 [1.0, 1.0, 1.0, 1.0] [1.0, 1.0]
>                  , item 2 2 4 [2.0, 2.0, 2.0, 2.0] [0.0, 0.0]]

> test_PackWorker'6_3 = TestCase . assertEqual "test_PackWorker'6_3" xs . packWorker' 0 ys zs $ ws
>   where
>     xs = [Just (Candidate 2 0 0 3 4.0), Just (Candidate 1 0 0 3 3.0)
>          ,Just (Candidate 1 0 0 2 2.0), Nothing, Nothing]
>     ys = replicate 1 Nothing
>     zs = [Just (Candidate 1 0 0 3 3.0), Just (Candidate 1 0 0 2 2.0)
>          ,Nothing, Nothing]
>     ws = map step [item 1 2 4 [1.0, 1.0, 1.0] [1.0, 1.0, 1.0]
>                  , item 2 2 4 [2.0, 2.0, 2.0] [2.0, 0.0, 0.0]]

> test_getBest_for_PackWorker'6_3 = TestCase . assertEqual "test_best_for_PackWorker'6_3" result . getBest 0 zs $ ws
>   where
>     result = Just (Candidate {cId = 2, cProj = 0, cStart = 0, cDuration = 3, cScore = 4.0})
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
>     w <- getWeatherTest . Just $ starttime 
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
>     w <- getWeatherTest . Just $ starttime 
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
>     ft2 = (11*60) `addMinutes` starttime
>     fixed2 = Period 0 defaultSession ft2 30 0.0 Pending starttime False 30
>     dts = mask dts' (toSchedule dts' [fixed1, fixed2])
>     sess = testSession
>     scores = (take 44 defaultPackSessionScores) ++
>              [0.0,0.0,3.1394598,3.1463003]
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

> test_takeWhilen = TestCase $ do
>     assertEqual "test_takeWhile_1"  []      (takeWhilen 1 odd [])
>     assertEqual "test_takeWhile_2"  [2]     (takeWhilen 1 odd [2])
>     assertEqual "test_takeWhile_3"  [2]     (takeWhilen 1 odd [2,3,4,5])
>     assertEqual "test_takeWhile_4"  [1,3,4] (takeWhilen 1 odd [1,3,4,5])
>     assertEqual "test_takeWhile_5"  [1,3]   (takeWhilen 1 odd [1,3])
>     assertEqual "test_takeWhile_6"  [1,3,4] (takeWhilen 1 odd [1,3,4])
>     assertEqual "test_takeWhile_7"  []      (takeWhilen 2 odd [])
>     assertEqual "test_takeWhile_8"  [2]     (takeWhilen 2 odd [2])
>     assertEqual "test_takeWhile_9"  [2]     (takeWhilen 2 odd [2,3,4,5])
>     assertEqual "test_takeWhile_10" [1,3,4] (takeWhilen 2 odd [1,3,4,5])
>     assertEqual "test_takeWhile_11" [1,3]   (takeWhilen 2 odd [1,3])
>     assertEqual "test_takeWhile_12" [3,4]   (takeWhilen 2 odd [3,4])
>     assertEqual "test_takeWhile_13" [3,4,6] (takeWhilen 2 odd [3,4,6])
>     assertEqual "test_takeWhile_14" [3,4]   (takeWhilen 2 odd [3,4,5])
>     assertEqual "test_takeWhile_15" [3,4,6] (takeWhilen 2 odd [3,4,6,5])

> test_candidates1 = candidate_tests "test_candidates1" [0.5, 1.0 .. ] [0.0,0.0,0.0,3.0,5.0,7.5,10.5,14.0]

> test_candidates2 = candidate_tests "test_candidates2" [4.0,3.5,3.0,2.5,2.0,0.00001,1.0,0.5] [0.0,0.0,0.0,10.5,13.0,15.0]

> test_candidates3 = candidate_tests "test_candidates3" [4.0] []

> test_candidates4 = candidate_tests "test_candidates4" [] []

> test_candidates5 = candidate_tests "test_candidates5" [0.0, 0.5 .. ] []

> test_candidates6 = candidate_tests "test_candidates6" [3.5,3.0,2.5,2.0,0.00001,1.0,0.5,0.0] [0.0,0.0,0.0,9.0,11.0]

> test_candidates7 = candidate_tests "test_candidates7" [3.0,2.5,2.0,0.00001,1.0,0.5,0.0,0.0] [0.0,0.0,0.0,7.5]

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
>     dt1 = (8*quarter) `addMinutes` dt
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

> test_ToSchedule = TestCase $ do
>     assertEqual "test_ToSchedule" expected result
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     ft = 120 `addMinutes` starttime
>     fixed = Period 0 defaultSession ft 30 0.0 Pending ft False 30
>     dts = quarterDateTimes starttime (8*60)
>     result = toSchedule dts [fixed]
>     candidates = map (\d -> Just $ Candidate defaultSession 0 0 d 0.0) [1, 2]
>     expected = (replicate 8 Nothing) ++ candidates ++ (replicate 22 Nothing) 

Same test, >1 fixed

> test_ToSchedule2 = TestCase $ do
>     assertEqual "test_ToSchedule" expected result
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     ft = 120 `addMinutes` starttime
>     fixed1 = Period 0 defaultSession ft 30 0.0 Pending ft False 30
>     ft2 = 270 `addMinutes` starttime
>     fixed2 = Period 0 defaultSession ft2 30 0.0 Pending ft2 False 30
>     dts = quarterDateTimes starttime (8*60)
>     result = toSchedule dts [fixed1,fixed2]
>     candidates1 = map (\d -> Just $ Candidate defaultSession 0 0 d 0.0) [1, 2]
>     candidates2 = map (\d -> Just $ Candidate defaultSession 0 0 d 0.0) [1, 2]
>     expected = (replicate 8 Nothing) ++ candidates1 ++ (replicate 8 Nothing)
>                                      ++ candidates2 ++ (replicate 12 Nothing)

Simplest test case of high-level 'pack': schedule a single candidate.

> test_Pack1 = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
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
>     expStartTime = fromGregorian 2006 11 8 21 45 0
>     expPeriod = Period 0 candidate expStartTime 135 2.609496 Pending expStartTime False 135

> test_Pack1' = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate]
>         pack fs starttime duration [] [candidate]
>     assertEqual "test_Pack1'_1" 1 (length periods')
>     assertEqual "test_Pack1'_2" expPeriod (head periods')
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
>                                , oType = Vlbi
>                                }
>     expStartTime = fromGregorian 2006 11 8 21 30 0
>     expPeriod = Period 0 candidate expStartTime 150 2.609496 Pending expStartTime False 150

> test_PackTransit1 = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
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
>     w <- getWeatherTest . Just $ starttime 
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

> test_pack_timeBetween = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getRT
>     -- pack with the first session: just one period created
>     periods1' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate1]
>         pack fs starttime duration [] [candidate1]
>     assertEqual "test_PackBt_1" 1 (length periods1')
>     assertEqual "test_PackBt_2" expPeriod1 (head periods1')
>     -- the second session has a much smaller min/max duration:
>     -- but 4 periods of this session are packed
>     periods2' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate2]
>         pack fs starttime duration [] [candidate2]
>     assertEqual "test_PackBt_3" 4 (length periods2')
>     assertEqual "test_PackBt_4" expPeriod2_1 (head periods2')
>     assertEqual "test_PackBt_5" expPeriod2_2 (head . tail $ periods2')
>     -- now pack with the same session, but it has a big time between
>     -- and see how we're back to just one period scheduled.
>     periods3' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate3]
>         pack fs starttime duration [] [candidate3]
>     assertEqual "test_PackBt_6" 1 (length periods3')
>     assertEqual "test_PackBt_7" expPeriod3 (head periods3')
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     duration = 14*60
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
>     -- change the min/max duration
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
>     -- now add a big time between
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
>     expStartTime5 = fromGregorian 2006 11 8 21  45 0
>     expPeriod1 = Period 0 candidate1 expStartTime1 135 2.8371732 Pending expStartTime1 False 135
>     expPeriod2_1 = Period 0 candidate2 expStartTime5 60 2.0238378 Pending expStartTime1 False 60
>     expPeriod2_2 = Period 0 candidate2 expStartTime2 60 3.1227193 Pending expStartTime2 False 60
>     expPeriod3 = Period 0 candidate3 expStartTime3 60 2.3257873 Pending expStartTime3 False 60

Create a long schedule from a reproducable randomly created set of sessions.
The main value of this test is to catch changes in the packing algorithm that 
produce changes in the final result.

> test_Pack2 = TestCase $ do
>     rt <- getRT
>     w <- getWeatherTest . Just $ starttime 
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime sess
>         pack fs starttime dur [] sess
>     assertEqual "test_Pack2" expPeriods periods'
>   where
>     sess = getOpenPSessions 
>     starttime = fromGregorian 2006 11 8 12 0 0
>     dur = 24*60
>     expPeriods = zipWith9 Period (repeat 0) ss times durs scores (repeat Pending) times (repeat False) durs
>       where
>         names = ["CV", "AS", "CV"]
>         ids = map getPSessionId names
>         ss  = map (\i -> defaultSession {sId = i}) ids
>         durs = [210,390,210]
>         times = [ starttime
>                 , fromGregorian 2006 11 8  15 30 0
>                 , fromGregorian 2006 11 9   8 30 0 ]
>         scores = [2.9446945, 3.260362, 3.2440414]

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

This test demonstrates how unwind (called after packWorker' inside of packWorker)
will mangle the scores for the pre-scheduled periods we pack around.
That's okay, we make up for it in 'pack' using restoreFixed.

> test_PackWorker5 =
>     TestCase . assertEqual "test_PackWorker5" result . packWorker fixed $ open
>   where
>     result = [ Candidate "B"  0 0 8 1.925
>              , Candidate "F1" 0 8 2 (-1.925)
>              ]
>     fixed  = [ Nothing                         --  0
>              , Nothing                         --  1
>              , Nothing                         --  2
>              , Nothing                         --  3
>              , Nothing                         --  4
>              , Nothing                         --  5
>              , Nothing                         --  6
>              , Nothing                         --  7
>              , Just (Candidate "F1" 0 0 1 0.0) --  8 
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
>               , Just (Candidate "F1"  0 0 1 0.0)
>               , Just (Candidate "B" 0 0 8 1.925)
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
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getRT
>     periods <- runScoring w [] rt $ do
>         fs <- genScore starttime sess
>         pack fs starttime duration [fixed] sess
>     assertEqual "test_Pack3" expPeriods periods  
>   where
>     sess = concatMap sessions pTestProjects
>     starttime = fromGregorian 2006 11 8 12 0 0
>     ft1 = (4*60)  `addMinutes` starttime
>     fixed = Period 0 defaultSession {sId = 0} ft1 60 0.0 Pending ft1 False 60
>     duration = 5*60
>     p1 = Period 0 defaultSession {sId = getPSessionId "CV"} starttime (4*60) 62.88887 Pending starttime False (4*60)
>     expPeriods  = [p1, fixed]

This is the original test that exposed many of the bugs with packing
around fixed periods.

> test_Pack4 = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getRT
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime sess
>         pack fs starttime duration fixed sess
>     assertEqual "test_Pack4" expPeriods periods'  
>   where
>     sess = getOpenPSessions --concatMap sessions pTestProjects 
>     ds = defaultSession
>     starttime = fromGregorian 2006 11 8 12 0 0
>     ft1 = (4*60)  `addMinutes` starttime
>     ft2 = (10*60) `addMinutes` starttime
>     dur1 = 2*60
>     dur2 = 4*60
>     fixed1 = Period 0 ds {sId = 1000, sName = "1000"} ft1 dur1 0.0 Pending ft1 False dur1
>     fixed2 = Period 0 ds {sId = 1001, sName = "1001"} ft2 dur2 0.0 Pending ft2 False dur2
>     fixed = [fixed1, fixed2]
>     duration = 24*60
>     expPeriods = zipWith9 Period (repeat 0) ss times durs scores (repeat Pending) times (repeat False) durs
>       where
>         names = ["CV", "CV", "CV"]
>         ids' = map getPSessionId names
>         ids  = [head ids']++[1000]++[head . tail $ ids']++[1001]++[last ids']
>         ss  = map (\i -> ds {sId = i}) ids
>         durs = [240, dur1, 135, dur2, 210]
>         times = [starttime
>                , ft1
>                , fromGregorian 2006 11 8 18 0 0
>                , ft2
>                , fromGregorian 2006 11 9 8 30 0] 
>         scores = [2.9998863, 0.0, 1.5633384, 0.0, 3.2440414]

Same as above, but with even more fixed periods

> test_Pack5 = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
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
>     ft1 = (4*60)  `addMinutes` starttime
>     ft2 = (10*60) `addMinutes` starttime
>     ft3 = (22*60) `addMinutes` starttime
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
>     dts = [(i*quarter) `addMinutes` starttime | i <- [0..((240 `div` quarter)-1)]]

revealed a bug where scores are turning negative in pact.

> test_Pack6 = TestCase $ do
>     rt <- getRT
>     w <- getWeatherTest . Just $ starttime 
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
>     w <- getWeatherTest . Just $ starttime 
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
>     w <- getWeatherTest . Just $ starttime 
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
>     expStartTime = fromGregorian 2006 11 8 22 0 0
>     expPeriod = Period 0 candidate expStartTime 120 1.2334107 Pending expStartTime False 120

> test_Pack9 = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getRT
>     -- pack a single session that is always up
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate]
>         pack fs starttime dur [] [candidate]
>     assertEqual "test_Pack9_1" 1 (length periods')
>     assertEqual "test_Pack9_2" (2*60) (duration (head periods'))
>     -- now make sure it makes way for a fixed in the beginning
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime [candidate]
>         pack fs starttime dur [fixedP] [candidate]
>     assertEqual "test_Pack9_3" 2 (length periods')
>     assertEqual "test_Pack9_4" (2*60) (duration (last periods'))
>     assertEqual "test_Pack9_5" "candidate" (sName . session . last $ periods')
>     -- now make sure it can handle having periods w/ out billed time 
>     periods' <- runScoring w [] rt $ do
>         fs <- genScore starttime [c2]
>         pack fs starttime dur [p2, fixedP] [c2]
>     assertEqual "test_Pack9_3" 2 (length periods')
>     assertEqual "test_Pack9_4" (2*60) (duration (last periods'))
>     assertEqual "test_Pack9_5" "candidate" (sName . session . last $ periods')
>     -- Uncomment this too see the BUG!!! WTF.
>     -- From the Story: https://www.pivotaltracker.com/story/show/13914635
>     {-
>     Pack's queryPast function looks at the Candidates durations - but if these Candidates are from pre-scheduled periods, those pre-scheduled periods durations may not be equal to pDuration (the billed time).
>     This will be very rare, so I don't think it's worth tackling, but worth noting.
>     -}
>     -- now push the sessions periods w/ out billed time to w/ in the
>     -- range of pack (put it where that fixed period was), and watch
>     -- the bug: pack thinks the session has no more time left
>     --periods' <- runScoring w [] rt $ do
>     --    fs <- genScore starttime [c2]
>     --    pack fs starttime dur [p3] [c3]
>     --print "pack w/ fixed: "
>     --print periods'
>     --assertEqual "test_Pack9_3" 2 (length periods')
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     dur = 12*60
>     candidate' = defaultSession { minDuration = 2*60
>                                , maxDuration = 2*60
>                                , frequency = 2.0
>                                , receivers = [[Rcvr1_2]]
>                                , project = testProject
>                                , sAllottedT = 4*60
>                                , sAllottedS = 4*60
>                                , ra = 0.0
>                                , dec = 1.5 -- always up
>                                , sName = "candidate"
>                                , sId = 101
>                                }
>     p1 = defaultPeriod { session = candidate'
>                       , startTime = fromGregorian 2006 10 1 0 0 0
>                       , duration = 2*60
>                       , pDuration = 2*60
>                       }
>     candidate = makeSession candidate' [] [p1]
>     fixedP = defaultPeriod { startTime = starttime
>              , duration = 2*60 
>              , pDuration = 2*60
>              }
>     p2 = p1 { startTime = fromGregorian 2006 11 1 0 0 0, pDuration = 0}
>     c2 = makeSession candidate' [] [p1, p2]
>     p3 = p1 { startTime = starttime, pDuration = 0}
>     c3 = makeSession candidate' [] [p1, p3]




> test_Pack_overlapped_fixed = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
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
>     ft1 = ((-4)*60)  `addMinutes` starttime -- -outside range
>     ft2 = (10*60) `addMinutes` starttime -- inside range
>     ft3 = (22*60) `addMinutes` starttime -- overlaps end boundary
>     ft4 = (24*60*3) `addMinutes` starttime -- outside range
>     d = (4*60)
>     fixed1 = Period 0 ds {sId = 1000, sName = "1000"} ft1 d 0.0 Pending starttime False d
>     fixed2 = Period 0 ds {sId = 1001, sName = "1001"} ft2 d 0.0 Pending starttime False d
>     fixed3 = Period 0 ds {sId = 1002, sName = "1002"} ft3 d 0.0 Pending starttime False d
>     fixed4 = Period 0 ds {sId = 1003, sName = "1003"} ft4 d 0.0 Pending starttime False d
>     fixed = [fixed1, fixed2, fixed3, fixed4]
>     open1 = Period 0 (ds {sName = "CV", sId =  getPSessionId "CV"}) starttime 210 9165013 Pending starttime False 210
>     open2 = Period 0 (ds {sName = "AS", sId = getPSessionId "AS"}) (fromGregorian 2006 11 8 15 30 0) 390 3.2747638 Pending starttime False 390
>     expPeriods = [open1, open2, fixed2, fixed3]

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
>     w <- getWeatherTest . Just $ dt
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
>     dt1 = 59 `addMinutes` dt
>     dt2 = 61 `addMinutes` dt
>     dt3 = 121 `addMinutes` dt
>     hr1Score = 7.1331340485018409
>     hr2Score = 2.4934096782883213 
>     hr3Score = 7.6572318406256947 
> 

and test again:

> test_RandomScore2 = TestCase $ do
>     w <- getWeatherTest . Just $ dt
>     rt <- getRT
>     scores <- mapM (score' w rt) times
>     assertEqual "test_RandomScore2" expScores scores
>   where
>     dt = pythonTestStarttime
>     times = [(15*q) `addMinutes` dt | q <- [0..23]]
>     score' w rt dt = do
>         [(_, Just result)] <- runScoring w [] rt (randomScoreFactor dt defaultSession)
>         return result
>     expScores = concat [(replicate 4 x) | x <- (take 6 randomList)]

The next three tests are packing a single session into a duration.  The main
difference between tests is the packing duration:

Here, packing duration (6 hrs) == session maxDur (6 hrs)

> test_TestPack_pack1 = TestCase $ do
>     let periods = pack randomScore starttime duration [] [testSession]
>     w <- getWeatherTest Nothing
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
>     w <- getWeatherTest Nothing
>     rt <- getRT
>     periods' <- runScoring w [] rt $ periods
>     assertEqual "test_Pack1_history_2" [fixed,p2] periods'
>   where
>     starttime = pythonTestStarttime --fromGregorian 2006 11 8 12 0 0
>     duration = 6*60
>     fixedSession = defaultSession {sId = 1001}
>     fixed = Period 0 fixedSession starttime (3*60) 0.0 Pending starttime False (3*60)
>     p2 = Period 0 testSession ((3*60) `addMinutes` starttime) (3*60) 0.0 Pending starttime False (3*60)
>     expScore = 133.01317 -- 5.542216 * (6*4) python: mean, here: sum


Here, packing duration (9 hrs) > session maxDur (6 hrs)

> test_TestPack_pack2 = TestCase $ do
>     let periods = pack randomScore starttime duration [] [testSession]
>     w <- getWeatherTest Nothing
>     rt <- getRT
>     periods' <- runScoring w [] rt $ periods
>     assertEqual "test_TestPack_pack2" expPeriods periods'
>   where
>     starttime = pythonTestStarttime
>     starttime2 = (6*60) `addMinutes` pythonTestStarttime 
>     duration = 9*60
>     expScore1 = 5.2450013
>     expScore2 = 4.7073417
>     expPeriod1 = Period 0 testSession starttime  (6*60) expScore1 Pending starttime False (6*60)
>     expPeriod2 = Period 0 testSession starttime2 (3*60) expScore2 Pending starttime False (3*60)
>     expPeriods = [expPeriod1, expPeriod2]

Here, packing duration (7 hrs) > session maxDur (6 hrs)

> test_TestPack_pack3 = TestCase $ do
>     let periods = pack randomScore starttime duration [] [testSession]
>     w <- getWeatherTest Nothing
>     rt <- getRT
>     periods' <- runScoring w [] rt $ periods
>     assertEqual "test_TestPack_pack3" expPeriods periods'
>   where
>     starttime = pythonTestStarttime
>     starttime2 = (5*60) `addMinutes` pythonTestStarttime 
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
>     w <- getWeatherTest Nothing
>     rt <- getRT
>     periods' <- runScoring w [] rt $ periods
>     assertEqual "test_TestPack_pack8" 2 (length periods')
>     assertEqual "test_TestPack_pack8" expPeriods periods'
>   where
>     starttime = pythonTestStarttime
>     starttime2 = (6*60) `addMinutes` pythonTestStarttime 
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
> defaultPackSessionScores = (replicate 40 0.0) ++ 
> --    [3.1126366,3.1218858,2.33521,2.6951327,2.863466,3.0243123,3.1388562,3.145695]
>       [3.113275,3.1225264,2.3510582,2.7080994,2.8739493,3.030795,3.1394598,3.1463003]

This is the list of random numbers generated on the python side:

> randomList :: [Score]
> randomList = [7.1331340485018409, 2.4934096782883213, 7.6572318406256947, 5.046714456152789, 6.8446511584066618, 4.0781524926716983, 2.7252730440470305, 4.9143871264557122, 7.1636843840447568, 6.9446361985339973, 4.8230123064175849, 3.4473390258899297, 6.3350439397544198, 2.8207298844712874, 5.1058061299127466, 2.4985974189931035, 7.7080423642050198, 7.158122187895521, 2.5448732889679264, 5.0495207342152231, 2.6078672746394629, 4.5245768464312714, 4.6630376376658127, 4.9814692299184458, 3.9230995086978351, 3.124772317749299, 4.3545291190078173, 3.9156803332050671, 4.7649071147900779, 3.2957866086525902, 2.5266837648837353, 4.1279381958049832, 2.846086056357267, 7.9501503718916222, 5.0040843232701224, 6.2997134589932822, 2.8066033004458157, 3.3695805540586292, 7.1911605255609041, 5.1902010664882869, 6.0641085042114264, 3.1763244030347106, 5.5648306304235842, 4.8999056732443051, 4.8385202083992347, 7.821359353269389, 6.8195409456787983, 6.5591857654180128, 6.0411887011958951, 7.3687373406644578, 3.925478958851746, 6.1593368290906056, 6.2553947135435362, 2.9056687203569784, 2.0240197872208707, 7.0209407927591698, 7.5301119472143458, 6.5565343260879541, 7.4360080633805605, 5.5085736431979573, 3.2467669017752971, 2.4987826901996266, 2.5630089003230587, 2.7377288186642774, 5.1937658979896675, 3.8563605554932829, 4.4845133909067876, 2.130284284547066, 2.9602689950032728, 5.0062212541991116, 5.9676442585520162, 2.2570001356632856, 6.8411971054101093, 2.7563438298968426, 4.7276830627264941, 3.582367067990142, 3.9523405698149894, 6.8553413853738157, 5.0858901299373809, 4.1812254209649007, 7.2192209080032637, 6.4402617123341059, 6.6274389533438569, 6.3186576885368311, 4.6516827521820217, 4.0545997777170779, 6.865594825435954, 6.4993202696106422, 5.6206213173954378, 4.597663643263302, 5.3082458395844654, 6.4621121691512515, 2.8828921454728942, 2.8137617782918687, 4.6148063504374415, 3.3878648645377645, 5.3193346648162638, 2.1265679616606326, 4.3173508768876703, 2.477299227172681]

> badItems :: [Item Session]
> badItems = [Item {iId = defaultSession {sId = 1}, iSessId = 1, iProj = 1, iMinDur = 23, iMaxDur = 23, iOvhdDur = 1, iSTimAv = 23, iPTimAv = 23, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.8591611,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.892247,2.9061723,2.917584,2.927072,2.9338834,2.9409533,2.9469898,2.9521842,2.9568286,2.960693,2.964056,2.9669874,2.9692965,2.9715154,2.9734364,2.97509,2.9768338,2.9780762,2.9791095,2.979952,2.9807348,2.9811115,2.9814065,2.9814065,2.9817677,2.9814382,2.9808013,2.9802685,2.979565,2.9784238,2.977073,2.9754891,2.973803,2.97169,2.9692643,2.9664702,2.964767,2.9612782,2.957255,2.9526162,2.9443092,2.9378924,2.9303398,2.9213643,2.907713,2.8948965,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.3802057,0.75908905,1.2067287,1.5062361,1.9802306,2.297395,2.5332446,2.6959705,2.8366847,2.8731458,2.8815436,2.8865664,2.8999035,2.9023583,2.9056165,2.90843,2.9182522,2.920235,2.921866,2.92278,2.9756682,2.9760036,2.9761448,2.9760036,2.9954252,2.9951518,2.9948218,2.9942117,2.9905174,2.9896233,2.9885583,2.987318,2.961254,2.9589972,2.956407,2.953427,2.934911,2.9302692,2.9249241,2.9187405,2.8877554,2.87779,2.845655,2.6923628,2.2788653,1.7468405], iPast = []}
>     , Item {iId = defaultSession {sId = 2}, iSessId = 2, iProj = 2, iMinDur = 8, iMaxDur = 12, iOvhdDur = 1, iSTimAv = 20, iPTimAv = 20, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [2.728294,2.728294,2.7275984,2.7275984,2.7252429,2.7226794,2.7225943,2.7166972,2.713297,2.7095292,2.7000067,2.6886103,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.6966696,2.7073288,2.7114232,2.718423,2.7214353,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>     , Item {iId = defaultSession {sId = 5}, iSessId = 5, iProj = 5, iMinDur = 4, iMaxDur = 6, iOvhdDur = 1, iSTimAv = 10, iPTimAv = 10, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.7627802,3.7663183,3.7681532,3.771406,3.7744544,3.7773113,3.7816825,3.784166,3.786512,3.7887251,3.7969038,3.7986836,3.8003778,3.8019836,3.8049142,3.8076959,3.808977,3.8102121,3.8143623,3.815391,3.8163817,3.8173249,3.8167374,3.8175724,3.8183548,3.8191152,3.821006,3.8223298,3.8229485,3.8235366,3.8231506,3.823724,3.824263,3.824263,3.8238628,3.8238628,3.824371,3.824371,3.8237567,3.8237567,3.8232424,3.8232424,3.823586,3.823586,3.823039,3.8224573,3.8216062,3.8209684,3.8203127,3.8196175,3.8183808,3.8176115,3.8168209,3.8151007,3.815048,3.8140926,3.8120503,3.8109577,3.8106067,3.8094091,3.8068402,3.8054583,3.8043222,3.802796,3.7994938,3.7976942,3.7973418,3.795373,3.7932787,3.7910562,3.791104,3.7886636,3.7860541,3.7832787,3.779945,3.779945,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.7840784,3.786997,3.7915208,3.7940207,3.7940207,3.796357,3.7990246,3.801086,3.804861,3.8065922,3.8099644,3.8114693,3.8129034,3.8142684,3.8172288,3.8183923,3.8194978,3.8215437,3.8233168,3.8242168,3.8258882,3.826668,3.8257504,3.8264647,3.827135,3.8284125,3.829562,3.8301194,3.8306518,3.8311684,3.8323085,3.8323085,3.832767,3.8332083,3.8337336,3.8337336,3.83415,3.83415,3.835105,3.8346941,3.8346941,3.8346941,3.8351338,3.8351338,3.8347032,3.8342454,3.8337998,3.8333013,3.8327851,3.8322384,3.8325858,3.83139,3.830743,3.8300686,3.8302422,3.8287513,3.827958,3.8271198,3.8274724,3.8256319,3.824638,3.8236008,3.8218179,3.8206153,3.8193471,3.8180006,3.8169715,3.8154802,3.8139,3.8122187,3.8109334,3.809049,3.8070328,3.8048906,3.8020046,3.8020046,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.796658,3.796658,3.7991567,3.7878654,3.7904127,3.7927902,3.7950418,3.788683,3.7908704,3.7929323,3.794877,3.7836595,3.7856615,3.7893765,3.7910905,3.7892246,3.7908404,3.7938476,3.795244,3.7861013,3.7889068,3.7902172,3.7914615,3.7792227,3.781729,3.7829084,3.7840295,3.7771196,3.778239,3.7792976,3.7803247,3.7699032,3.770944,3.770944,3.7719445,3.8062146,3.8062146,3.8068552,3.8068552,3.8043172,3.8036766,3.8036766,3.8036766,3.8018498,3.8011236,3.8011236,3.8003628,3.8045528,3.8037753,3.8029578,3.802105,3.8030133,3.8020577,3.801048,3.8000023,3.8175018,3.8165712,3.8156037,3.8135262,3.8241313,3.8231618,3.8221388,3.8199372,3.8175015,3.8162436,3.8149157,3.8135023,3.7976687,3.795693,3.7935877,3.7913535,3.7802238,3.7774212,3.7774212,3.7744374,3.762709,0.0,0.0,0.0,0.0,0.0], iPast = []} 
>      , Item {iId = defaultSession {sId = 8}, iSessId = 8, iProj = 8, iMinDur = 6, iMaxDur = 18, iOvhdDur = 1, iSTimAv = 24, iPTimAv = 24, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.6383355,3.9240475,4.233555,4.248047,4.257944,4.2755117,4.2982926,4.305403,4.312006,4.312006,4.319992,4.319992,4.319992,4.319992,4.332672,4.3267283,4.3203273,4.3134212,4.3104515,4.2939,4.284476,4.262787,4.2357373,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.264351,4.2859163,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []} 
>     , Item {iId = defaultSession {sId = 12}, iSessId = 12, iProj = 12, iMinDur = 18, iMaxDur = 24, iOvhdDur = 1, iSTimAv = 42, iPTimAv = 42, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [2.9624164,2.963096,2.9648197,2.9650939,2.9653583,2.9653583,2.9645724,2.9645724,2.9643078,2.9640207,2.9623394,2.961648,2.9608946,2.9596186,2.9553084,2.9535472,2.9522357,2.9500258,2.9484687,2.9455867,2.9422452,2.939716,2.9366176,2.9315872,2.9255762,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.9295008,2.9355273,2.9405282,2.9447227,2.9498706,2.952832,2.9553788,2.957586,2.9587317,2.960485,2.9620225,2.962928,2.9644375,2.965162,2.9658117,2.9664044,2.9666388,2.9668984,2.9671497,2.9671497,2.9677417,2.9677417,2.9674923,2.9669595,2.9667602,2.966127,2.9654217,2.9646575,2.9644294,2.9634585,2.9618282,2.9599574,2.9588013,2.9572191,2.9545283,2.9513874,2.9480462,2.9436896,2.9384627,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.950129,2.9543152,2.9578001,2.9607332,2.9635391,2.965662,2.9669073,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>     , Item {iId = defaultSession {sId = 21}, iSessId = 21, iProj = 21, iMinDur = 9, iMaxDur = 9, iOvhdDur = 1, iSTimAv = 9, iPTimAv = 9, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.9113934,3.9163058,3.9226542,3.9280033,3.9302888,3.9344347,3.938002,3.9410992,3.9449253,3.947223,3.9492261,3.9504118,3.9517045,3.9531,3.9543052,3.9553556,3.9556546,3.9563944,3.957017,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>    ,  Item {iId = defaultSession {sId = 23}, iSessId = 23, iProj = 23, iMinDur = 28, iMaxDur = 29, iOvhdDur = 1, iSTimAv = 57, iPTimAv = 57, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.2806437,3.2971883,3.3061495,3.3138587,3.3228135,3.328536,3.331134,3.3358846,3.3447342,3.3466327,3.3484263,3.3501208,3.354343,3.3558612,3.3558612,3.3558612,3.358484,3.358484,3.3569732,3.3553734,3.3559606,3.3523777,3.3504183,3.34611,3.3367712,3.3310635,3.3244476,3.3167078,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.322226,3.330156,3.3368897,3.3426723,3.3497078,3.3539813,3.3577383,3.3594546,3.3617685,3.3647056,3.3647056,3.3660514,3.3690648,3.3690648,3.3690648,3.3690648,3.3680732,3.366768,3.366768,3.3639278,3.3629534,3.3613253,3.3577478,3.3536913,3.3476362,3.342612,3.3335173,3.3261015,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.3286784,3.3355584,3.3414345,3.3465037,3.3225698,3.3278918,3.3303108,3.334736,3.3201196,3.3223815,3.3245246,3.3245246,3.3005304,3.3005304,3.3005304,3.2979794,3.2901835,3.287313,3.284281,3.281073,3.2508314,3.2316804,3.2009227,3.0802934,1.7911614,1.3472905,0.9038186,0.5152642,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>    , Item {iId = defaultSession {sId = 49}, iSessId = 49, iProj = 49, iMinDur = 4, iMaxDur = 22, iOvhdDur = 1, iSTimAv = 26, iPTimAv = 26, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.65668,3.6661355,3.6680984,3.6756794,3.6822193,3.6851578,3.690742,3.695413,3.6995397,3.7032082,3.706868,3.7098022,3.7124403,3.7148206,3.7176218,3.7195597,3.7213213,3.7229166,3.7243729,3.7263176,3.7274697,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>    ,  Item {iId = defaultSession {sId = 55}, iSessId = 55, iProj = 55, iMinDur = 4, iMaxDur = 9, iOvhdDur = 1, iSTimAv = 13, iPTimAv = 13, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.2910039,1.0447681,2.120604,3.0571284,3.3181374,3.3452837,3.368771,3.3468423,3.3644207,3.379783,3.388943,3.406201,3.413472,3.423202,3.4289796,3.42882,3.4336522,3.4358869,3.4380078,3.4274135,3.4274135,3.425333,3.4231377,3.4061165,3.4009726,3.3952806,3.3890123,3.3793895,3.3677518,3.3590722,3.3445234,3.3228283,3.2627797,2.9732091,2.4141161,1.3532797,0.87769586,0.34438902,8.3537094e-2,1.1377492e-2,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.4679422,1.3655062,2.4299622,3.1089356,3.320367,3.3465877,3.3693163,3.346191,3.3633525,3.3735824,3.3873231,3.4000525,3.407192,3.416743,3.422397,3.4300017,3.4323676,3.4367414,3.4387538,3.4410388,3.4410388,3.4390368,3.436927,3.4351242,3.4303296,3.4250286,3.4160562,3.4046519,3.39337,3.384964,3.3708627,3.3525918,3.3341637,3.320398,3.2017725,2.7878275,2.0224483,1.1270475,0.4223569,8.305312e-2,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.501285e-17,2.0491436e-14,1.1265533e-17,4.7220956e-15,6.292054e-13,3.4061545e-11,1.3269516e-17,7.9878426e-16,2.4932244e-14,4.5203912e-13,2.2031755e-15,2.957104e-14,1.3306627e-13,5.09911e-13,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>     , Item {iId = defaultSession {sId = 71}, iSessId = 71, iProj = 71, iMinDur = 4, iMaxDur = 4, iOvhdDur = 1, iSTimAv = 4, iPTimAv = 4, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.8785079,2.8884664,2.8913403,2.8950777,2.8982382,2.902421,2.9046736,2.9060066,2.9077919,2.910927,2.9122205,2.9133499,2.9143348,2.9128788,2.913634,2.914285,2.9148402,2.9163358,2.9167087,2.9169035,2.9171498,2.91688,2.9169295,2.9169295,2.9168234,2.9158475,2.915564,2.9153368,2.914923,2.9140027,2.913404,2.9126976,2.9118862,2.9118829,2.9108298,2.9096363,2.9082608,2.9063275,2.9051278,2.9031205,2.9007826,2.8971386,2.8939009,2.8900714,2.8871117,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.898295,2.902728,2.9059112,2.908597,2.9108791,2.912528,2.9142854,2.9158046,2.917122,2.918902,2.9198856,2.9207413,2.9214897,2.920383,2.9209385,2.921424,2.9218347,2.9225204,2.9228132,2.9230459,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>     , Item {iId = defaultSession {sId = 75}, iSessId = 75, iProj = 75, iMinDur = 7, iMaxDur = 7, iOvhdDur = 1, iSTimAv = 7, iPTimAv = 7, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.5779262,2.5988364,2.61149,2.617885,2.6227353,2.6271534,2.6311893,2.6345744,2.6345744,2.6345744,2.6345744,2.6347673,2.6347673,2.6347673,2.6310718,2.627177,2.6227705,2.617931,2.612595,2.6064434,2.5836263,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.5974605,2.6110058,2.6167347,2.6225455,2.6271982,2.6314337,2.6353078,2.638843,2.638843,2.638843,2.638843,2.6390707,2.6390707,2.6390707,2.6355367,2.6317713,2.6275563,2.6229305,2.617826,2.6121752,2.5987825,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.6041887,2.6105874,2.6163251,2.6239438,2.6283057,2.6322932,2.6322932,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>     , Item {iId = defaultSession {sId = 81}, iSessId = 81, iProj = 81, iMinDur = 6, iMaxDur = 9, iOvhdDur = 1, iSTimAv = 15, iPTimAv = 15, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.6233652,2.6267395,2.6328118,2.636165,2.6410959,2.6454508,2.6474404,2.6510758,2.6543334,2.6572552,2.6598923,2.6610916,2.6633592,2.6654093,2.66728,2.6691337,2.6706772,2.6720858,2.6733725,2.674551,2.6750934,2.676115,2.6770446,2.6774852,2.6778743,2.6786106,2.6789486,2.6792357,2.679539,2.679539,2.6798294,2.6799273,2.6799273,2.6799273,2.679633,2.6797085,2.6793857,2.679051,2.678321,2.6779685,2.6771271,2.6761985,2.6757004,2.6746914,2.673524,2.6722453,2.670836,2.6691957,2.6675055,2.6656425,2.664642,2.6622303,2.659846,2.6572087,2.6542852,2.6510906,2.6493196,2.6454601,2.6411097,2.6387281,2.633469,2.6274421,2.624092,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.6267307,2.633044,2.6358826,2.6414766,2.6437929,2.6480165,2.6517587,2.6552725,2.656802,2.6596286,2.662179,2.6645768,2.6666543,2.6685503,2.670264,2.671801,2.6725273,2.673891,2.675133,2.6762013,2.6772382,2.6781843,2.6786203,2.679414,2.6797898,2.6804972,2.6808283,2.6812701,2.6815581,2.6818376,2.6818376,2.6818705,2.6818705,2.6815903,2.6815903,2.6813374,2.6810286,2.6803558,2.6799974,2.679267,2.678847,2.6779523,2.6769702,2.6760416,2.6754653,2.6742334,2.6728735,2.6714132,2.6697845,2.6679914,2.6660216,2.6638587,2.6614535,2.6587932,2.6573553,2.6543393,2.650848,2.6469233,2.6447744,2.6400661,2.63746,2.6316822,2.6284661,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.6347308,2.6376314,2.6428604,2.6449754,2.6492884,2.653102,2.654846,2.6575797,2.660464,2.6630595,2.6654036,2.6664538,2.6684706,2.6703062,2.6719706,2.6733246,2.6747105,2.675979,2.6771314,2.6809268,2.6813886,2.6822598,2.6830418,2.6832194,2.6838982,2.6842155,2.6845124,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>     , Item {iId = defaultSession {sId = 83}, iSessId = 83, iProj = 83, iMinDur = 9, iMaxDur = 9, iOvhdDur = 1, iSTimAv = 9, iPTimAv = 9, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.4847019,2.492332,2.5015254,2.5065606,2.5108776,2.51463,2.5179205,2.5182438,2.5210414,2.52232,2.523529,2.5248399,2.5259154,2.5269375,2.5269375,2.5272007,2.5272007,2.5261858,2.5261858,2.5256703,2.5245514,2.5233665,2.5207798,2.5193908,2.5162919,2.512766,2.5087318,2.5046988,2.4993105,2.4893408,2.4809968,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.490415,2.4972494,2.503006,2.5081952,2.512411,2.5160773,2.5192945,2.522299,2.5235934,2.5259674,2.5270584,2.5283046,2.5283046,2.5292764,2.5292764,2.529389,2.529389,2.528419,2.528419,2.5276935,2.5266147,2.5242724,2.522992,2.5204604,2.5172963,2.513691,2.509543,2.5051603,2.4995522,2.4928935,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.4875727,2.4953303,2.5045896,2.5095901,2.513896,2.5176027,2.5208423,2.5236883,2.5254295,2.527794,2.5288768,2.529901,2.5300045,2.530941,2.530941,2.530941,2.530652,2.530652,2.5297124,2.5287209,2.526767,2.5256393,2.5244474,2.5218308,2.5205095,2.5173628,2.5137775,2.5096483,2.503467,2.4944944,2.487048,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>     , Item {iId = defaultSession {sId = 90}, iSessId = 90, iProj = 90, iMinDur = 6, iMaxDur = 8, iOvhdDur = 1, iSTimAv = 14, iPTimAv = 14, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [1.3253677,1.6346262,2.634212,2.8736863,3.0720181,3.295385,3.3263607,3.4174428,3.4824445,3.526236,3.5133774,3.541594,3.5514474,3.55895,3.071085,3.071085,3.03092,2.9360542,3.036872,2.9204788,2.7797296,2.6130443,3.5228775,3.4476001,3.329221,3.057109,3.036572,2.6056015,2.251405,1.8608359,2.2609222,1.8015324,1.3352314,0.71594083,0.46607542,0.23409337,5.8333263e-2,1.698968e-2,4.3542805e-4,4.9113478e-5,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.5581251e-6,3.4451503e-5,6.2329834e-4,2.8301093e-3,9.890022e-3,3.2342285e-2,0.10850134,0.2051479,0.34777206,0.7384951,1.0099188,1.3056109,1.7626634,2.1333687,2.5284858,2.7527673,2.9433765,3.2542517,3.3541028,3.4289665,3.4830348,3.5431542,3.553377,3.5610726,3.5666296,3.6230812,3.6230812,3.619519,3.6175134,3.6179104,3.6128616,3.6071663,3.6007981,3.5989296,3.5869825,3.5780172,3.560441,3.4860952,3.338713,2.9629157,2.609966,2.1742725,1.4841478,1.0420487,0.66152865,0.2376461,0.10262489,3.578659e-2,9.562659e-3,1.7209546e-4,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.8238413e-5,3.3836643e-4,2.237492e-3,9.251821e-3,2.939169e-2,0.11305439,0.12005236,0.22807005,0.48612258,0.72115165,0.50553805,0.8248646,1.0692585,1.3268875,0.88616455,1.0770402,1.366207,1.553403,2.0067112,2.1710217,2.3846374,2.4469578,1.804412,1.9034144,1.9476237,1.9476237,3.0924091,3.0563285,3.0160751,2.971338,0.14584054,0.11975524,9.522382e-2,6.306193e-2,5.3896074e-4,3.0163783e-4,1.0889576e-4,4.9263344e-5,8.392413e-11,6.8331694e-12,9.931715e-13,3.4939455e-14,2.90916e-17,1.2084385e-18,4.717811e-21,6.376989e-23,1.9156308e-34,1.8804141e-37,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.0638695e-3,9.48748e-3,1.0425357e-4,1.2255196e-3,4.4955965e-3,1.3278046e-2,3.0823307e-5,2.348201e-4,7.212908e-4,2.9383274e-3,4.360986e-3,8.773362e-3], iPast = []}
>     , Item {iId = defaultSession {sId = 92}, iSessId = 92, iProj = 92, iMinDur = 9, iMaxDur = 9, iOvhdDur = 1, iSTimAv = 9, iPTimAv = 9, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [2.3954687,2.3924391,2.390151,2.3860536,2.3782396,2.3714573,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.374525,2.380653,2.387808,2.3919582,2.3951464,2.397878,2.3990984,2.4013867,2.40238,2.404187,2.4050102,2.4061356,2.4061356,2.406859,2.406859,2.4074001,2.4074001,2.4074001,2.4066913,2.4058816,2.4050798,2.4042282,2.4023504,2.401481,2.3991992,2.396556,2.3934724,2.389844,2.3854911,2.380194,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.387655,2.3922634,2.3960474,2.3997731,2.4024076,2.4046643,2.406615,2.4076269,2.4092112,2.4099314,2.4106083,2.4113233,2.4119196,2.4119196,2.4119196,2.4120333,2.4120333,2.4114401,2.4114401,2.4106293,2.40996,2.408485,2.4076734,2.4052973,2.4042902,2.4020293,2.3993945,2.395291,2.389292,2.3841467,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.3762918,2.3825524,2.3897567,2.3935285,2.4017732,2.4041388,2.4061716,2.407086,2.4082663,2.4090178,2.409723,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>    ,  Item {iId = defaultSession {sId = 104}, iSessId = 104, iProj = 104, iMinDur = 9, iMaxDur = 9, iOvhdDur = 1, iSTimAv = 9, iPTimAv = 9, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.4235635,2.4267511,2.4294713,2.4318242,2.434698,2.4364486,2.4379954,2.4387035,2.4412346,2.4417996,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>    , Item {iId = defaultSession {sId = 126}, iSessId = 126, iProj = 126, iMinDur = 10, iMaxDur = 10, iOvhdDur = 1, iSTimAv = 10, iPTimAv = 10, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [0.0,0.0,0.0,0.0,0.0,2.908521,2.9181347,2.925776,2.9298997,2.9350128,2.9386024,2.942059,2.9449384,2.946605,2.947442,2.9487903,2.950014,2.9516358,2.9529178,2.9533577,2.9541814,2.9545627,2.9552689,2.955614,2.95594,2.95594,2.956807,2.956489,2.9561548,2.9558063,2.9553456,2.9549444,2.9540873,2.9531415,2.9526937,2.9509537,2.9496343,2.9473753,2.9436197,2.941549,2.9379199,2.933486,2.9285846,2.9216473,2.9125571,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.9144177,2.9212494,2.9285426,2.9342263,2.938764,2.942743,2.945781,2.947524,2.949798,2.9518566,2.9536033,2.9546363,2.9555666,2.956753,2.9575152,2.9578674,2.9585252,2.9589403,2.9589403,2.959232,2.959232,2.959327,2.959327,2.959027,2.9583814,2.9581904,2.957444,2.9566321,2.9557328,2.955035,2.9539487,2.952093,2.9499092,2.9474475,2.944533,2.941009,2.9366815,2.9335399,2.9272027,2.918892,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.9216588,2.9300392,2.9359717,2.9406345,2.9443827,2.9469562,2.9487197,2.9510026,2.9529376,2.9549274,2.9559486,2.9573066,2.958106,2.9563868,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>     , Item {iId = defaultSession {sId = 135}, iSessId = 135, iProj = 135, iMinDur = 9, iMaxDur = 9, iOvhdDur = 1, iSTimAv = 9, iPTimAv = 9, iTimeBt = 288, iTrType = Optional, iTrnsts = [], iFuture = [2.694692,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.7046986,2.7127197,2.71937,2.7249584,2.729629,2.733684,2.737173,2.7402,2.742951,2.745227,2.7472103,2.7483916,2.7503996,2.7517567,2.7529275,2.7536185,2.75455,2.7550654,2.7552977,2.7557273,2.7557728,2.7557728,2.7557728,2.755344,2.7550254,2.754509,2.753611,2.7529175,2.7515237,2.750158,2.7491329,2.7474055,2.7454722,2.7432017,2.7405944,2.7375865,2.7341242,2.7300923,2.7253792,2.719823,2.7132657,2.7052908,2.6990085,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.7023568,2.7111328,2.7183867,2.7244244,2.729517,2.734041,2.737743,2.7409337,2.7437,2.7453766,2.747568,2.7494726,2.7511387,2.752621,2.7538738,2.75461,2.7555785,2.7561855,2.756687,2.7571268,2.7573256,2.757461,2.757461,2.7572625,2.757044,2.7565958,2.7560644,2.7554731,2.7544453,2.753263,2.7523725,2.7508602,2.7491322,2.7472236,2.7449458,2.7423258,2.739308,2.7358222,2.7317538,2.7269871,2.7213426,2.7171261,2.7094853,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.713894,2.7211673,2.7274272,2.7326608,2.7370782,2.7400048,2.7433972,2.746313,2.7488463,2.7523546,2.7542238,2.7558477,2.7572696,2.7592547,2.7599688,2.7609215,2.7614708,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], iPast = []}
>    ]

