Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

> module Antioch.StatisticsTests where

> import Antioch.DateTime
> import Antioch.Statistics
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Score
> import Antioch.Utilities
> import Antioch.Generators (receiver2Band)
> import Antioch.GenerateSchedule (validSimulatedWindows)
> import Antioch.PProjects
> import Antioch.TimeAccounting
> import Antioch.ReceiverTemperatures
> import Antioch.Simulate (updateSessions)
> import Data.List
> import Test.HUnit
> import System.Random

> tests = TestList [
>     test_scheduleHonorsFixed
>   , test_count
>   , test_sessionDecFreq
>   , test_periodDecFreq
>   , test_sessionDecRA
>   , test_periodDecRA
>   , test_sessionRA
>   , test_periodRA
>   , test_sessionDec
>   , test_periodDec
>   , test_sessionFreq
>   , test_sessionFreq2
>   , test_sessionFreqHrs
>   , test_periodFreq
>   , test_sessionTP
>   , test_sessionTP2
>   , test_sessionTPQtrs
>   , test_periodStart
>   , test_periodDuration
>   , test_sessionMinDuration
>   , test_freqTime
>   , test_periodBand
>   , test_periodEfficiencyByBand
>   , test_decVsElevation
>   , test_efficiencyVsFrequency
>   --, test_bandEfficiencyByTime
>   , test_historicalFreq
>   , test_historicalDec
>   , test_historicalRA
>   , test_historicalTime
>   , test_historicalTime'
>   , test_historicalLST
>   , test_satisfactionRatio
>   , test_findScheduleGaps
>   , test_getOriginalSchedule'
>   , test_breakdownSimulationTimes
>   , test_fracObservedTimeByDays
>   , test_periodSchdFactors
>   , test_getPeriodsSchdEffs
>   , test_periodObsFactors
>   , test_historicalSchdMeanFactors
>   , test_historicalSchdObsEffs
>   , test_historicalSchdMeanObsEffs_getPeriodsSchdEffs
>   , test_compareWindowPeriodEfficiencies
>   , test_calcMeanWindowEfficiencies
>   , test_getPeriodsObsEffs
>   , test_getCanceledPeriodsDetails
>    ]


> test_getCanceledPeriodsDetails = TestCase $ do
>     -- first calculate the MOC for each period
>     w <- getWeatherTest Nothing
>     rt <- getReceiverTemperatures
>     minObs' <- mapM (moc w rt) ps
>     let minObs = map (\(Just x, _) -> x) minObs'
>     -- now get the details
>     details <- getCanceledPeriodsDetails w rt [] ps
>     -- make sure parts of the details agree w/ the min obs results
>     assertEqual "test_getCanceledPeriodsDetails_0" True $ all (==True) $ map compare $ zip minObs details
>     -- make sure the first one is self consistent
>     let (p, minObs, meanEff, effs, trks, winds) = head details
>     let obsEffs = map (\(a,b,c,d) -> d) effs
>     let prods = zipWith prod obsEffs trks
>     let meanEff' = (sum prods) / (fromIntegral . length $ prods)
>     assertEqual "test_getCanceledPeriodsDetails_1" meanEff' meanEff
>     assertEqual "test_getCanceledPeriodsDetails_2" exp (head details)
>   where
>     prod e (Just t) = e * t
>     ss = getOpenPSessions -- 10 of them
>     start = fromGregorian 2006 2 2 0 0 0
>     pdur = 2*60
>     numPs = length ss 
>     dts = [start, addMinutes pdur start .. addMinutes (pdur*(numPs-1)) start]
>     mkPeriod (s, dt) = defaultPeriod { session = s, startTime = dt, duration = pdur }
>     ps = map mkPeriod $ zip ss dts
>     moc w rt p = runScoring' w [] rt $ minimumObservingConditions (startTime p) (duration p) (session p) 
>     compare (minObs, (p, mo, meanEff, _, _, _)) = minObs == (meanEff >= mo)
>     exp = (head ps, 0.35922727,0.64642495,[(0.6885284,0.96142334,0.92952526,0.6153153),(0.68980044,0.9626149,0.92952526,0.6172161),(0.6979545,0.9626149,0.92952526,0.62451214),(0.70289797,0.9626149,0.92952526,0.6289354),(0.70747703,0.9626149,0.92952526,0.6330327),(0.7023932,0.9979989,1.0,0.70098764),(0.70638925,0.9979989,1.0,0.70497566)],[Just 1.0,Just 1.0,Just 1.0,Just 1.0,Just 1.0,Just 1.0,Just 1.0],[Just 1.714091,Just 2.300624,Just 2.300624,Just 2.300624,Just 2.300624,Just 0.60922635,Just 0.60922635]) 

> test_getPeriodsObsEffs = TestCase $ do
>     w <- getWeatherTest Nothing
>     rt <- getReceiverTemperatures
>     peffs <- getPeriodsObsEffs w rt [] ps
>     assertEqual "test_getPeriodsObsEffs_1" firstEffs (snd . head $ peffs)
>     assertEqual "test_getPeriodsObsEffs_2" lastEffs  (snd . last $ peffs)
>   where
>     ss = getOpenPSessions -- 10 of them
>     start = fromGregorian 2006 2 2 0 0 0
>     pdur = 2*60
>     numPs = length ss 
>     dts = [start, addMinutes pdur start .. addMinutes (pdur*(numPs-1)) start]
>     mkPeriod (s, dt) = defaultPeriod { session = s, startTime = dt, duration = pdur }
>     ps = map mkPeriod $ zip ss dts
>     firstEffs = [(0.67870116,0.9770739,0.92952526,0.6164065),(0.68843114,0.9770739,0.92952526,0.6252434),(0.7175121,0.9756617,0.92952526,0.65071326),(0.7251464,0.9756617,0.92952526,0.6576369),(0.7297692,0.9756617,0.92952526,0.66182923),(0.73404694,0.9756617,0.92952526,0.6657088),(0.72494024,0.9999896,1.0,0.72493273),(0.728718,0.9999896,1.0,0.7287104),(0.7304929,0.9999896,1.0,0.7304853)]
>     lastEffs = [(0.26221526,0.8907109,0.888959,0.20762347),(0.47001064,0.8907109,0.888959,0.37215698),(0.52427566,0.82269764,0.888959,0.3834261),(0.5634801,0.82269764,0.888959,0.41209802),(0.5944801,0.82269764,0.888959,0.43476972),(0.612092,0.82269764,0.888959,0.4476501),(0.63853604,0.8714121,0.888959,0.4946417),(0.65821105,0.8714121,0.888959,0.5098829),(0.6753238,0.8714121,0.888959,0.5231393)]


> test_calcMeanWindowEfficiencies = TestCase $ do
>     -- equal weights
>     let r1 = calcMeanWindowEfficiencies ps 
>     assertEqual "test_calcMeanWindowEfficiencies_1" (0.75, 0.375) r1 
>     -- nonequal weights
>     let r2 = calcMeanWindowEfficiencies ps2 
>     assertEqual "test_calcMeanWindowEfficiencies_1" (0.6666667, 0.33333334) r2 
>   where
>     p1 = defaultPeriod {duration = 4*60}
>     p2 = defaultPeriod {duration = 8*60}
>     ps  = [((p1, 1.0), (p1, 0.5)), ((p1, 0.5),(p1, 0.25))] 
>     ps2 = [((p1, 1.0), (p1, 0.5)), ((p2, 0.5),(p2, 0.25))] 

> test_compareWindowPeriodEfficiencies = TestCase $ do
>     w <- getWeatherTest Nothing
>     assertEqual "test_compareWindowPeriodEfficiencies_0" True (validSimulatedWindows $ wSession . (\(w,c,p) -> w) . head $ wInfo2)
>     effs <- compareWindowPeriodEfficiencies wInfo w rs 
>     assertEqual "test_compareWindowPeriodEfficiencies_1" [] effs
>     effs <- compareWindowPeriodEfficiencies wInfo2 w rs
>     assertEqual "test_compareWindowPeriodEfficiencies_2" exp effs
>     effs <- compareWindowPeriodEfficiencies (wInfo2 ++ wInfo3) w rs
>     assertEqual "test_compareWindowPeriodEfficiencies_3" exp2 effs
>   where
>     rs = []
>     s = getTestWindowSession
>     wInfo = [(head . windows $ s, Nothing, head . periods $ s)]
>     s2' = getTestWindowSession2
>     cp = defaultPeriod { session = s2
>                        , startTime = fromGregorian 2006 3 2 12 0 0
>                        , duration = 60*2 }
>     dp = head . periods $ s2'
>     s2 = makeSession s2' (windows s2') [cp]
>     wInfo2 = [(head . windows $ s2, Just cp, dp)]
>     exp = [((cp,0.6327699),(dp,0.6808212))]
>     cp2 = cp { startTime = fromGregorian 2006 4 5 12 0 0 }
>     dp2 = dp { startTime = fromGregorian 2006 4 1 12 0 0 }
>     -- the windows info doesn't really matter
>     wInfo3 = [(head . windows $ s2, Just cp2, dp2)]
>     exp2 = [((cp, 0.6327699),(dp,0.6808212 ))
>            ,((cp2,0.6814143), (dp2,0.6646747))]

> test_partitionWindowedPeriodEfficiencies = TestCase $ do
>     assertEqual "test_partitionWindowedPeriodEfficiencies_1"
>        expected
>        (partitionWindowedPeriodEfficiencies wps pes)
>     assertEqual "test_partitionWindowedPeriodEfficiencies_2"
>        ([], [])
>        (partitionWindowedPeriodEfficiencies wps [])
>   where
>     ps = [defaultPeriod {startTime = i} | i <- [0 .. 11]]
>     pes = [(p,[]) | p <- ps]
>     w1 = defaultWindow {wId = 1}
>     w2 = defaultWindow {wId = 2}
>     w3 = defaultWindow {wId = 3}
>     wps = [(w1 {wPeriodId = Just 11}, Nothing,         ps !! 11)
>          , (w2 {wPeriodId = Just 10}, Just (ps !!  9), ps !! 10)
>          , (w3 {wPeriodId = Just  8}, Just (ps !!  7), ps !!  8)
>          , (w1 {wPeriodId = Just  6}, Nothing,         ps !!  6)
>          , (w2 {wPeriodId = Just  5}, Just (ps !!  4), ps !!  5)
>          , (w3 {wPeriodId = Just  3}, Nothing,         ps !!  3)
>          , (w1 {wPeriodId = Just  2}, Nothing,         ps !!  2)
>          , (w3 {wPeriodId = Just  1}, Just (ps !!  0), ps !!  1)
>           ]
>     expected = partition (\pe -> (elem (startTime . fst $ pe) [0, 4, 7, 9])) pes

> test_historicalSchdMeanFactors = TestCase $ do
>   w <- getWeatherTest Nothing
>   r <- historicalSchdMeanFactors [p] trackingEfficiency w rs
>   assertEqual "test_historicalSchdMeanFactors_1" [0.99873495] r
>     where
>   rs = []
>   p = getTestPeriod

Note: refactor so that historical*Factors methods can take a test weather.
Story: https://www.pivotaltracker.com/story/show/14140221

> test_historicalSchdObsEffs = TestCase $ do
>   w <- getWeatherTest Nothing
>   r <- historicalSchdObsEffs [getTestPeriod] w [] 
>   assertEqual "test_historicalSchdObsEffs_0" 20 (length r)
>   assertEqual "test_historicalSchdObsEffs_1" [0.9804807,0.9770225] (take 2 r)
>   -- these should be equivalent
>   rt <- getReceiverTemperatures
>   pSchdEffs <- getPeriodsSchdEffs w rt [] [getTestPeriod]
>   let allEffs = concatMap snd pSchdEffs 
>   let pObsEffs = map (\(a, t, s, o) -> o) allEffs
>   assertEqual "test_historicalSchdObsEffs_0" r pObsEffs

Test that two ways to get the same result yield the same answer.

> test_historicalSchdMeanObsEffs_getPeriodsSchdEffs = TestCase $ do
>   w <- getWeatherTest Nothing
>   rt <- getReceiverTemperatures
>   -- method 1
>   r1 <- historicalSchdMeanObsEffs [getTestPeriod, getTestPeriod2] w []
>   -- method 2
>   r2' <- getPeriodsSchdEffs w rt [] [getTestPeriod, getTestPeriod2]
>   let r2 = extractPeriodMeanEffs r2' (\(a,t,s,o) -> o)
>   assertEqual "test_hsmo_gps_1" r1 r2  

> test_getPeriodsSchdEffs = TestCase $ do
>   w <- getWeatherTest Nothing
>   rt <- getReceiverTemperatures
>   pSchdEffs <- getPeriodsSchdEffs w rt [] [getTestPeriod]
>   --let exp = [(0.9814386,0.9992234,0.9996135,0.9802974)
>   --          ,(0.977912,0.99928236,0.9996135,0.97683257) ]
>   let exp = [(0.9814386,0.9994102,0.9996135,0.9804807),(0.977912,0.99947673,0.9996135,0.9770225)]
>   assertEqual "test_getPeriodsSchdEffs_1" exp (take 2 $ snd . head $ pSchdEffs)
>   --let r2 = extractPeriodMeanEffs r2' (\(a,t,s,o) -> o)
>   pObsEffs  <- getPeriodsObsEffs w rt [] [getTestPeriod]
>   assertEqual "test_getPeriodsSchdEffs_1" True (pSchdEffs /= pObsEffs)

> test_periodSchdFactors = TestCase $ do
>   -- score it the same way epriodSchdFactors will 
>   w <- getWeather $ Just . pForecast $ getTestPeriod
>   rts <- getReceiverTemperatures
>   let s = session getTestPeriod
>   let dt = startTime getTestPeriod
>   [(_, Just trkEff1)] <- runScoring w [] rts $ trackingEfficiency dt s
>   let dt2 = periodEndTime getTestPeriod
>   [(_, Just trkEff2)] <- runScoring w [] rts $ trackingEfficiency dt2 s
>   -- now score the period, and make sure results match
>   w <- getWeatherTest Nothing
>   fcs <- periodSchdFactors getTestPeriod trackingEfficiency w []
>   assertEqual "test_periodSchdFactors_1" trkEff1 (head fcs)
>   assertEqual "test_periodSchdFactors_2" trkEff2 (last fcs)
>   fcs <- periodSchdFactors getTestPeriod2 trackingEfficiency w []
>   assertEqual "test_periodSchdFactors_3" 0.9761378  (head fcs)
>   assertEqual "test_periodSchdFactors_4" 0.97425276 (last fcs)

> test_periodObsFactors = TestCase $ do
>   w <- getWeatherTest Nothing
>   fcs <- periodObsFactors getTestPeriod2 trackingEfficiency w [] 
>   assertEqual "test_periodObsFactors_1" 0.9774544 (head fcs)
>   assertEqual "test_periodObsFactors_2" 0.9773642 (last fcs)

> test_fracObservedTimeByDays = TestCase $ do
>     let result = fracObservedTimeByDays ss ps
>     let exp = [(0.0,1.0),(1.0,0.8888889),(2.0,0.6666667),(3.0,0.6666667),(4.0,0.5555556),(5.0,0.33333334),(6.0,0.33333334),(7.0,0.22222222),(8.0,0.22222222),(9.0,0.0)]
>     assertEqual "fracObservedTimeByDays_1" exp result 
>     let r2 = fracObservedTimeByDays ss [] 
>     assertEqual "fracObservedTimeByDays_1" [] r2 
>   where
>     s1 = defaultSession { sAllottedT = 60 }
>     s2 = defaultSession { sAllottedT = 120 }
>     s3 = defaultSession { sAllottedT = 60 }
>     s4 = defaultSession { sAllottedT = 120 }
>     s5 = defaultSession { sAllottedT = 60 }
>     s6 = defaultSession { sAllottedT = 120 }
>     ss = [s1, s2, s3, s4, s5, s6]
>     dts = [ fromGregorian 2006 1 1 0 0 0 
>           , fromGregorian 2006 1 2 0 0 0 
>           , fromGregorian 2006 1 4 0 0 0 
>           , fromGregorian 2006 1 5 0 0 0 
>           , fromGregorian 2006 1 7 0 0 0 
>           , fromGregorian 2006 1 9 0 0 0 
>           ]
>     durs = [60, 120, 60, 120, 60, 120]
>     ps = zipWith3 mkPeriod ss dts durs 
>     mkPeriod s start dur = Period 0 s start dur 0.0 Pending undefined False dur Nothing


> test_scheduleHonorsFixed = TestCase $ do
>     assertEqual "StatisticsTests_test_scheduleHonorsFixed_1" True (scheduleHonorsFixed fixed1 schd)
>     assertEqual "StatisticsTests_test_scheduleHonorsFixed_2" False (scheduleHonorsFixed fixed2 schd)
>     assertEqual "StatisticsTests_test_scheduleHonorsFixed_3" True (scheduleHonorsFixed [] schd)
>   where
>     s1 = defaultSession { sId = 1 }
>     s2 = defaultSession { sId = 2 }
>     s3 = defaultSession { sId = 3 }
>     s4 = defaultSession { sId = 4 }
>     ss = [s1, s2, s3, s4]
>     dts = [ fromGregorian 2006 1 1 0 0 0 
>           , fromGregorian 2006 1 2 0 0 0 
>           , fromGregorian 2006 1 4 0 0 0 
>           , fromGregorian 2006 1 5 0 0 0 
>           ]
>     durs = [60, 120, 60, 120]
>     schd = zipWith3 mkPeriod ss dts durs 
>     mkPeriod s start dur = Period 0 s start dur 0.0 Pending undefined False dur Nothing
>     fixed1 = [(schd!!1)]
>     fixed2 = [Period 0 s2 (dts!!1) 30 0.0 Pending undefined False 30 Nothing]

> test_count = TestCase $ do
>     assertEqual "StatisticsTests_test_count1" exp1 cnt1
>     assertEqual "StatisticsTests_test_count2" exp2 cnt2
>  where
>    cnt1 = count minDuration [0..5] [s1]
>    s1 = defaultSession {minDuration = 3}
>    exp1 = [(0,0),(1,0),(2,0),(3,1),(4,0),(5,0)]
>    s2 = defaultSession {minDuration = 1}
>    cnt2 = count minDuration [0..5] [s1,s2,s2,s1,s1]
>    exp2 = [(0,0),(1,2),(2,0),(3,3),(4,0),(5,0)]
> 
> test_sessionDecFreq = TestCase $ do
>     assertEqual "test_sessionDecFreq" expected (sessionDecFreq sessions)
>   where
>     (sessions, _) = getTestData
>     expected = [(1.4,1.4),(9.0,1.4),(0.8,3.5e-2),(0.8,0.0),(1.34,-2.8e-2),(0.8,0.0),(1.4,1.3962634),(9.0,-0.325),(1.34,0.0),(0.8,-0.22),(9.0,1.16),(1.4,-2.82e-2),(1.2,1.4),(9.0,2.7260742),(0.8,3.4906585e-2),(0.8,-0.18448131),(0.8,-0.18448131),(0.8,0.0)]

> test_periodDecFreq = TestCase $ do
>     assertEqual "test_periodDecFreq" expected (periodDecFreq periods)
>   where
>     (_, periods) = getTestData
>     expected = [(1.4,1.4),(9.0,1.4),(0.8,3.5e-2),(0.8,0.0),(1.34,-2.8e-2),(0.8,0.0),(1.4,1.3962634),(9.0,-0.325),(1.34,0.0),(0.8,-0.22),(9.0,1.16),(1.4,-2.82e-2),(1.2,1.4),(9.0,2.7260742),(0.8,3.4906585e-2),(0.8,-0.18448131),(0.8,-0.18448131),(0.8,0.0)] 

> test_sessionDecRA = TestCase $ do
>     assertEqual "test_sessionDecRA" expected (sessionDecRA sessions)
>   where
>     (sessions, _) = getTestData
>     expected = [(0.0,1.4),(1.0969394,1.4),(3.796091,3.5e-2),(5.67232,0.0),(5.8564525,-2.8e-2),(5.67232,0.0),(0.0,1.3962634),(2.7260742,-0.325),(7.272205e-6,0.0),(4.8092546,-0.22),(4.7060914,1.16),(5.8564525,-2.82e-2),(7.272205e-6,1.4),(-0.32537785,2.7260742),(3.796091,3.4906585e-2),(4.858997,-0.18448131),(4.858997,-0.18448131),(5.67232,0.0)] 

> test_periodDecRA = TestCase $ do
>     assertEqual "test_periodDecRA" expected (periodDecRA periods)
>   where
>     (_, periods) = getTestData
>     expected = [(0.0,1.4),(1.0969394,1.4),(3.796091,3.5e-2),(5.67232,0.0),(5.8564525,-2.8e-2),(5.67232,0.0),(0.0,1.3962634),(2.7260742,-0.325),(7.272205e-6,0.0),(4.8092546,-0.22),(4.7060914,1.16),(5.8564525,-2.82e-2),(7.272205e-6,1.4),(-0.32537785,2.7260742),(3.796091,3.4906585e-2),(4.858997,-0.18448131),(4.858997,-0.18448131),(5.67232,0.0)]


> test_sessionRA = TestCase $ do
>     assertEqual "test_sessionRA" expected (sessionRA sessions)
>   where
>     (sessions, _) = getTestData 
>     expected = [(0.0,3.0),(1.0,2.0),(2.0,0.0),(3.0,0.0),(4.0,0.0),(5.0,1.0),(6.0,0.0),(7.0,0.0),(8.0,0.0),(9.0,0.0),(10.0,0.0),(11.0,1.0),(12.0,0.0),(13.0,0.0),(14.0,0.0),(15.0,2.0),(16.0,0.0),(17.0,0.0),(18.0,1.0),(19.0,3.0),(20.0,0.0),(21.0,0.0),(22.0,3.0),(23.0,2.0),(24.0,0.0)]

> test_periodRA = TestCase $ do
>     assertEqual "test_periodRA" expected (periodRA periods)
>   where
>     (_, periods) = getTestData
>     expected = [(0.0,3.0),(1.0,2.0),(2.0,0.0),(3.0,0.0),(4.0,0.0),(5.0,1.0),(6.0,0.0),(7.0,0.0),(8.0,0.0),(9.0,0.0),(10.0,0.0),(11.0,1.0),(12.0,0.0),(13.0,0.0),(14.0,0.0),(15.0,2.0),(16.0,0.0),(17.0,0.0),(18.0,1.0),(19.0,3.0),(20.0,0.0),(21.0,0.0),(22.0,3.0),(23.0,2.0),(24.0,0.0)]

> test_sessionDec = TestCase $ do
>     assertEqual "test_sessionDec" expected (sessionDec sessions)
>   where
>     (sessions, _) = getTestData
>     expected = [(-40.0,0.0),(-39.0,0.0),(-38.0,0.0),(-37.0,0.0),(-36.0,0.0),(-35.0,0.0),(-34.0,0.0),(-33.0,0.0),(-32.0,0.0),(-31.0,0.0),(-30.0,0.0),(-29.0,0.0),(-28.0,0.0),(-27.0,0.0),(-26.0,0.0),(-25.0,0.0),(-24.0,0.0),(-23.0,0.0),(-22.0,0.0),(-21.0,0.0),(-20.0,0.0),(-19.0,0.0),(-18.0,1.0),(-17.0,0.0),(-16.0,0.0),(-15.0,0.0),(-14.0,0.0),(-13.0,0.0),(-12.0,1.0),(-11.0,0.0),(-10.0,2.0),(-9.0,0.0),(-8.0,0.0),(-7.0,0.0),(-6.0,0.0),(-5.0,0.0),(-4.0,0.0),(-3.0,0.0),(-2.0,0.0),(-1.0,2.0),(0.0,4.0),(1.0,0.0),(2.0,1.0),(3.0,1.0),(4.0,0.0),(5.0,0.0),(6.0,0.0),(7.0,0.0),(8.0,0.0),(9.0,0.0),(10.0,0.0),(11.0,0.0),(12.0,0.0),(13.0,0.0),(14.0,0.0),(15.0,0.0),(16.0,0.0),(17.0,0.0),(18.0,0.0),(19.0,0.0),(20.0,0.0),(21.0,0.0),(22.0,0.0),(23.0,0.0),(24.0,0.0),(25.0,0.0),(26.0,0.0),(27.0,0.0),(28.0,0.0),(29.0,0.0),(30.0,0.0),(31.0,0.0),(32.0,0.0),(33.0,0.0),(34.0,0.0),(35.0,0.0),(36.0,0.0),(37.0,0.0),(38.0,0.0),(39.0,0.0),(40.0,0.0),(41.0,0.0),(42.0,0.0),(43.0,0.0),(44.0,0.0),(45.0,0.0),(46.0,0.0),(47.0,0.0),(48.0,0.0),(49.0,0.0),(50.0,0.0),(51.0,0.0),(52.0,0.0),(53.0,0.0),(54.0,0.0),(55.0,0.0),(56.0,0.0),(57.0,0.0),(58.0,0.0),(59.0,0.0),(60.0,0.0),(61.0,0.0),(62.0,0.0),(63.0,0.0),(64.0,0.0),(65.0,0.0),(66.0,0.0),(67.0,1.0),(68.0,0.0),(69.0,0.0),(70.0,0.0),(71.0,0.0),(72.0,0.0),(73.0,0.0),(74.0,0.0),(75.0,0.0),(76.0,0.0),(77.0,0.0),(78.0,0.0),(79.0,0.0),(80.0,1.0),(81.0,3.0),(82.0,0.0),(83.0,0.0),(84.0,0.0),(85.0,0.0),(86.0,0.0),(87.0,0.0),(88.0,0.0),(89.0,0.0),(90.0,0.0)]

> test_periodDec = TestCase $ do
>     assertEqual "test_periodDec" expected (periodDec periods)
>   where
>     (_, periods) = getTestData
>     expected = [(-40.0,0.0),(-39.0,0.0),(-38.0,0.0),(-37.0,0.0),(-36.0,0.0),(-35.0,0.0),(-34.0,0.0),(-33.0,0.0),(-32.0,0.0),(-31.0,0.0),(-30.0,0.0),(-29.0,0.0),(-28.0,0.0),(-27.0,0.0),(-26.0,0.0),(-25.0,0.0),(-24.0,0.0),(-23.0,0.0),(-22.0,0.0),(-21.0,0.0),(-20.0,0.0),(-19.0,0.0),(-18.0,1.0),(-17.0,0.0),(-16.0,0.0),(-15.0,0.0),(-14.0,0.0),(-13.0,0.0),(-12.0,1.0),(-11.0,0.0),(-10.0,2.0),(-9.0,0.0),(-8.0,0.0),(-7.0,0.0),(-6.0,0.0),(-5.0,0.0),(-4.0,0.0),(-3.0,0.0),(-2.0,0.0),(-1.0,2.0),(0.0,4.0),(1.0,0.0),(2.0,1.0),(3.0,1.0),(4.0,0.0),(5.0,0.0),(6.0,0.0),(7.0,0.0),(8.0,0.0),(9.0,0.0),(10.0,0.0),(11.0,0.0),(12.0,0.0),(13.0,0.0),(14.0,0.0),(15.0,0.0),(16.0,0.0),(17.0,0.0),(18.0,0.0),(19.0,0.0),(20.0,0.0),(21.0,0.0),(22.0,0.0),(23.0,0.0),(24.0,0.0),(25.0,0.0),(26.0,0.0),(27.0,0.0),(28.0,0.0),(29.0,0.0),(30.0,0.0),(31.0,0.0),(32.0,0.0),(33.0,0.0),(34.0,0.0),(35.0,0.0),(36.0,0.0),(37.0,0.0),(38.0,0.0),(39.0,0.0),(40.0,0.0),(41.0,0.0),(42.0,0.0),(43.0,0.0),(44.0,0.0),(45.0,0.0),(46.0,0.0),(47.0,0.0),(48.0,0.0),(49.0,0.0),(50.0,0.0),(51.0,0.0),(52.0,0.0),(53.0,0.0),(54.0,0.0),(55.0,0.0),(56.0,0.0),(57.0,0.0),(58.0,0.0),(59.0,0.0),(60.0,0.0),(61.0,0.0),(62.0,0.0),(63.0,0.0),(64.0,0.0),(65.0,0.0),(66.0,0.0),(67.0,1.0),(68.0,0.0),(69.0,0.0),(70.0,0.0),(71.0,0.0),(72.0,0.0),(73.0,0.0),(74.0,0.0),(75.0,0.0),(76.0,0.0),(77.0,0.0),(78.0,0.0),(79.0,0.0),(80.0,1.0),(81.0,3.0),(82.0,0.0),(83.0,0.0),(84.0,0.0),(85.0,0.0),(86.0,0.0),(87.0,0.0),(88.0,0.0),(89.0,0.0),(90.0,0.0)]

> test_sessionFreq = TestCase $ do
>     assertEqual "test_sessionFreq" expected (sessionFreq sessions)
>   where
>     (sessions, _) = getTestData
>     expected = [(0.0,0),(1.0,63210),(2.0,191760),(3.0,0),(4.0,0),(5.0,0),(6.0,0),(7.0,0),(8.0,0),(9.0,12555)] ++ empty
>     empty = [(x, 0) | x <- [10.0, 11.0 .. 120.0]]

> test_sessionFreq2 = TestCase $ do
>     assertEqual "test_sessionFreq2_1" cnt4_5   (snd (freqHist!!5)) 
>     assertEqual "test_sessionFreq2_2" cnt5_6   (snd (freqHist!!6)) 
>     assertEqual "test_sessionFreq2_3" cnt22_23 (snd (freqHist!!23)) 
>     assertEqual "test_sessionFreq2_4" cnt27_28 (snd (freqHist!!28)) 
>  where
>    ss = getOpenPSessions
>    freqHist = sessionFreq ss
>    cnt4_5 = 3600
>    cnt5_6 = 2400
>    cnt22_23 = 1800
>    cnt27_28  = 4800 + 4800
>     

Test border affects in histograms - put a frequency right at 2.0 and see
what bin it shows up in.

> test_sessionFreqHrs = TestCase $ do
>     assertEqual "test_sessionFreqHrs" 1.0 (snd (freqHist!!2))
>     assertEqual "test_sessionFreqHrs" 1.0 (snd (freqHist!!3))
>   where
>     s1 = defaultSession { sAllottedT = 60, frequency = 2.0 }
>     s2 = defaultSession { sAllottedT = 60, frequency = 2.2 }
>     freqHist = sessionFreqHrs [s1, s2]

> test_periodFreq = TestCase $ do
>     assertEqual "test_periodFreq" expected (periodFreq periods)
>   where
>     (_, periods) = getTestData
>     expected = [(0.0,0),(1.0,1905),(2.0,1815),(3.0,0),(4.0,0),(5.0,0),(6.0,0),(7.0,0),(8.0,0),(9.0,585)] ++ empty
>     empty = [(x, 0) | x <- [10.0, 11.0 .. 120.0]]

> -- This test is failing because auto-generated report range only needs to go to 11.
> test_sessionTP = TestCase $ do
>     assertEqual "test_sessionTP" expected (sessionTP periods)
>   where
>     (_, periods) = getTestData
>     expected = [(1.0,1),(2.0,5),(3.0,2),(4.0,3),(5.0,0),(6.0,4),(7.0,2),(8.0,0),(9.0,1)]

> -- This test is failing because auto-generated report range only needs to go to 3.
> test_sessionTP2 = TestCase $ do
>     assertEqual "test_sessionTP2" exp cnt
>   where
>     cnt = sessionTP ps
>     ps = [p1, p2, p1, p2, p1]
>     p1 = defaultPeriod {duration = 60, pDuration = 60}
>     p2 = defaultPeriod {duration = 150, pDuration = 150}
>     exp = [(1.0,3),(2.0,0),(3.0,2)]

> test_sessionTPQtrs = TestCase $ do
>     assertEqual "test_sessionTPQtrs" exp cnt
>   where
>     cnt = take 8 $ sessionTPQtrs ps
>     ps = [p1, p2, p1, p2, p1]
>     p1 = defaultPeriod {duration = 30, pDuration = 30}
>     p2 = defaultPeriod {duration = 105, pDuration = 105}
>     q  = quarter
>     exp = [(0,0),(1*q,0),(2*q,3),(3*q,0),(4*q,0),(5*q,0),(6*q,0),(7*q,2)]

> test_periodDuration = TestCase $ do
>     assertEqual "test_periodDuration" exp cnt
>   where
>     cnt = take 8 $ periodDuration ps
>     ps = [p1, p2, p1, p2, p1]
>     p1 = defaultPeriod {duration = 30, pDuration = 30}
>     p2 = defaultPeriod {duration = 105, pDuration = 105}
>     q  = quarter
>     exp = [(0,0),(1*q,0),(2*q,(3*30)),(3*q,0),(4*q,0),(5*q,0),(6*q,0),(7*q,(2*105))]

> test_periodStart = TestCase $ do
>     assertEqual "test_periodStart" exp cnt
>   where
>     dt1 = fromGregorian 2008 6 1 0 0 0
>     dt2 = fromGregorian 2008 6 3 0 0 0
>     ps = [p1, p2, p1, p2, p1]
>     p1 = defaultPeriod {startTime = dt1}
>     p2 = defaultPeriod {startTime = dt2}
>     cnt = take 8 $ periodStart dt1 ps
>     exp = [(0,3),(1,0),(2,2),(3,0),(4,0),(5,0),(6,0),(7,0)]

> test_sessionMinDuration = TestCase $ do
>     assertEqual "test_sessionMinDuration" exp cnt
>   where
>     cnt = take 8 $ sessionMinDuration ss
>     ss = [s1, s2, s1, s2, s1]
>     s1 = defaultSession {minDuration = 30}
>     s2 = defaultSession {minDuration = 105}
>     q  = quarter
>     exp = [(0,0),(1*q,0),(2*q,(3*30)),(3*q,0),(4*q,0),(5*q,0),(6*q,0),(7*q,(2*105))]

> test_freqTime = TestCase $ do
>     assertEqual "test_freqTime" expected (freqTime periods)
>   where
>     (_, periods) = getTestData
>     expected = [(1306251000,1.4),(1306272600,9.0),(1306279800,0.8),(1306304100,0.8),(1306317600,1.34),(1306324800,0.8),(1306337400,1.4),(1306356300,9.0),(1306366200,1.34),(1306389600,0.8),(1306400400,9.0),(1306404000,1.4),(1306411200,1.2),(1306441800,9.0),(1306456200,0.8),(1306476000,0.8),(1306483200,0.8),(1306490400,0.8)]

> test_periodBand = TestCase $ do
>     assertEqual "test_periodBand" expected (periodBand periods)
>   where
>     (_, periods) = getTestData
>     expected = [(P,31.75),(L,30.25),(S,0.0),(C,0.0),(X,9.75),(U,0.0),(K,0.0),(A,0.0),(Q,0.0),(W,0.0)]

> test_periodEfficiencyByBand = TestCase $ do
>     assertEqual "test_periodEfficiencyByBand" expected (periodEfficiencyByBand periods $ getEfficiencies $ length periods)
>   where
>     (_, periods) = getTestData
>     expected = [(P,7.88484),(L,15.026633),(S,0.0),(C,0.0),(X,2.468213),(U,0.0),(K,0.0),(A,0.0),(Q,0.0),(W,0.0)]

> test_decVsElevation = TestCase $ do
>     assertEqual "test_decVsElevation" expected (decVsElevation periods)
>   where
>     (_, periods) = getTestData
>     expected = [(39.5268,1.4),(39.982254,1.4),(51.95502,3.5e-2),(36.54371,0.0),(49.33683,-2.8e-2),(34.16056,0.0),(40.36268,1.3962634),(29.595814,-0.325),(-45.3517,0.0),(38.95833,-0.22),(55.17757,1.16),(49.47442,-2.82e-2),(44.587585,1.4),(67.858765,2.7260742),(53.3594,3.4906585e-2),(40.320133,-0.18448131),(37.103363,-0.18448131),(43.1762,0.0)]

> test_efficiencyVsFrequency = TestCase $ do
>     assertEqual "test_efficiencyVsFreq" expected (efficiencyVsFrequency sessions $ getEfficiencies $ length sessions)
>   where
>     (sessions, _) = getTestData
>     expected = [(1.4,0.98727703),(9.0,0.35925463),(0.8,0.23123395),(0.8,0.10321328),(1.34,0.4751926),(0.8,0.34717193),(1.4,0.21915126),(9.0,9.1130584e-2),(1.34,0.4631099),(0.8,0.33508924),(9.0,0.20706856),(1.4,7.904789e-2),(1.2,0.4510272),(9.0,0.32300654),(0.8,0.19498587),(0.8,6.696519e-2),(0.8,0.43894452),(0.8,0.31092384)]

> test_bandEfficiencyByTime = TestCase $ do
>   w <- getWeatherTest Nothing
>   -- result :: (atmospheric, tracking, surface, observing)
>   let dt = fromGregorian 2006 2 15 0 0 0
>   result <- bandEfficiencyByTime' w ss dt
>   assertEqual "test_bandEfficiencyByTime' 1" (0.98777103,0.9999681,0.9999935,0.98773295) (result !! 1)
>   let dt = fromGregorian 2006 5 15 0 0 0
>   result <- bandEfficiencyByTime' w ss dt
>   assertEqual "test_bandEfficiencyByTime' 2" (0.97235125,0.9967924,0.99886394,0.96812457) (result !! 3)
>   let dt = fromGregorian 2006 8 15 0 0 0
>   result <- bandEfficiencyByTime' w ss dt
>   assertEqual "test_bandEfficiencyByTime' 3" (0.3714521,0.963962,0.96827173,0.35170558) (result !! 6)
>   let dt = fromGregorian 2006 11 15 0 0 0
>   result <- bandEfficiencyByTime' w ss dt
>   assertEqual "test_bandEfficiencyByTime' 4" (0.8120911,0.99860746,0.9996801,0.8106577) (result !! 7)
>     where
>       ss = getOpenPSessions

> test_historicalFreq = TestCase $ do
>     assertEqual "test_historicalFreq" expected (historicalFreq periods)
>   where
>     (_, periods) = getTestData
>     expected = map getFreq testDataTemplate 
>     getFreq (dur, id, ra, dec, freq, rcvr, time) = freq

> test_historicalDec = TestCase $ do
>     assertEqual "test_historicalDec" expected (historicalDec periods)
>   where
>     (_, periods) = getTestData
>     expected = map getDecs testDataTemplate 
>     getDecs (dur, id, ra, dec, freq, rcvr, time) = dec

> test_historicalRA = TestCase $ do
>     assertEqual "test_historicalRA" expected (historicalRA periods)
>   where
>     (_, periods) = getTestData
>     expected = map getRAs testDataTemplate 
>     getRAs (dur, id, ra, dec, freq, rcvr, time) = ra

> test_historicalTime = TestCase $ do
>     assertEqual "test_historicalTime" expected (historicalTime periods)
>   where
>     (_, periods) = getTestData
>     expected = [1306251000,1306272600,1306279800,1306304100,1306317600,1306324800,1306337400,1306356300,1306366200,1306389600,1306400400,1306404000,1306411200,1306441800,1306456200,1306476000,1306483200,1306490400]

> test_historicalTime' = TestCase $ do
>     assertEqual "test_historicalTime'" expected (historicalTime' periods)
>   where
>     (_, periods) = getTestData
>     expected = [0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2]

> test_historicalLST = TestCase $ do
>     assertEqual "test_historicalLST" expected (historicalLST periods)
>   where
>     (_, periods) = getTestData
>     expected = [5.3110948,9.322046,13.700668,18.965042,21.85627,0.61379945,4.9924216,9.003373,13.649392,18.412397,20.417873,21.92198,3.1863542,9.453466,14.216471,17.976738,19.982214,23.608784]

> test_satisfactionRatio = TestCase $ do
>     assertEqual "test_satisfactionRatio" expected (satisfactionRatio sessions periods)
>   where
>     (sessions, periods) = getTestData
>     expected = [(0.0,0.0,0.0),(1.0,1.8728389,5.4432377e-3),(2.0,0.5881794,1.7513619e-3),(3.0,0.0,0.0),(4.0,0.0,0.0),(5.0,0.0,0.0),(6.0,0.0,0.0),(7.0,0.0,0.0),(8.0,0.0,0.0),(9.0,2.8955452,1.5186469e-2)] ++ i_cant_get_no_satisfaction
>     i_cant_get_no_satisfaction = [(x,0.0,0.0) | x <- [10.0,11.0 .. 120.0]]

> test_findScheduleGaps = TestCase $ do
>   assertEqual "test_findScheduleGaps" exp gaps
>     where
>   start = fromGregorian 2006 2 1 0 0 0
>   dur   = 24 * 60
>   gaps = findScheduleGaps start dur ps 
>   dt1 = fromGregorian 2006 2 1 1 30 0 -- gap at start for 1.5 hrs
>   dt2 = fromGregorian 2006 2 1 5 30 0 -- gap p1-p2 of 1 hr
>   dur1 = 120
>   dur2 = 240
>   end1 = dur1 `addMinutes` dt1
>   end2 = dur2 `addMinutes` dt2
>   p1 = Period 0 defaultSession dt1 dur1 0.0 Pending undefined False dur1 Nothing
>   p2 = Period 0 defaultSession dt2 dur2 0.0 Pending undefined False dur2 Nothing
>   ps = [p1, p2]
>   exp = [(start, 90), (end1, 120), (end2, (14*60)+30)]

> test_getOriginalSchedule' = TestCase $ do
>   assertEqual "test_getOriginalSchedule'" exp original
>     where
>   (observed, canceled, failedBackups) = getTestPeriods
>   original = getOriginalSchedule' observed canceled
>   exp = sort $ observed ++ failedBackups
>   

> test_breakdownSimulationTimes = TestCase $ do
>   assertEqual "test_breakdownSimulationTimes" exp times
>     where
>   (observed, canceled, failedBackups) = getTestPeriods
>   start = fromGregorian 2006 2 1 0 0 0
>   dur = 12*60
>   times = breakdownSimulationTimes [defaultSession] start dur observed canceled
>   sessHrs = 0.0 :: Float
>   simHrs = 12.0 :: Float
>   shdHrs = 7.0 :: Float
>   obsHrs = 5.0 :: Float
>   cnlHrs = 5.0 :: Float
>   bckHrs = 3.0 :: Float
>   totalDead = 7.0 :: Float
>   scheduledDead = 5.0 :: Float
>   failedBackup = 2.0 :: Float
>   sessAvHrs = 0.0 :: Float
>   sessBackupHrs = 0.0 :: Float
>   sessAvBckp = 0.0 :: Float
>   exp = (simHrs, sessHrs, sessBackupHrs, sessAvHrs, sessAvBckp, shdHrs, obsHrs, cnlHrs, bckHrs, totalDead, scheduledDead, failedBackup)

Test utilities

> getTestPeriods :: ([Period], [Period], [Period])
> getTestPeriods = (observed, canceled, failedBackups)
>   where
>   start = fromGregorian 2006 2 1 0 0 0
>   dur = 60
>   dts = [(2*i*60) `addMinutes` start | i <- [1..5]]
>   observed = zipWith mkPeriod dts [True, True, True, False, False] 
>   mkPeriod dt backup = Period 0 defaultSession dt dur 0.0 Pending undefined backup dur Nothing
>   canceled' = take 3 observed
>   canceledDts = [start, (5*60) `addMinutes` start]
>   failedBackups = zipWith mkPeriod canceledDts [False, False]
>   canceled = sort $ failedBackups ++ canceled'
>   

> getTestPeriod :: Period
> getTestPeriod = head . periods $ s
>     where
>   s' = defaultSession { frequency=2.0, ra=3.7, dec=(-2.8), receivers=[[Rcvr1_2]], band=L, grade=4.0 }
>   start = fromGregorian 2006 6 20 12 15 0
>   scheduled = fromGregorian 2006 6 20 0 0 0
>   p = defaultPeriod { session = s', startTime = start, duration = 285, pForecast = scheduled}
>   s = makeSession s' [] [p]

Can you believe it?  The dates I chose above fall right into one of 
the gaps in the gbt_weather data.  So, another period that avoids these.

> getTestPeriod2 :: Period
> getTestPeriod2 = head . periods $ s
>     where
>   s' = defaultSession { frequency=27.5, ra=3.7, dec=(-2.8), receivers=[[Rcvr26_40]], band=A, grade=4.0 }
>   start = fromGregorian 2006 7 20 12 15 0
>   scheduled = fromGregorian 2006 7 20 0 0 0
>   p = defaultPeriod { session = s', startTime = start, duration = 120, pForecast = scheduled}
>   s = makeSession s' [] [p]

> getEfficiencies    :: Int -> [Float]
> getEfficiencies n =
>     [fst $ randomR (0.0, 1.0) $ mkStdGen i | i <- [0 .. n]]

> assertAlmostEqual :: String -> Int -> Float -> Float -> IO ()
> assertAlmostEqual name places expected value =
>     assertBool name $ abs (value - expected) < epsilon
>   where
>     epsilon = 1.0 / 10.0 ** fromIntegral places

> getTestSessions :: [Session]
> getTestSessions = ss
>   where
>     p = defaultProject { semester = "05C" }
>     ds = defaultSession { project = p
>                         , authorized = True
>                         , sAllottedS = 120
>                         , periods = [] }
>     s1 = ds { sId = 1, sAllottedT = 240 }
>     s2 = ds { sId = 2, sAllottedT = 120 }
>     s3 = ds { sId = 3, sAllottedT = 240 }
>     s4 = ds { sId = 4, sAllottedT = 120 }
>     s5 = ds { sId = 5, sAllottedT = 240 }
>     s6 = ds { sId = 6, sAllottedT = 120 }
>     ss' = [s1, s2, s3, s4, s5, s6]
>     dts = [ fromGregorian 2006 1 1 0 0 0 
>           , fromGregorian 2006 1 2 0 0 0 
>           , fromGregorian 2006 1 4 0 0 0 
>           , fromGregorian 2006 1 5 0 0 0 
>           , fromGregorian 2006 1 7 0 0 0 
>           , fromGregorian 2006 1 9 0 0 0 
>           ]
>     durs = [60, 120, 60, 120, 60, 120]
>     ps = zipWith3 mkPeriod ss' dts durs 
>     ss = updateSessions ss' ps [] [] []
>     mkPeriod s start dur = Period 0 s start dur 0.0 Scheduled start False dur Nothing

> getTestWindowSession :: Session
> getTestWindowSession = makeSession s' [w'] [p']
>   where
>     winStart = fromGregorian 2006 2 1 0 0 0
>     winDur   = 10*24*60
>     pStart   = fromGregorian 2006 2 8 5 30 0
>     scheduled = fromGregorian 2006 2 8 0 0 0
>     s' = defaultSession { sType = Windowed , receivers = [[Rcvr1_2]], frequency = 2.0, band=L, grade=4.0, ra=3.7, dec=(-2.8)}
>     p' = defaultPeriod { startTime = pStart
>                        , duration = 60*2 
>                        , session = s'
>                        , pForecast = scheduled}
>     wr = [(winStart, addMinutes winDur winStart)]
>     w' = defaultWindow { wSession = s' 
>                        , wTotalTime = 60*2
>                        , wRanges = wr }

> getTestWindowSession2 :: Session
> getTestWindowSession2 = makeSession s' [w'] [p']
>   where
>     winStart = fromGregorian 2006 3 1 0 0 0
>     winDur   = 10*24*60
>     pStart   = fromGregorian 2006 3 8 5 30 0
>     s' = defaultSession { sType = Windowed, receivers = [[Rcvr1_2]], frequency = 2.0, band=L, grade=4.0, ra=3.7, dec=(-2.8) }
>     p' = defaultPeriod { startTime = pStart
>                        , duration = 60*2 
>                        , session = s' }
>     wr = [(winStart, addMinutes winDur winStart)]
>     w' = defaultWindow { wSession = s' 
>                        , wTotalTime = 60*2
>                        , wRanges = wr }

Generates test data to use for staistics based off a template
that can be cut & past from somewhere else.

> getTestData :: ([Session], [Period])
> getTestData = (ss, concatMap periods ss)
>   where
>     ss = createTestData templateStart testDataTemplate 
>     templateStart = fromGregorian 2011 5 24 15 30 0
>     createTestData startDt template = map createSession $ addStart startDt template
>     createSession (start, dur, id, ra, dec, freq, rcvr, time) = makeSession (mkSession (start, dur, id, ra, dec, freq, rcvr, time)) [] [mkPeriod start dur]
>     mkSession (start, dur, id, ra, dec, freq, rcvr, time) =
>         defaultSession {sId = id
>                       , ra = ra
>                       , dec = dec
>                       , frequency = freq
>                       , receivers = [[rcvr]]
>                       , sAllottedT = floor $ time * 60.0
>                       , sAllottedS = floor $ time * 60.0
>                       , band = receiver2Band rcvr 
>                       , periods = [defaultPeriod { startTime = start
>                                                  , duration = dur
>                                                  }]
>                        }
>     mkPeriod start dur = defaultPeriod {startTime = start, duration = dur} 

> addStart :: DateTime -> [(Minutes, Int, Float, Float, Float, Receiver, Float)] -> [(DateTime, Minutes, Int, Float, Float, Float, Receiver, Float)] 
> addStart dt template = zipWith addStart' (getDateTimes dt (map getDur template)) template
>   where
>     addStart' start (dur, id, ra, dec, freq, rcvr, time) = (start, dur, id, ra, dec, freq, rcvr, time)
>     getDur (dur, id, ra, dec, freq, rcvr, time) = dur

> getDateTimes :: DateTime -> [Minutes] -> [DateTime]
> getDateTimes _  []     = []
> getDateTimes dt (x:xs) = dt:(getDateTimes (addMinutes x dt) xs)

The simplest way to create realistic test data was to actually grab
stuff from the production database.  So here was three days worth of
schedule, starting May 24, 2001, 15:30 (which was easy to dump
in the Django shell).

(duration (mins), session id, ra, dec, frequency, rcvr, allotemnt (hrs))

> testDataTemplate = [(360, 1259, 0.0, 1.4, 1.3999999999999999, Rcvr1_2, 1420.0),
>   (120, 1113, 1.0969394348799999, 1.4, 9.0, Rcvr8_10,59.25 ),
>   (405, 703, 3.7960911230900001, 0.035, 0.800, Rcvr_800, 270.0),
>   (225, 702, 5.6723200689800004, 0.0, 0.800, Rcvr_800, 150.0),
>   (120, 1075, 5.8564523050700004, -0.028, 1.340, Rcvr1_2,10.0 ),
>   (210, 702, 5.6723200689800004, 0.0, 0.800, Rcvr_800, 150.0),
>   (315, 1259, 0.0, 1.3962634016,1.400, Rcvr1_2, 1420.0),
>   (165, 1263, 2.7260741191400002, -0.325, 9.0, Rcvr8_10, 50.0),
>   (390, 1265, 7.2722052166400002e-06, 0.0, 1.340, Rcvr1_2, 168.0),
>   (180, 1047, 4.8092547538700003, -0.22, 0.80, Rcvr_800, 30.0),
>   (60, 1264, 4.7060912506700001, 1.16, 9.0, Rcvr8_10, 50.0),
>   (120, 1075, 5.8564523050700004, -0.0282, 1.3999999999999999, Rcvr1_2, 10.0),
>   (510, 1194, 7.2722052166400002e-06, 1.4, 1.2, Rcvr1_2, 168.0),
>   (240, 1263, -0.32537785394000002, 2.7260741191400002, 9.0, Rcvr8_10, 50.0),
>   (330, 703, 3.7960911230900001, 0.034906585039899997, 0.80, Rcvr_800, 270.0),
>   (120, 740, 4.8589966375499998, -0.18448130193599999, 0.80, Rcvr_800, 16.75),
>   (120, 740, 4.8589966375499998, -0.18448130193599999, 0.80, Rcvr_800, 16.75),
>   (315, 702, 5.6723200689800004, 0.0, 0.800, Rcvr_800, 150.0)]
