> module Antioch.StatisticsTests where

> import Antioch.DateTime
> import Antioch.Statistics
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Score
> import Antioch.Utilities
> import Antioch.Generators (generateTestData)
> import Antioch.GenerateSchedule (validSimulatedWindows)
> import Antioch.PProjects
> import Antioch.TimeAccounting
> import Antioch.ReceiverTemperatures
> import Antioch.Simulate (updateSessions)
> import Data.List
> import Test.HUnit
> import System.Random

TBF: there are about 10 tests that rely on TestGenerateData, which
is used for input to the tests.  These will produce different inputs
whenever the slightest change is made to Generators.lhs, breaking
these unit tests.  We need to remove that dependency.

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

TBF: refactor so that historical*Factors methods can take a test weather.

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
>   -- TBF: score the session
>   -- now socre the period, and make sure results match
>   w <- getWeatherTest Nothing
>   fcs <- periodSchdFactors getTestPeriod trackingEfficiency w []
>   assertEqual "test_periodSchdFactors_1" 0.9994102 (head fcs)
>   assertEqual "test_periodSchdFactors_2" 0.9973051 (last fcs)
>   fcs <- periodSchdFactors getTestPeriod2 trackingEfficiency w []
>   assertEqual "test_periodSchdFactors_3" 0.9761378  (head fcs)
>   assertEqual "test_periodSchdFactors_4" 0.97425276 (last fcs)

> test_periodObsFactors = TestCase $ do
>   w <- getWeatherTest Nothing
>   fcs <- periodObsFactors getTestPeriod2 trackingEfficiency w
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
>     mkPeriod s start dur = Period 0 s start dur 0.0 Pending undefined False dur


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
>     mkPeriod s start dur = Period 0 s start dur 0.0 Pending undefined False dur
>     fixed1 = [(schd!!1)]
>     fixed2 = [Period 0 s2 (dts!!1) 30 0.0 Pending undefined False 30]

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
>     (sessions, _) = generateTestData 100
>     expected = [(0.35577524,0.47623572),(2.1895847,4.252978e-2),(2.395574,0.53812695),(48.277756,-0.50425416),(0.37403882,1.0874313),(27.983438,-0.4260103),(22.2,0.39609724),(1.8783274,9.471178e-5),(8.674597,1.0906446),(38.425694,0.49002618),(47.804672,0.15560223),(18.604387,0.2770956),(24.520023,-0.19889463),(9.495309,-0.5555474),(22.2,0.73994595),(13.266174,-0.15411195),(41.56894,7.8556634e-2),(28.06007,1.0969883),(1.4235873,-0.32472938),(1.7123574,1.0468142),(4.0409746,4.9841534e-2),(27.817541,1.1699796),(21.733278,0.5070484),(1.0100694,1.0779898),(9.924717,0.9367015),(90.0,-5.2755535e-2),(90.0,0.77219695),(30.976189,-0.5833577),(3.9893498,0.7394532),(40.552467,-0.106884986),(9.9770775,-0.28538352),(90.0,0.47596583),(0.3440411,0.37868357),(1.0549526,4.339968e-2),(2.6116548,-0.43405452),(0.37288198,0.56981087),(1.770606,0.5513684),(13.8910675,0.8354136),(23.071808,0.16430223),(15.28557,-0.5656911),(24.506676,0.44070128),(4.086435,-8.372329e-2),(0.76902723,0.15594037),(26.09234,-0.47388044),(90.0,0.22856916),(30.003836,0.118912205),(90.0,1.1704198),(1.2099806,0.6355781),(14.697347,-0.4877464),(23.753284,-8.631324e-2),(37.23504,2.4290108e-3),(19.351748,6.967458e-2),(0.68542796,0.19237168),(1.2838671,-0.41489246),(15.120647,7.767485e-3),(31.441956,0.8703967),(3.5114658,1.2960639),(20.825266,1.0952467),(4.527053,1.0914029),(8.466557,-0.2660734),(22.2,1.0945269),(90.0,0.8139722),(3.3353465,0.37650114),(8.02989,-0.22396849),(1.6116482,0.91400814),(9.057315,0.44876957),(49.56528,0.84143883),(38.32644,0.7303613),(4.5937076,1.040249),(47.350327,0.977887),(90.0,1.0809405),(1.625175,1.5956035e-2),(42.026432,-0.49930158),(41.68712,-0.18309043),(0.71201384,-0.24313517),(1.1200557,0.4702638),(4.1243362,0.68266124),(90.0,0.42338145),(90.0,0.21835586),(90.0,-9.22699e-3),(8.135961,-0.32752123),(35.078262,-0.414729),(22.216305,0.27493513),(0.8902895,2.7394248e-2),(1.3830401,-0.506914),(2.0768716,0.40044433),(46.422607,-1.9969795e-2),(1.5195354,1.164524),(90.0,0.109786525),(14.000989,1.0115127e-2),(20.343851,-0.47125766),(23.646555,-0.4771885),(29.256842,0.32714063),(90.0,0.65024656),(4.468768,0.29171142),(22.2,0.59418243),(1.8171127,0.93078256),(42.239998,0.88392806),(44.476303,-0.41651148),(2.5952575,-0.2486362)]

> test_periodDecFreq = TestCase $ do
>     assertEqual "test_periodDecFreq" expected (periodDecFreq periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(22.2,0.88389367),(3.9791346,-0.27464512),(34.997387,-0.5072232),(22.2,0.8051078),(1.817084,-0.12282421),(0.38929772,0.5533878),(9.062773,-0.49110794),(21.288109,1.2163651),(9.887055,0.5001144),(90.0,1.0535471),(5.841917,1.0495619),(13.706154,0.47537318),(5.382965,7.299652e-2),(4.240201,0.53972495),(22.2,0.83740026),(46.85267,9.519441e-2),(23.766878,0.3817392),(44.56217,0.34255493),(8.523153,-0.1744196),(1.2686639,0.8163508),(2.5683708,0.1501175),(0.33646175,0.69803804),(2.150926,0.5510235),(18.28855,0.9723723),(0.71302366,0.37261942),(2.7124887,-0.24262673),(22.2,0.883044),(1.3873891,0.57971),(8.438935,-0.5057993),(90.0,-4.7268827e-2),(31.222557,1.0694193),(4.9130635,0.45509547),(3.2205627,0.13170244),(28.719046,0.4473911),(37.38616,-0.5354764),(20.754557,-0.18087214),(1.7516974,-0.31181464),(22.2,0.101155855),(8.093245,-0.18615025),(42.103703,0.42608637),(90.0,0.615602),(21.557297,0.20544977),(4.174094,0.18857574),(38.329906,-0.47331607),(20.877628,0.36253095),(5.06563,0.6454446),(45.884586,1.0306063),(24.93642,-3.0572036e-2),(21.695347,0.79800963),(8.285271,-0.48526937),(9.453254,-3.294235e-2),(1.3414816,0.31871307),(28.117617,0.5450085),(34.25819,1.0647041),(1.92829,-4.9259704e-2),(0.88665384,-6.863196e-2),(45.900887,1.6191654e-2),(1.8316113,1.1519129),(22.2,0.5075307),(15.162437,-0.32560778),(35.69246,-0.50103384),(1.8113804,0.99980825),(18.70927,0.64607185),(14.511671,-0.42784485),(49.117157,0.36737704),(90.0,0.41769913),(1.8326076,1.2501698),(45.136562,0.60944664),(12.405247,-0.43018067),(22.2,0.46265897),(1.0602586,1.0972357),(8.065807,-0.52266407),(1.1354592,0.6035951),(39.894382,6.256886e-2),(34.996918,0.27118987),(44.266125,0.8735829),(24.468071,0.20241466),(13.28478,0.7365372),(1.6283133,0.15346724),(22.2,0.9279352),(5.2392955,1.093586),(90.0,0.21715273),(0.87596864,-0.59806275),(1.1371654,0.25072753),(1.0038188,0.9953381),(90.0,0.9134297),(27.890638,-0.58189934),(0.9022349,-0.3025735),(22.2,7.7850685e-2),(9.933023,1.0481862),(5.205706,7.979844e-2),(44.507084,0.32950327),(3.4708266,0.38801876),(1.010767,0.1017484),(3.8727517,-2.6071698e-3),(90.0,1.0454168),(38.271523,0.5680049),(0.33771953,-0.26702267),(0.3352432,0.10543607),(4.2936463,-0.48442215)] 

> test_sessionDecRA = TestCase $ do
>     assertEqual "test_sessionDecRA" expected (sessionDecRA sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(1.0792952,0.47623572),(2.6042538,4.252978e-2),(4.0851264,0.53812695),(4.712389,-0.50425416),(0.4435295,1.0874313),(2.6694477,-0.4260103),(5.406888,0.39609724),(5.4558945,9.471178e-5),(0.40488172,1.0906446),(5.213649,0.49002618),(3.648202,0.15560223),(1.3925505,0.2770956),(5.007658,-0.19889463),(6.2680044,-0.5555474),(4.951024,0.73994595),(3.465972,-0.15411195),(0.16081977,7.8556634e-2),(0.18408342,1.0969883),(2.7106795,-0.32472938),(2.6413836,1.0468142),(1.0893681,4.9841534e-2),(0.972677,1.1699796),(1.3086164,0.5070484),(6.2034564,1.0779898),(4.651469,0.9367015),(0.8488927,-5.2755535e-2),(3.7047868,0.77219695),(0.41806483,-0.5833577),(4.857506,0.7394532),(0.32292676,-0.106884986),(1.9461274,-0.28538352),(1.3841348,0.47596583),(5.7449093,0.37868357),(2.2499232,4.339968e-2),(2.0350204,-0.43405452),(2.9462397,0.56981087),(5.2573795,0.5513684),(0.47654128,0.8354136),(3.7348216,0.16430223),(1.4268454,-0.5656911),(1.7751285,0.44070128),(1.8382459,-8.372329e-2),(1.4213305,0.15594037),(4.712389,-0.47388044),(3.028317,0.22856916),(1.7339851,0.118912205),(5.2986684,1.1704198),(1.3093021,0.6355781),(4.712389,-0.4877464),(4.8924823,-8.631324e-2),(0.8767221,2.4290108e-3),(3.9250374,6.967458e-2),(5.036791,0.19237168),(2.0228662,-0.41489246),(1.7912569,7.767485e-3),(2.6891441,0.8703967),(3.8266807,1.2960639),(0.32534027,1.0952467),(0.99251604,1.0914029),(1.1749816,-0.2660734),(2.8654163,1.0945269),(1.1807181,0.8139722),(0.53509593,0.37650114),(5.3834157,-0.22396849),(3.0285048,0.91400814),(0.7943857,0.44876957),(2.5061083,0.84143883),(0.6092942,0.7303613),(3.437586,1.040249),(4.2177706,0.977887),(0.5050463,1.0809405),(3.3635392,1.5956035e-2),(4.712389,-0.49930158),(3.9315567,-0.18309043),(3.188348,-0.24313517),(2.9540527,0.4702638),(5.724622,0.68266124),(0.15113473,0.42338145),(2.4142246,0.21835586),(1.6387138,-9.22699e-3),(4.7618637,-0.32752123),(4.709302,-0.414729),(4.0854535,0.27493513),(1.8577647,2.7394248e-2),(4.0247083,-0.506914),(5.1184354,0.40044433),(1.8054696,-1.9969795e-2),(5.461546,1.164524),(1.3000618,0.109786525),(1.790054,1.0115127e-2),(4.712389,-0.47125766),(5.0471725,-0.4771885),(5.2230425,0.32714063),(2.0568209,0.65024656),(1.6407765,0.29171142),(4.4810653,0.59418243),(3.009115,0.93078256),(5.611641,0.88392806),(6.045621,-0.41651148),(2.299907,-0.2486362)]

> test_periodDecRA = TestCase $ do
>     assertEqual "test_periodDecRA" expected (periodDecRA periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(5.611585,0.88389367),(2.0540617,-0.27464512),(0.4683349,-0.5072232),(1.2331495,0.8051078),(1.858526,-0.12282421),(4.318395,0.5533878),(0.5082016,-0.49110794),(2.809732,1.2163651),(2.154395,0.5001144),(3.4659338,1.0535471),(5.0218077,1.0495619),(5.2037144,0.47537318),(1.0976746,7.299652e-2),(4.984044,0.53972495),(0.4616151,0.83740026),(4.5176225,9.519441e-2),(1.9307709,0.3817392),(3.0387373,0.34255493),(2.892424,-0.1744196),(1.218741,0.8163508),(2.8819237,0.1501175),(1.3509327,0.69803804),(3.919384,0.5510235),(0.500196,0.9723723),(1.7087058,0.37261942),(3.0048926,-0.24262673),(3.7335176,0.883044),(4.6364684,0.57971),(4.712389,-0.5057993),(2.5668106,-4.7268827e-2),(4.504141,1.0694193),(5.0527186,0.45509547),(3.3063054,0.13170244),(2.0755796,0.4473911),(0.13702846,-0.5354764),(5.797039,-0.18087214),(3.9745328,-0.31181464),(3.467835,0.101155855),(5.0960636,-0.18615025),(1.5605367,0.42608637),(3.3877187,0.615602),(5.0438013,0.20544977),(5.034764,0.18857574),(4.712389,-0.47331607),(5.132405,0.36253095),(5.332925,0.6454446),(0.5710633,1.0306063),(4.975602,-3.0572036e-2),(5.490058,0.79800963),(4.712389,-0.48526937),(0.86012745,-3.294235e-2),(4.5613203,0.31871307),(2.8023837,0.5450085),(0.61573344,1.0647041),(3.27224,-4.9259704e-2),(4.496201,-6.863196e-2),(1.2774023,1.6191654e-2),(4.757482,1.1519129),(5.198938,0.5075307),(3.4918878,-0.32560778),(4.712389,-0.50103384),(4.1375093,0.99980825),(4.910763,0.64607185),(3.9552033,-0.42784485),(6.1777143,0.36737704),(2.9853556,0.41769913),(2.9414415e-2,1.2501698),(2.199093,0.60944664),(3.8368502,-0.43018067),(3.0276396,0.46265897),(0.24470434,1.0972357),(5.9110146,-0.52266407),(5.8411922,0.6035951),(3.8176427,6.256886e-2),(1.8009102,0.27118987),(5.5950785,0.8735829),(3.3100474,0.20241466),(5.495553,0.7365372),(1.0918067,0.15346724),(5.3032255,0.9279352),(4.747118,1.093586),(1.9564886,0.21715273),(5.5531106,-0.59806275),(5.5862136,0.25072753),(2.5419014,0.9953381),(1.7594695,0.9134297),(1.5704693,-0.58189934),(2.49827,-0.3025735),(0.36141396,7.7850685e-2),(0.6991122,1.0481862),(1.6114947,7.979844e-2),(1.8695315,0.32950327),(5.984838,0.38801876),(6.2035794,0.1017484),(5.098386,-2.6071698e-3),(6.020699,1.0454168),(1.1436119,0.5680049),(1.9358552,-0.26702267),(1.740989,0.10543607),(3.5923,-0.48442215)]


> test_sessionRA = TestCase $ do
>     assertEqual "test_sessionRA" expected (sessionRA sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(0.0,0.0),(1.0,3.0),(2.0,7.0),(3.0,2.0),(4.0,5.0),(5.0,6.0),(6.0,5.0),(7.0,7.0),(8.0,6.0),(9.0,2.0),(10.0,3.0),(11.0,5.0),(12.0,5.0),(13.0,2.0),(14.0,3.0),(15.0,4.0),(16.0,4.0),(17.0,1.0),(18.0,8.0),(19.0,4.0),(20.0,6.0),(21.0,6.0),(22.0,3.0),(23.0,0.0),(24.0,3.0)]

> test_periodRA = TestCase $ do
>     assertEqual "test_periodRA" expected (periodRA periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(0.0,0.0),(1.0,3.0),(2.0,5.0),(3.0,3.0),(4.0,1.0),(5.0,6.0),(6.0,3.0),(7.0,5.0),(8.0,7.0),(9.0,2.0),(10.0,3.0),(11.0,2.0),(12.0,6.0),(13.0,4.0),(14.0,4.0),(15.0,4.0),(16.0,3.0),(17.0,1.0),(18.0,9.0),(19.0,3.0),(20.0,11.0),(21.0,4.0),(22.0,4.0),(23.0,5.0),(24.0,2.0)]

> test_sessionDec = TestCase $ do
>     assertEqual "test_sessionDec" expected (sessionDec sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(-40.0,0.0),(-39.0,0.0),(-38.0,0.0),(-37.0,0.0),(-36.0,0.0),(-35.0,0.0),(-34.0,0.0),(-33.0,1.0),(-32.0,1.0),(-31.0,1.0),(-30.0,0.0),(-29.0,1.0),(-28.0,2.0),(-27.0,4.0),(-26.0,0.0),(-25.0,0.0),(-24.0,2.0),(-23.0,3.0),(-22.0,0.0),(-21.0,0.0),(-20.0,0.0),(-19.0,0.0),(-18.0,2.0),(-17.0,0.0),(-16.0,1.0),(-15.0,1.0),(-14.0,1.0),(-13.0,1.0),(-12.0,1.0),(-11.0,1.0),(-10.0,1.0),(-9.0,0.0),(-8.0,1.0),(-7.0,0.0),(-6.0,1.0),(-5.0,0.0),(-4.0,2.0),(-3.0,1.0),(-2.0,0.0),(-1.0,1.0),(0.0,1.0),(1.0,5.0),(2.0,1.0),(3.0,3.0),(4.0,1.0),(5.0,1.0),(6.0,0.0),(7.0,2.0),(8.0,0.0),(9.0,2.0),(10.0,1.0),(11.0,0.0),(12.0,1.0),(13.0,1.0),(14.0,1.0),(15.0,0.0),(16.0,2.0),(17.0,1.0),(18.0,0.0),(19.0,1.0),(20.0,0.0),(21.0,0.0),(22.0,2.0),(23.0,2.0),(24.0,0.0),(25.0,1.0),(26.0,2.0),(27.0,1.0),(28.0,2.0),(29.0,1.0),(30.0,1.0),(31.0,1.0),(32.0,1.0),(33.0,1.0),(34.0,0.0),(35.0,1.0),(36.0,0.0),(37.0,1.0),(38.0,1.0),(39.0,0.0),(40.0,1.0),(41.0,0.0),(42.0,1.0),(43.0,2.0),(44.0,0.0),(45.0,1.0),(46.0,0.0),(47.0,1.0),(48.0,1.0),(49.0,1.0),(50.0,1.0),(51.0,1.0),(52.0,0.0),(53.0,1.0),(54.0,2.0),(55.0,0.0),(56.0,0.0),(57.0,1.0),(58.0,0.0),(59.0,0.0),(60.0,2.0),(61.0,0.0),(62.0,2.0),(63.0,6.0),(64.0,0.0),(65.0,0.0),(66.0,0.0),(67.0,1.0),(68.0,2.0),(69.0,0.0),(70.0,0.0),(71.0,0.0),(72.0,0.0),(73.0,0.0),(74.0,0.0),(75.0,1.0),(76.0,0.0),(77.0,0.0),(78.0,0.0),(79.0,0.0),(80.0,0.0),(81.0,0.0),(82.0,0.0),(83.0,0.0),(84.0,0.0),(85.0,0.0),(86.0,0.0),(87.0,0.0),(88.0,0.0),(89.0,0.0),(90.0,0.0)]

> test_periodDec = TestCase $ do
>     assertEqual "test_periodDec" expected (periodDec periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(-40.0,0.0),(-39.0,0.0),(-38.0,0.0),(-37.0,0.0),(-36.0,0.0),(-35.0,0.0),(-34.0,1.0),(-33.0,1.0),(-32.0,0.0),(-31.0,0.0),(-30.0,1.0),(-29.0,2.0),(-28.0,3.0),(-27.0,3.0),(-26.0,0.0),(-25.0,0.0),(-24.0,2.0),(-23.0,0.0),(-22.0,0.0),(-21.0,0.0),(-20.0,0.0),(-19.0,0.0),(-18.0,1.0),(-17.0,2.0),(-16.0,0.0),(-15.0,2.0),(-14.0,0.0),(-13.0,1.0),(-12.0,0.0),(-11.0,0.0),(-10.0,2.0),(-9.0,1.0),(-8.0,0.0),(-7.0,1.0),(-6.0,0.0),(-5.0,0.0),(-4.0,0.0),(-3.0,1.0),(-2.0,2.0),(-1.0,2.0),(0.0,1.0),(1.0,1.0),(2.0,0.0),(3.0,0.0),(4.0,1.0),(5.0,3.0),(6.0,3.0),(7.0,1.0),(8.0,1.0),(9.0,2.0),(10.0,0.0),(11.0,1.0),(12.0,2.0),(13.0,1.0),(14.0,0.0),(15.0,1.0),(16.0,1.0),(17.0,0.0),(18.0,0.0),(19.0,2.0),(20.0,1.0),(21.0,1.0),(22.0,3.0),(23.0,1.0),(24.0,1.0),(25.0,1.0),(26.0,1.0),(27.0,2.0),(28.0,1.0),(29.0,1.0),(30.0,1.0),(31.0,1.0),(32.0,3.0),(33.0,1.0),(34.0,1.0),(35.0,2.0),(36.0,1.0),(37.0,1.0),(38.0,1.0),(39.0,0.0),(40.0,1.0),(41.0,0.0),(42.0,0.0),(43.0,1.0),(44.0,0.0),(45.0,0.0),(46.0,1.0),(47.0,2.0),(48.0,1.0),(49.0,0.0),(50.0,0.0),(51.0,3.0),(52.0,0.0),(53.0,1.0),(54.0,1.0),(55.0,0.0),(56.0,1.0),(57.0,0.0),(58.0,2.0),(59.0,0.0),(60.0,2.0),(61.0,3.0),(62.0,2.0),(63.0,2.0),(64.0,0.0),(65.0,0.0),(66.0,1.0),(67.0,0.0),(68.0,0.0),(69.0,0.0),(70.0,1.0),(71.0,0.0),(72.0,1.0),(73.0,0.0),(74.0,0.0),(75.0,0.0),(76.0,0.0),(77.0,0.0),(78.0,0.0),(79.0,0.0),(80.0,0.0),(81.0,0.0),(82.0,0.0),(83.0,0.0),(84.0,0.0),(85.0,0.0),(86.0,0.0),(87.0,0.0),(88.0,0.0),(89.0,0.0),(90.0,0.0)]

> test_sessionFreq = TestCase $ do
>     assertEqual "test_sessionFreq" expected (sessionFreq sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(0.0,0),(1.0,7260),(2.0,15375),(3.0,3675),(4.0,2880),(5.0,6705),(6.0,0),(7.0,0),(8.0,0),(9.0,4215),(10.0,4275),(11.0,0),(12.0,0),(13.0,0),(14.0,1170),(15.0,1065),(16.0,2370),(17.0,0),(18.0,0),(19.0,1095),(20.0,1035),(21.0,2490),(22.0,1755),(23.0,5355),(24.0,2820),(25.0,1305),(26.0,0),(27.0,1170),(28.0,1170),(29.0,1770),(30.0,945),(31.0,2205),(32.0,1335),(33.0,0),(34.0,0),(35.0,0),(36.0,1245),(37.0,0),(38.0,360),(39.0,2430),(40.0,0),(41.0,1680),(42.0,2835),(43.0,3180),(44.0,0),(45.0,540),(46.0,0),(47.0,1230),(48.0,1755),(49.0,1335),(50.0,1455),(51.0,0),(52.0,0),(53.0,0),(54.0,0),(55.0,0),(56.0,0),(57.0,0),(58.0,0),(59.0,0),(60.0,0),(61.0,0),(62.0,0),(63.0,0),(64.0,0),(65.0,0),(66.0,0),(67.0,0),(68.0,0),(69.0,0),(70.0,0),(71.0,0),(72.0,0),(73.0,0),(74.0,0),(75.0,0),(76.0,0),(77.0,0),(78.0,0),(79.0,0),(80.0,0),(81.0,0),(82.0,0),(83.0,0),(84.0,0),(85.0,0),(86.0,0),(87.0,0),(88.0,0),(89.0,0),(90.0,13125),(91.0,0),(92.0,0),(93.0,0),(94.0,0),(95.0,0),(96.0,0),(97.0,0),(98.0,0),(99.0,0),(100.0,0),(101.0,0),(102.0,0),(103.0,0),(104.0,0),(105.0,0),(106.0,0),(107.0,0),(108.0,0),(109.0,0),(110.0,0),(111.0,0),(112.0,0),(113.0,0),(114.0,0),(115.0,0),(116.0,0),(117.0,0),(118.0,0),(119.0,0),(120.0,0)]

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
>     (_, periods) = generateTestData 100
>     expected = [(0.0,0),(1.0,2805),(2.0,5040),(3.0,1200),(4.0,945),(5.0,1290),(6.0,930),(7.0,0),(8.0,0),(9.0,1230),(10.0,1290),(11.0,0),(12.0,0),(13.0,90),(14.0,960),(15.0,495),(16.0,390),(17.0,0),(18.0,0),(19.0,1125),(20.0,0),(21.0,570),(22.0,375),(23.0,3135),(24.0,540),(25.0,270),(26.0,0),(27.0,0),(28.0,60),(29.0,705),(30.0,0),(31.0,0),(32.0,435),(33.0,0),(34.0,0),(35.0,1350),(36.0,555),(37.0,0),(38.0,210),(39.0,540),(40.0,540),(41.0,0),(42.0,0),(43.0,315),(44.0,0),(45.0,1005),(46.0,1260),(47.0,345),(48.0,0),(49.0,0),(50.0,240),(51.0,0),(52.0,0),(53.0,0),(54.0,0),(55.0,0),(56.0,0),(57.0,0),(58.0,0),(59.0,0),(60.0,0),(61.0,0),(62.0,0),(63.0,0),(64.0,0),(65.0,0),(66.0,0),(67.0,0),(68.0,0),(69.0,0),(70.0,0),(71.0,0),(72.0,0),(73.0,0),(74.0,0),(75.0,0),(76.0,0),(77.0,0),(78.0,0),(79.0,0),(80.0,0),(81.0,0),(82.0,0),(83.0,0),(84.0,0),(85.0,0),(86.0,0),(87.0,0),(88.0,0),(89.0,0),(90.0,1860),(91.0,0),(92.0,0),(93.0,0),(94.0,0),(95.0,0),(96.0,0),(97.0,0),(98.0,0),(99.0,0),(100.0,0),(101.0,0),(102.0,0),(103.0,0),(104.0,0),(105.0,0),(106.0,0),(107.0,0),(108.0,0),(109.0,0),(110.0,0),(111.0,0),(112.0,0),(113.0,0),(114.0,0),(115.0,0),(116.0,0),(117.0,0),(118.0,0),(119.0,0),(120.0,0)]

> -- This test is failing because auto-generated report range only needs to go to 11.
> test_sessionTP = TestCase $ do
>     assertEqual "test_sessionTP" expected (sessionTP periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(1.0,3),(2.0,10),(3.0,12),(4.0,14),(5.0,8),(6.0,10),(7.0,11),(8.0,12),(9.0,13),(10.0,7),(11.0,0)]

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
>     (_, periods) = generateTestData 100
>     expected = [(1199145600,22.2),(1199232000,3.9791346),(1199232000,34.997387),(1199145600,22.2),(1199232000,1.817084),(1199145600,0.38929772),(1199232000,9.062773),(1199145600,21.288109),(1199145600,9.887055),(1199145600,90.0),(1199145600,5.841917),(1199232000,13.706154),(1199232000,5.382965),(1199232000,4.240201),(1199145600,22.2),(1199232000,46.85267),(1199232000,23.766878),(1199232000,44.56217),(1199145600,8.523153),(1199232000,1.2686639),(1199145600,2.5683708),(1199232000,0.33646175),(1199232000,2.150926),(1199232000,18.28855),(1199232000,0.71302366),(1199232000,2.7124887),(1199145600,22.2),(1199232000,1.3873891),(1199232000,8.438935),(1199232000,90.0),(1199145600,31.222557),(1199232000,4.9130635),(1199232000,3.2205627),(1199145600,28.719046),(1199232000,37.38616),(1199232000,20.754557),(1199145600,1.7516974),(1199232000,22.2),(1199145600,8.093245),(1199145600,42.103703),(1199145600,90.0),(1199232000,21.557297),(1199145600,4.174094),(1199232000,38.329906),(1199232000,20.877628),(1199145600,5.06563),(1199145600,45.884586),(1199145600,24.93642),(1199145600,21.695347),(1199145600,8.285271),(1199232000,9.453254),(1199145600,1.3414816),(1199145600,28.117617),(1199232000,34.25819),(1199145600,1.92829),(1199232000,0.88665384),(1199232000,45.900887),(1199145600,1.8316113),(1199145600,22.2),(1199145600,15.162437),(1199145600,35.69246),(1199232000,1.8113804),(1199145600,18.70927),(1199145600,14.511671),(1199232000,49.117157),(1199145600,90.0),(1199145600,1.8326076),(1199145600,45.136562),(1199232000,12.405247),(1199232000,22.2),(1199145600,1.0602586),(1199232000,8.065807),(1199232000,1.1354592),(1199145600,39.894382),(1199232000,34.996918),(1199232000,44.266125),(1199145600,24.468071),(1199145600,13.28478),(1199145600,1.6283133),(1199145600,22.2),(1199232000,5.2392955),(1199145600,90.0),(1199232000,0.87596864),(1199232000,1.1371654),(1199232000,1.0038188),(1199232000,90.0),(1199232000,27.890638),(1199232000,0.9022349),(1199145600,22.2),(1199232000,9.933023),(1199145600,5.205706),(1199145600,44.507084),(1199232000,3.4708266),(1199232000,1.010767),(1199232000,3.8727517),(1199232000,90.0),(1199145600,38.271523),(1199232000,0.33771953),(1199145600,0.3352432),(1199145600,4.2936463)]

> test_periodBand = TestCase $ do
>     assertEqual "test_periodBand" expected (periodBand periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(P,46.75),(L,84.0),(S,31.0),(C,41.75),(X,42.0),(U,32.25),(K,100.25),(A,73.25),(Q,52.75),(W,31.0)]

> test_periodEfficiencyByBand = TestCase $ do
>     assertEqual "test_periodEfficiencyByBand" expected (periodEfficiencyByBand periods $ getEfficiencies $ length periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(P,27.152431),(L,15.273281),(S,10.879611),(C,13.059364),(X,10.432823),(U,8.445928),(K,24.175377),(A,17.71191),(Q,18.855852),(W,7.2059493)]

> test_decVsElevation = TestCase $ do
>     assertEqual "test_decVsElevation" expected (decVsElevation periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [(30.58569,0.88389367),(11.414902,-0.27464512),(6.5965652,-0.5072232),(81.143875,0.8051078),(37.995907,-0.12282421),(-17.62645,0.5533878),(9.491768,-0.49110794),(26.03503,1.2163651),(42.747715,0.5001144),(10.460114,1.0535471),(22.739235,1.0495619),(-12.33963,0.47537318),(55.74774,7.299652e-2),(-4.164444,0.53972495),(45.446114,0.83740026),(-44.083252,9.519441e-2),(65.05243,0.3817392),(4.417404,0.34255493),(-36.200676,-0.1744196),(80.65527,0.8163508),(2.0039291,0.1501175),(76.777985,0.69803804),(-19.99022,0.5510235),(48.257893,0.9723723),(30.40247,0.37261942),(2.1407166,-0.24262673),(5.9207535,0.883044),(-18.343193,0.57971),(-65.39575,-0.5057993),(-15.030113,-4.7268827e-2),(9.746704,1.0694193),(-9.917023,0.45509547),(-40.036682,0.13170244),(31.737373,0.4473911),(11.628693,-0.5354764),(0.60157776,-0.18087214),(-48.209167,-0.31181464),(-26.410538,0.101155855),(-30.029861,-0.18615025),(61.102364,0.42608637),(-11.106964,0.615602),(-4.274727,0.20544977),(-22.895424,0.18857574),(-48.36946,-0.47331607),(-12.408501,0.36253095),(18.499962,0.6454446),(63.8961,1.0306063),(-25.295647,-3.0572036e-2),(32.25393,0.79800963),(-73.32042,-0.48526937),(49.679398,-3.294235e-2),(-27.786583,0.31871307),(24.3917,0.5450085),(53.00408,1.0647041),(-47.337708,-4.9259704e-2),(-55.096573,-6.863196e-2),(51.96526,1.6191654e-2),(14.496529,1.1519129),(-16.164978,0.5075307),(-43.29271,-0.32560778),(-80.269104,-0.50103384),(5.778801,0.99980825),(-14.348892,0.64607185),(-56.951233,-0.42784485),(38.185238,0.36737704),(-0.58826447,0.41769913),(37.947685,1.2501698),(65.31961,0.60944664),(-74.861496,-0.43018067),(-16.41188,0.46265897),(50.17063,1.0972357),(6.297348,-0.52266407),(38.48471,0.6035951),(-28.203308,6.256886e-2),(52.992615,0.27118987),(35.89401,0.8735829),(-37.198593,0.20241466),(1.2473907,0.7365372),(60.352173,0.15346724),(14.621445,0.9279352),(22.376823,1.093586),(12.047493,0.21715273),(-56.373672,-0.59806275),(22.56971,0.25072753),(26.511139,0.9953381),(72.33081,0.9134297),(-1.560318,-0.58189934),(-9.24913,-0.3025735),(52.174202,7.7850685e-2),(67.246475,1.0481862),(35.015358,7.979844e-2),(57.099796,0.32950327),(12.922447,0.38801876),(32.839573,0.1017484),(-15.342476,-2.6071698e-3),(30.517994,1.0454168),(83.97884,0.5680049),(4.7452316,-0.26702267),(53.87789,0.10543607),(-48.280273,-0.48442215)]

> test_efficiencyVsFrequency = TestCase $ do
>     assertEqual "test_efficiencyVsFreq" expected (efficiencyVsFrequency sessions $ getEfficiencies $ length sessions)
>   where
>     (sessions, _) = generateTestData 100
>     expected = [(0.35577524,0.98727703),(2.1895847,0.35925463),(2.395574,0.23123395),(48.277756,0.10321328),(0.37403882,0.4751926),(27.983438,0.34717193),(22.2,0.21915126),(1.8783274,9.1130584e-2),(8.674597,0.4631099),(38.425694,0.33508924),(47.804672,0.20706856),(18.604387,7.904789e-2),(24.520023,0.4510272),(9.495309,0.32300654),(22.2,0.19498587),(13.266174,6.696519e-2),(41.56894,0.43894452),(28.06007,0.31092384),(1.4235873,0.18290317),(1.7123574,5.4882497e-2),(4.0409746,0.42686182),(27.817541,0.29884115),(21.733278,0.17082047),(1.0100694,4.279983e-2),(9.924717,0.41477913),(90.0,0.28675845),(90.0,0.15873778),(30.976189,3.0717134e-2),(3.9893498,0.40269643),(40.552467,0.2746758),(9.9770775,0.14665508),(90.0,1.8634439e-2),(0.3440411,0.39061373),(1.0549526,0.2625931),(2.6116548,0.13457239),(0.37288198,6.5517426e-3),(1.770606,0.37853104),(13.8910675,0.2505104),(23.071808,0.12248972),(15.28557,0.99446905),(24.506676,0.36644834),(4.086435,0.23842767),(0.76902723,0.110407025),(26.09234,0.98238635),(90.0,0.35436565),(30.003836,0.22634497),(90.0,9.832433e-2),(1.2099806,0.97030365),(14.697347,0.34228295),(23.753284,0.2142623),(37.23504,8.624163e-2),(19.351748,0.45822093),(0.68542796,0.33020025),(1.2838671,0.20217961),(15.120647,7.415894e-2),(31.441956,0.44613823),(3.5114658,0.31811756),(20.825266,0.19009691),(4.527053,6.207624e-2),(8.466557,0.43405554),(22.2,0.30603486),(90.0,0.17801422),(3.3353465,4.9993545e-2),(8.02989,0.42197287),(1.6116482,0.29395217),(9.057315,0.16593152),(49.56528,3.791085e-2),(38.32644,0.40989017),(4.5937076,0.28186947),(47.350327,0.15384883),(90.0,2.5828153e-2),(1.625175,0.39780748),(42.026432,0.26978678),(41.68712,0.14176613),(0.71201384,1.3745457e-2),(1.1200557,0.38572478),(4.1243362,0.25770408),(90.0,0.12968343),(90.0,1.662761e-3),(90.0,0.3736421),(8.135961,0.24562141),(35.078262,0.11760074),(22.216305,0.98958004),(0.8902895,0.3615594),(1.3830401,0.23353872),(2.0768716,0.10551804),(46.422607,0.97749734),(1.5195354,0.3494767),(90.0,0.22145602),(14.000989,9.343535e-2),(20.343851,0.96541464),(23.646555,0.337394),(29.256842,0.20937333),(90.0,8.135265e-2),(4.468768,0.95333195),(22.2,0.3253113),(1.8171127,0.19729063),(42.239998,6.9269955e-2),(44.476303,0.94124925),(2.5952575,0.3132286)]

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
>     (_, periods) = generateTestData 100
>     expected = [22.2,3.9791346,34.997387,22.2,1.817084,0.38929772,9.062773,21.288109,9.887055,90.0,5.841917,13.706154,5.382965,4.240201,22.2,46.85267,23.766878,44.56217,8.523153,1.2686639,2.5683708,0.33646175,2.150926,18.28855,0.71302366,2.7124887,22.2,1.3873891,8.438935,90.0,31.222557,4.9130635,3.2205627,28.719046,37.38616,20.754557,1.7516974,22.2,8.093245,42.103703,90.0,21.557297,4.174094,38.329906,20.877628,5.06563,45.884586,24.93642,21.695347,8.285271,9.453254,1.3414816,28.117617,34.25819,1.92829,0.88665384,45.900887,1.8316113,22.2,15.162437,35.69246,1.8113804,18.70927,14.511671,49.117157,90.0,1.8326076,45.136562,12.405247,22.2,1.0602586,8.065807,1.1354592,39.894382,34.996918,44.266125,24.468071,13.28478,1.6283133,22.2,5.2392955,90.0,0.87596864,1.1371654,1.0038188,90.0,27.890638,0.9022349,22.2,9.933023,5.205706,44.507084,3.4708266,1.010767,3.8727517,90.0,38.271523,0.33771953,0.3352432,4.2936463] 

> test_historicalDec = TestCase $ do
>     assertEqual "test_historicalDec" expected (historicalDec periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [0.88389367,-0.27464512,-0.5072232,0.8051078,-0.12282421,0.5533878,-0.49110794,1.2163651,0.5001144,1.0535471,1.0495619,0.47537318,7.299652e-2,0.53972495,0.83740026,9.519441e-2,0.3817392,0.34255493,-0.1744196,0.8163508,0.1501175,0.69803804,0.5510235,0.9723723,0.37261942,-0.24262673,0.883044,0.57971,-0.5057993,-4.7268827e-2,1.0694193,0.45509547,0.13170244,0.4473911,-0.5354764,-0.18087214,-0.31181464,0.101155855,-0.18615025,0.42608637,0.615602,0.20544977,0.18857574,-0.47331607,0.36253095,0.6454446,1.0306063,-3.0572036e-2,0.79800963,-0.48526937,-3.294235e-2,0.31871307,0.5450085,1.0647041,-4.9259704e-2,-6.863196e-2,1.6191654e-2,1.1519129,0.5075307,-0.32560778,-0.50103384,0.99980825,0.64607185,-0.42784485,0.36737704,0.41769913,1.2501698,0.60944664,-0.43018067,0.46265897,1.0972357,-0.52266407,0.6035951,6.256886e-2,0.27118987,0.8735829,0.20241466,0.7365372,0.15346724,0.9279352,1.093586,0.21715273,-0.59806275,0.25072753,0.9953381,0.9134297,-0.58189934,-0.3025735,7.7850685e-2,1.0481862,7.979844e-2,0.32950327,0.38801876,0.1017484,-2.6071698e-3,1.0454168,0.5680049,-0.26702267,0.10543607,-0.48442215]

> test_historicalRA = TestCase $ do
>     assertEqual "test_historicalRA" expected (historicalRA periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [5.611585,2.0540617,0.4683349,1.2331495,1.858526,4.318395,0.5082016,2.809732,2.154395,3.4659338,5.0218077,5.2037144,1.0976746,4.984044,0.4616151,4.5176225,1.9307709,3.0387373,2.892424,1.218741,2.8819237,1.3509327,3.919384,0.500196,1.7087058,3.0048926,3.7335176,4.6364684,4.712389,2.5668106,4.504141,5.0527186,3.3063054,2.0755796,0.13702846,5.797039,3.9745328,3.467835,5.0960636,1.5605367,3.3877187,5.0438013,5.034764,4.712389,5.132405,5.332925,0.5710633,4.975602,5.490058,4.712389,0.86012745,4.5613203,2.8023837,0.61573344,3.27224,4.496201,1.2774023,4.757482,5.198938,3.4918878,4.712389,4.1375093,4.910763,3.9552033,6.1777143,2.9853556,2.9414415e-2,2.199093,3.8368502,3.0276396,0.24470434,5.9110146,5.8411922,3.8176427,1.8009102,5.5950785,3.3100474,5.495553,1.0918067,5.3032255,4.747118,1.9564886,5.5531106,5.5862136,2.5419014,1.7594695,1.5704693,2.49827,0.36141396,0.6991122,1.6114947,1.8695315,5.984838,6.2035794,5.098386,6.020699,1.1436119,1.9358552,1.740989,3.5923]

> test_historicalTime = TestCase $ do
>     assertEqual "test_historicalTime" expected (historicalTime periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [1199145600,1199232000,1199232000,1199145600,1199232000,1199145600,1199232000,1199145600,1199145600,1199145600,1199145600,1199232000,1199232000,1199232000,1199145600,1199232000,1199232000,1199232000,1199145600,1199232000,1199145600,1199232000,1199232000,1199232000,1199232000,1199232000,1199145600,1199232000,1199232000,1199232000,1199145600,1199232000,1199232000,1199145600,1199232000,1199232000,1199145600,1199232000,1199145600,1199145600,1199145600,1199232000,1199145600,1199232000,1199232000,1199145600,1199145600,1199145600,1199145600,1199145600,1199232000,1199145600,1199145600,1199232000,1199145600,1199232000,1199232000,1199145600,1199145600,1199145600,1199145600,1199232000,1199145600,1199145600,1199232000,1199145600,1199145600,1199145600,1199232000,1199232000,1199145600,1199232000,1199232000,1199145600,1199232000,1199232000,1199145600,1199145600,1199145600,1199145600,1199232000,1199145600,1199232000,1199232000,1199232000,1199232000,1199232000,1199232000,1199145600,1199232000,1199145600,1199145600,1199232000,1199232000,1199232000,1199232000,1199145600,1199232000,1199145600,1199145600]

> test_historicalTime' = TestCase $ do
>     assertEqual "test_historicalTime'" expected (historicalTime' periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

> test_historicalLST = TestCase $ do
>     assertEqual "test_historicalLST" expected (historicalLST periods)
>   where
>     (_, periods) = generateTestData 100
>     expected = [3.2177482,3.784827,5.2889338,5.106238,5.2889338,3.2177482,5.171948,2.5993931,4.4711704,2.7163792,2.7163792,4.921263,4.16921,3.4171565,5.858291,4.2861958,5.924001,4.921263,2.4656947,5.0382495,4.721855,6.2916718,3.0327735,5.7903028,2.0300357,6.42537,4.9725394,5.7903028,4.16921,2.665103,4.9725394,3.9185255,2.0300357,3.3514466,3.166472,3.5341425,5.9752774,4.5368805,3.4684327,3.9698017,2.9670637,2.28072,3.9698017,2.782089,4.0355115,2.7163792,3.8528156,3.1007621,2.3487086,4.9725394,3.283458,3.6021311,5.223224,5.5396185,2.2150104,4.7875648,4.419894,5.858291,5.724593,4.604869,5.9752774,3.5341425,6.35966,5.4739084,3.4171565,3.9698017,5.724593,6.35966,2.163734,2.0300357,4.4711704,2.0300357,2.665103,5.858291,4.670579,2.5314045,1.8473397,5.858291,4.220486,4.3541846,2.0300357,1.8473397,6.42537,2.163734,3.0327735,5.6733165,1.9130496,3.667841,2.5993931,3.4171565,2.9670637,5.106238,4.921263,3.166472,2.782089,5.2889338,4.4711704,2.665103,5.4739084,4.9725394]

> test_satisfactionRatio = TestCase $ do
>     assertEqual "test_satisfactionRatio" expected (satisfactionRatio sessions periods)
>   where
>     (sessions, periods) = generateTestData 100
>     expected = [(0.0,0.0,0.0),(1.0,1.2591121,1.3169337e-2),(2.0,1.0682762,8.335549e-3),(3.0,1.0641235,1.7016387e-2),(4.0,1.0693195,1.9268924e-2),(5.0,0.6269878,9.670082e-3),(6.0,1.0,1.0),(7.0,0.0,0.0),(8.0,0.0,0.0),(9.0,0.9509894,1.5020661e-2),(10.0,0.98338085,1.516676e-2),(11.0,0.0,0.0),(12.0,0.0,0.0),(13.0,1.0,1.0),(14.0,2.6739516,4.780616e-2),(15.0,1.5146899,3.7712652e-2),(16.0,0.53627115,1.5042432e-2),(17.0,0.0,0.0),(18.0,0.0,0.0),(19.0,3.348163,5.529632e-2),(20.0,0.0,0.0),(21.0,0.7460083,1.7309006e-2),(22.0,0.6963416,1.9919233e-2),(23.0,1.9078588,1.8875279e-2),(24.0,0.62404054,1.48758525e-2),(25.0,0.6742507,2.2730315e-2),(26.0,0.0,0.0),(27.0,0.0,0.0),(28.0,0.16712198,1.195154e-2),(29.0,1.2980279,2.7080419e-2),(30.0,0.0,0.0),(31.0,0.0,0.0),(32.0,1.0618818,2.820314e-2),(33.0,0.0,0.0),(34.0,0.0,0.0),(35.0,1.0,1.0),(36.0,1.4527531,3.4159478e-2),(37.0,0.0,0.0),(38.0,1.9010123,7.2667666e-2),(39.0,0.7241952,1.7263334e-2),(40.0,1.0,1.0),(41.0,0.0,0.0),(42.0,0.0,0.0),(43.0,0.32281342,1.0075399e-2),(44.0,0.0,0.0),(45.0,6.065135,0.10597987),(46.0,1.0,1.0),(47.0,0.9140757,2.7260795e-2),(48.0,0.0,0.0),(49.0,0.0,0.0),(50.0,0.53754693,1.9221032e-2),(51.0,0.0,0.0),(52.0,0.0,0.0),(53.0,0.0,0.0),(54.0,0.0,0.0),(55.0,0.0,0.0),(56.0,0.0,0.0),(57.0,0.0,0.0),(58.0,0.0,0.0),(59.0,0.0,0.0),(60.0,0.0,0.0),(61.0,0.0,0.0),(62.0,0.0,0.0),(63.0,0.0,0.0),(64.0,0.0,0.0),(65.0,0.0,0.0),(66.0,0.0,0.0),(67.0,0.0,0.0),(68.0,0.0,0.0),(69.0,0.0,0.0),(70.0,0.0,0.0),(71.0,0.0,0.0),(72.0,0.0,0.0),(73.0,0.0,0.0),(74.0,0.0,0.0),(75.0,0.0,0.0),(76.0,0.0,0.0),(77.0,0.0,0.0),(78.0,0.0,0.0),(79.0,0.0,0.0),(80.0,0.0,0.0),(81.0,0.0,0.0),(82.0,0.0,0.0),(83.0,0.0,0.0),(84.0,0.0,0.0),(85.0,0.0,0.0),(86.0,0.0,0.0),(87.0,0.0,0.0),(88.0,0.0,0.0),(89.0,0.0,0.0),(90.0,0.46182963,5.9318645e-3),(91.0,0.0,0.0),(92.0,0.0,0.0),(93.0,0.0,0.0),(94.0,0.0,0.0),(95.0,0.0,0.0),(96.0,0.0,0.0),(97.0,0.0,0.0),(98.0,0.0,0.0),(99.0,0.0,0.0),(100.0,0.0,0.0),(101.0,0.0,0.0),(102.0,0.0,0.0),(103.0,0.0,0.0),(104.0,0.0,0.0),(105.0,0.0,0.0),(106.0,0.0,0.0),(107.0,0.0,0.0),(108.0,0.0,0.0),(109.0,0.0,0.0),(110.0,0.0,0.0),(111.0,0.0,0.0),(112.0,0.0,0.0),(113.0,0.0,0.0),(114.0,0.0,0.0),(115.0,0.0,0.0),(116.0,0.0,0.0),(117.0,0.0,0.0),(118.0,0.0,0.0),(119.0,0.0,0.0),(120.0,0.0,0.0)] 

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
>   p1 = Period 0 defaultSession dt1 dur1 0.0 Pending undefined False dur1
>   p2 = Period 0 defaultSession dt2 dur2 0.0 Pending undefined False dur2
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
>   mkPeriod dt backup = Period 0 defaultSession dt dur 0.0 Pending undefined backup dur
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
>     mkPeriod s start dur = Period 0 s start dur 0.0 Scheduled start False dur

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

