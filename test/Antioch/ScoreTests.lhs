> module Antioch.ScoreTests where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Control.Monad.Trans  (lift)
> import Test.HUnit
> import Data.List            (zip4, zipWith4, zipWith5)
> import Data.Maybe           (isJust)
> import Data.Array.IArray    (elems)

Note: the keyword BETA throughout the unit tests denotes tests whose main
purpose is to cross check results between this haskell code and the 2008
DSS summer beta test python code.  Specifically, the python code found here:
/home/sandboxes/mclark/trunk/simulation/antioch
The reason why this branch should be used is that a bug was found in this
codes weather server used for unit tests (TWeather).  

> tests = TestList [
>     test_hourAngleLimit
>   , test_frequencyPressure
>   , test_frequencyPressureComparison
>   , test_rightAscensionPressure
>   , test_initBins
>   , test_receiver
>   , test_getReceivers
>   , test_zenithAngle
>   , test_zenithAngle2
>   , test_zenithAngleAtTransit
>   , test_minTsysPrime
>   , test_minimumObservingConditions
>   , test_observingEfficiency
>   , test_observingEfficiency2
>   , test_observingEfficiencyLimit
>   , test_efficiency
>   , test_zenithOpticalDepth
>   , test_zenithOpticalDepth2
>   , test_receiverTemperature
>   , test_minObservingEff
>   , test_kineticTemperature
>   , test_kineticTemperature2
>   , test_stringency
>   , test_projectCompletion
>   , test_politicalFactors
>   , test_trackingEfficiency
>   , test_trackingErrorLimit
>   , test_zenithAngleLimit
>   , test_surfaceObservingEfficiency
>   , test_scoreCV
>   , test_scoreCV2
>   , test_avgScoreForTime
>   , test_avgScoreForTime2
>   , test_score
>   , test_averageScore
>   , test_averageScore2
>   , test_obsAvailable
>   , test_obsAvailable2
>   , test_observerAvailable
>   , test_needsLowRFI
>   , test_lstExcepted
>   , test_enoughTimeBetween
>   , test_receiverBoost
>   , test_receiverBoost2
>   , test_observerOnSite
>   ]

> benchmark = do
>     start <- getCurrentTime
>     runTestTT tests
>     stop <- getCurrentTime
>     putStrLn $ "Test Execution Speed: " ++ show (diffSeconds stop start) ++ " seconds"

BETA: TestHourAngleLimit.py testcomputedScore
for some reason, the two tests only overlap for the first half of the 
tested time period

> test_hourAngleLimit = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 2
>     scores <- mapM (score' w) times
>     assertEqual "test_hourAngleLimit" expected scores
>   where
>     sess = findPSessionByName "LP"
>     score' w dt = do
>         [(_, Just s)] <- runScoring w [] (hourAngleLimit dt sess)
>         return s
>     times = [(60*h) `addMinutes'` dtLP | h <- [0..23]]
>     expected = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0,
>                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

> test_frequencyPressure = TestCase $ do
>     freqPressure <- runScoring undefined [] $ genFrequencyPressure pSessions
>     assertScoringResult "test_frequencyPressure" Nothing 5 1.35154 (freqPressure undefined . head $ pSessions)

Test that a frequency NOT in the initial bins gives a pressure of 1.0

> test_frequencyPressureComparison = TestCase $ do
>     freqPressure <- runScoring undefined [] $ genFrequencyPressure pSessions
>     assertScoringResult' "test_frequencyPressure comparison" Nothing 1.0 (freqPressure undefined . head $ ss)
>   where
>     ss = concatMap sessions pTestProjects

> test_rightAscensionPressure = TestCase $ do
>     raPressure <- runScoring undefined [] $ genRightAscensionPressure pSessions
>     assertScoringResult "test_rightAscensionPressure" Nothing 5 1.25729 (raPressure undefined . head $ pSessions)

> test_initBins = TestCase $ do
>     assertEqual "test_initBins1" expected result
>   where
>     accessor s = (round . rad2hrs . ra $ s) `mod` 24
>     expected  = [(0,0),(0,0),(0,0),(0,0),(0,0),(1320,420)
>                 ,(0,0),(0,0),(0,0),(0,0),(1080,480),(0,0)
>                 ,(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)
>                 ,(1200,720),(0,0),(0,0),(0,0),(0,0),(0,0)]
>     result    = elems $ initBins (0, 23) accessor pSessions

> test_receiver = TestCase $ do
>     let dt = fromGregorian 2006 6 15 12 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult' "test_receiver" Nothing 0.0 (receiver dt sess)
>     let dt = fromGregorian 2006 6 25 12 0 0
>     assertScoringResult' "test_receiver" Nothing 1.0 (receiver dt sess)
>     let dt = fromGregorian 2006 8 1 12 0 0
>     let sess = findPSessionByName "AS"
>     assertScoringResult' "test_receiver" Nothing 0.0 (receiver dt sess)

> test_getReceivers = TestCase $ do
>     assertEqual "test_getReceivers1" [Rcvr4_6, Rcvr12_18] result1
>     assertEqual "test_getReceivers2" [Rcvr1_2, Rcvr12_18] result2
>       where 
>         result1 = getReceivers (fromGregorian 2006 6 24 16 0 0) rSched
>         result2 = getReceivers (fromGregorian 2006 6 22 16 0 0) rSched

BETA: TestAtmosphericOpacity.py testgetZenithAngle

> test_zenithAngle = TestCase $ do
>    -- BETA: beta gets 63.88534, diff between Float vs. Double
>    let dt = fromGregorian 2006 10 15 12 0 0 
>    let sess = findPSessionByName "LP"
>    let result = zenithAngle dt sess
>    assertAlmostEqual "test_zenithAngle" 5 (deg2rad 63.704613) result 
>    let result = zenithAngle dt sessBug
>    assertAlmostEqual "test_zenithAngle" 4 (deg2rad 40.5076) result 
>    let result = zenithAngle dt sessBug2
>    assertAlmostEqual "test_zenithAngle" 4 (deg2rad 81.50164) result 

BETA: TestAtmosphericOpacity.py testHaskell

> test_zenithAngle2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 8 0 0
>     let dt1 = fromGregorian 2006 10 15 11 0 0
>     let sLP = findPSessionByName "LP" 
>     let za = zenithAngle dt1 sLP
>     -- BETA: difference due to Float vs. Double
>     assertAlmostEqual "test_zenithAngle2" 2 (deg2rad 75.3003270409) za 

BETA: TestAtmosphericOpacity testgetZenithAngle

> test_zenithAngleAtTransit = TestCase $ do
>    -- BETA
>    let sess = findPSessionByName "LP"
>    let result = zenithAngleAtTransit sess
>    assertEqual "test_zenithAngleAtTransit" (deg2rad 33.03313) result 
>    let result = zenithAngleAtTransit sessBug
>    assertAlmostEqual "test_zenithAngleAtTransit" 5 (deg2rad 30.98467) result 
>    let result = zenithAngleAtTransit sessBug2
>    assertAlmostEqual "test_zenithAngleAtTransit" 5 (deg2rad 44.62250) result 

> test_minTsysPrime = TestCase $ do
>    w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 2
>    -- session LP
>    let sess = findPSessionByName "LP"
>    Just result <- minTSysPrime w (frequency sess) (elevation sess)
>    assertAlmostEqual "test_minTsysPrime" 3 15.490067 result 
>    -- session AS
>    let sess = findPSessionByName "AS"
>    Just result <- minTSysPrime w (frequency sess) (elevation sess)
>    assertAlmostEqual "test_minTsysPrime" 3 25.958 result 
>    -- sessBug
>    Just result <- minTSysPrime w (frequency sessBug) (elevation sessBug)
>    assertAlmostEqual "test_minTsysPrime" 3 92.365046 result 
>    -- sessBug2
>    Just result <- minTSysPrime w (frequency sessBug2) (elevation sessBug2)
>    assertAlmostEqual "test_minTsysPrime" 4 29.858517 result 
>      where 
>        -- Guard against elevations < 5.0 degrees
>        elevation s = max (deg2rad 5.0)  (pi/2 - zenithAngle dt s)
>        dt = fromGregorian 2006 10 15 12 0 0

> test_minimumObservingConditions = TestCase $ do
>    let dt = fromGregorian 2006 10 13 16 0 0
>    w <- getWeather . Just $ dt
>    mocs <- mapM (moc w dt) sess
>    assertEqual "test_minimumObservingConditions" expected mocs
>   where
>     moc w dt s = do
>         Just result <- runScoring w [] (minimumObservingConditions dt s)
>         return result
>     names = ["GB","CV","LP","TX","VA","WV","AS"]
>     sess = concatMap (\name -> findPSessionsByName name) names
>     expected = [False, True, True, False, False, False, True]

> test_observingEfficiency = TestCase $ do
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     fs <- runScoring w [] (observingEfficiency dt s)
>     let result = eval fs
>     assertAlmostEqual "test_observingEfficiency" 4 0.857506 result

BETA: TestObservingEfficiency.py test_efficiency

> test_observingEfficiency2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 8 0 0
>     let dt1 = fromGregorian 2006 10 15 12 0 0
>     let dt2 = fromGregorian 2006 10 15 11 0 0
>     let sLP = findPSessionByName "LP" 
>     let sGB = findPSessionByName "GB" 
>     fs <- runScoring w [] (observingEfficiency dt1 sLP)
>     assertAlmostEqual "test_observingEfficiency2" 4 0.97984 (eval fs)
>     fs <- runScoring w [] (observingEfficiency dt2 sLP)
>     -- BETA: difference due to Float vs. Double
>     assertAlmostEqual "test_observingEfficiency2_2" 2 0.97567 (eval fs)
>     fs <- runScoring w [] (observingEfficiency dt1 sGB)
>     assertAlmostEqual "test_observingEfficiency2_3" 2 0.83052 (eval fs)

BETA: TestObservingEfficiencyLimit.testHaskell

> test_observingEfficiencyLimit = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     [(_, Just result)] <- runScoring w [] (observingEfficiencyLimit dt s)
>     -- BETA: diff probably due to Float vs. Double
>     assertAlmostEqual "test_observingEfficiencyLimit" 4 (2.92284277214e-4) result

BETA: TestAtmosphericOpacity.py testefficiency

> test_efficiency = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 2
>     let dt = fromGregorian 2006 10 15 12 0 0
>     let sess = findPSessionByName "LP"
>     assertResult "test_efficiency 1" (Just wdt) 2 0.98215 (efficiency dt sess)  
>     assertResult "test_efficiencyHA 2" (Just wdt) 2 0.72034 (efficiencyHA dt sess) 
>     let sess = findPSessionByName "WV"
>     assertResult "test_efficiency 3" (Just wdt) 2 0.89721 (efficiency dt sess) 
>     assertResult "test_efficiencyHA 4" (Just wdt) 2 0.70341 (efficiencyHA dt sess) 
>     let sess = findPSessionByName "AS"
>     assertResult "test_efficiency 5" (Just wdt) 2 0.9614152 (efficiency dt sess) 
>     assertResult "test_efficiencyHA 6" (Just wdt) 2 0.45480406 (efficiencyHA dt sess)
>     assertResult "test_efficiency 7" (Just wdt) 2 0.935551 (efficiency dt sessBug)
>     assertResult "test_efficiency 8" (Just wdt) 4 0.95340 (efficiency dt sessBug2) 
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     Just result <- runScoring w [] (efficiency dt s) 
>     assertEqual "test_efficiency 9" 0.87132007 result
>     Just result <- runScoring w [] (efficiencyHA dt s) 
>     assertEqual "test_efficiencyHA 10" 0.783711 result

BETA: TestAtmosphericOpacity.py testZenithOpticalDepth

> test_zenithOpticalDepth = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 2
>     let sess = findPSessionByName "LP"
>     assertResult "test_zenithOpticalDepth" (Just wdt) 5 0.00798 (zenithOpticalDepth dtLP sess)
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertResult "test_zenithOpticalDepth" (Just wdt) 5 0.0661772 (zenithOpticalDepth dt sessBug)
>     assertResult "test_zenithOpticalDepth" (Just wdt) 5 0.007394265 (zenithOpticalDepth dt sessBug2)

BETA: TestAtmosphericOpacity.py testHaskell

> test_zenithOpticalDepth2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 8 0 0
>     let dt1 = fromGregorian 2006 10 15 11 0 0
>     let sLP = findPSessionByName "LP" 
>     Just zod <- runScoring w [] (zenithOpticalDepth dt1 sLP)
>     assertEqual "test_zenithOpticalDepth2" 0.007960711 zod 

> test_receiverTemperature = TestCase $ do
>     let sess = findPSessionByName "LP"
>     assertEqual "test_receiverTemperature" 5.0 $ receiverTemperature dtLP sess
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertEqual "test_receiverTemperature" 60.0 $ receiverTemperature dt sessBug
>     assertEqual "test_receiverTemperature" 10.0 $ receiverTemperature dt sessBug2
>     -- pTestProjects session CV
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     let result = receiverTemperature dt s 
>     assertEqual "test_receiverTemperature" 5.0 result

> test_minObservingEff = TestCase $ do
>     -- pTestProjects session CV
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     let result = minObservingEff . frequency $ s
>     assertEqual "test_minObservingEff" 0.93819135 result

BETA: TestAtmosphericOpacity.py testkineticTemperature

> test_kineticTemperature = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 0
>     let sess = findPSessionByName "LP"
>     assertResult' "test_kineticTemperatureLP" (Just wdt) 257.49832 (kineticTemperature dtLP sess) 
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertResult' "test_kineticTemperatureBug" (Just wdt) 256.9823 (kineticTemperature dt sessBug2) 
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     Just result <- runScoring w [] (kineticTemperature dt s) 
>     assertEqual "test_kineticTemperatureCV" 271.3523 result

BETA: TestAtmosphericOpacity.py testHaskell

> test_kineticTemperature2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 8 0 0
>     let dt1 = fromGregorian 2006 10 15 11 0 0
>     let sLP = findPSessionByName "LP" 
>     Just kt <- runScoring w [] (kineticTemperature dt1 sLP)
>     assertEqual "test_kineticTemperature2" 257.41776 kt 

BETA: TestStringency.py testScore (first assert)

> test_stringency = TestCase $ do
>     let dt = fromGregorian 2006 10 15 18 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult "test_stringency" Nothing 5 1.40086 (stringency dt sess)
>     let sess = findPSessionByName "AS"
>     assertScoringResult "test_stringency" Nothing 5 1.03437 (stringency dt sess)

> makeTestProject :: Minutes -> Minutes -> Project
> makeTestProject tl tt = makeProject proj' tt ss'
>   where
>     proj' = defaultProject { pName = "time use test" }
>     ss''  = [
>         defaultSession {
>             periods = [defaultPeriod {duration = tt - tl}]
>           , sAlloted = tt
>           }
>       ]
>     ss'   = [ makeSession s (periods s) | s <- ss'' ]

BETA: TestProjectCompletion.py test_completion_score

> test_projectCompletion = TestCase $ do
>     let dt = fromGregorian 2006 10 15 18 0 0 -- don't need!
>     -- adjust the project's times to get desired results
>     let p = makeTestProject 28740 33812
>     let sess = findPSessionByName "LP"
>     let s = sess {project = p}
>     assertScoringResult "test_projectCompletion" Nothing 3 1.015 (projectCompletion dt s)

> test_politicalFactors = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 13 22 0 0
>     let dt = fromGregorian 2006 10 15 12 0 0
>     let s = head . filter (\s -> "CV" == (sName s)) . concatMap sessions $ pTestProjects
>     -- missing window, transit, observerOnSite, and ObserverAvailable
>     let politicalFactors = score [scienceGrade
>                           , thesisProject
>                           , projectCompletion]
>     fs <- runScoring w [] (politicalFactors dt s)
>     let result = eval fs
>     assertEqual "test_politicalFactors" 1.0024 result

BETA: TestTrackingEfficiency.py testefficiencyHaskell

> test_trackingEfficiency = TestCase $ do
>     -- session LP
>     let sess = findPSessionByName "LP"
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertScoringResult "test_trackingEfficiency lp" Nothing 4 0.99764 (trackingEfficiency dt sess)
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     [(_, Just result)] <- runScoring w [] (trackingEfficiency dt s)
>     assertAlmostEqual "test_trackingEfficiency cv" 3 0.9879579 result 

BETA: TestTrackingErrorLimit.py testHaskell testcomputedScore

> test_trackingErrorLimit = TestCase $ do
>     let dt = fromGregorian 2006 10 15 12 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult' "test_trackingErrorLimit" Nothing 1.0 (trackingErrorLimit dt sess)
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     [(_, Just result)] <- runScoring w [] (trackingErrorLimit dt s)
>     assertEqual "test_trackingErrorLimit" 1.0 result

BETA: TestZenithAngleLimit testScore

> test_zenithAngleLimit = TestCase $ do
>     let dt = fromGregorian 2006 10 15 0 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult' "test_zenithAngleLimit" Nothing 0.0 (zenithAngleLimit dt sess)

BETA: TestSurfaceObservingEfficiency testefficiency

> test_surfaceObservingEfficiency = TestCase $ do
>     let dt  = fromGregorian 2006 4 15 16 0 0
>     let wdt = Just $ fromGregorian 2006 4 15 0 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult "test_surfaceObservingEfficienyLP" wdt 5 0.99392 (surfaceObservingEfficiency dt sess)
>     let sess = findPSessionByName "WV"
>     assertScoringResult "test_surfaceObservingEfficienyWV" wdt 5 0.77517 (surfaceObservingEfficiency dt sess)

> test_scoreCV = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] $ genScore ss >>= \f -> f dt s
>     let result = eval fs
>     assertEqual "test_scoreCV" (1.0204386e-3) result  

New tests that do *not* match up to a 'beta test python code test', but rather
to use in conjunction with Pack tests.

> test_scoreCV2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 1 18 0 0
>     let dt = fromGregorian 2006 10 1 18 0 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] $ genScore ss >>= \f -> f dt s
>     let result = eval fs
>     assertAlmostEqual "test_scoreCV2" 3 3.9704554 result  

> test_scoreForTime = TestCase $ do
>     -- score on top of weather
>     w <- getWeather $ Just dt
>     fs <- runScoring w [] $ do
>         sf <- genScore ss
>         sf dt s
>     let w1Score = eval fs
>     -- use different forecast; should get different score
>     w <- getWeather $ Just dt2
>     fs <- runScoring w [] $ do
>         sf <- genScore ss
>         sf dt s
>     let w2Score = eval fs
>     assert (w1Score /= w2Score) 
>     -- now try to get the original score again, despite current weather obj
>     w3Score <- runScoring w [] $ do
>         sf <- genScore ss
>         scoreForTime sf dt False s
>     assertEqual "test_avgScoreForTime" w1Score w3Score
>   where
>     dt = fromGregorian 2006 10 1 18 0 0
>     dt2 = fromGregorian 2006 10 1 0 0 0
>     ss = getOpenPSessions
>     s = findPSessionByName "CV"
> 

> test_avgScoreForTime = TestCase $ do
>     -- score on top of weather
>     w <- getWeather $ Just dt
>     fs <- runScoring w [] $ do
>         sf <- genScore ss
>         sf dt s
>     let w1Score = eval fs
>     -- use different forecast; should get different score
>     w <- getWeather $ Just dt2
>     fs <- runScoring w [] $ do
>         sf <- genScore ss
>         sf dt s
>     let w2Score = eval fs
>     assert (w1Score /= w2Score) 
>     -- now try to get the original score again, despite current weather obj
>     w3Score <- runScoring w [] $ do
>         sf <- genScore ss
>         avgScoreForTimeRealWind sf dt 15 s
>     -- since we're using real (measured) wind, the scores should be close
>     -- but not equal
>     assert  (w1Score /= w3Score)
>     assertAlmostEqual "test_avgScoreForTime_2" 4 3.9365761 w3Score
>   where
>     dt = fromGregorian 2006 10 1 18 0 0
>     dt2 = fromGregorian 2006 10 1 0 0 0
>     ss = getOpenPSessions
>     s = findPSessionByName "CV"
> 

> test_avgScoreForTime2 = TestCase $ do
>     -- weather that shouldn't get used
>     w <- getWeather $ Just dummytime
>     -- score over a wide range of time, that includes zeros, and see
>     -- how it zeros out the scores.
>     avgScore <- runScoring w [] $ do
>         sf <- genScore [s]
>         avgScoreForTimeRealWind sf starttime (24*60) s
>     -- now limit the time window to an area w/ non-zero scores
>     avgScore2 <- runScoring w [] $ do
>         sf <- genScore [s]
>         avgScoreForTimeRealWind sf starttime2 (4*60) s
>     assertEqual "test_avgScoreForTime2_1" 0.0 avgScore
>     assertEqual "test_avgScoreForTime2_2" True (avgScore2 /= 0.0)
>   where
>     dummytime  = fromGregorian 2006 11 7 12 0 0
>     starttime  = fromGregorian 2006 11 8 12 0 0
>     starttime2 = fromGregorian 2006 11 8 22 0 0
>     s = defaultSession {sAlloted = 24*60, minDuration=2*60, maxDuration=6*60}
>     -- scoring using a weather from starttime gives these scores for this
>     -- session in 24 hours
>     --expScores = (replicate 39 0.0) ++ defaultScores ++ (replicate 22 0.0) 
> 

Test the 24-hour scoring profile of the default session, per quarter.

> test_score = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     let score' w dt = runScoring w [] $ do
>         fs <- genScore [sess]
>         s <- fs dt sess
>         return $ eval s
>     scores <- mapM (score' w) times
>     assertEqual "test_score" expected scores
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     score' w dt = runScoring w [] $ do
>         fs <- genScore [sess]
>         s  <- fs dt sess
>         return $ eval s
>     times = [(15*q) `addMinutes'` starttime | q <- [0..96]]
>     sess = defaultSession { sName = "singleton"
>                           , sAlloted = 24*60
>                           , minDuration = 2*60
>                           , maxDuration = 6*60
>                           }
>     expected = (replicate 39 0.0) ++ defaultScores ++ (replicate 22 0.0)

For defaultSession w/ sAlloted = 24*60; start time is  2006 11 8 12 0 0
plus 40 quarters.

> defaultScores = [3.2114944,3.2196305,3.2241328,2.8470442,3.0492089
>                 ,3.1139324,3.140008,3.187729,3.1933162,3.1966023
>                 ,3.1995883,3.2391315,3.239888,3.2483156,3.248886
>                 ,3.2764618,3.2764618,3.2766595,3.2766595,3.2787113
>                 ,3.2787113,3.278528,3.2781422,3.2795804,3.2791758
>                 ,3.2789626,3.2785032,3.2757215,3.2750494,3.274302
>                 ,3.273018,3.2735398,3.2719383,3.2699947,3.2675872,3.2657294]
> xdefaultScores = [3.2114944,3.2196305,3.2261546,2.8470442,3.0492089
>                 ,3.1299076,3.140008,3.1896837,3.1915457,3.1966023
>                 ,3.1995883,3.2383318,3.239888,3.2477167,3.248886
>                 ,3.2764618,3.2764618,3.2766595,3.2766595,3.2787113
>                 ,3.2787113,3.278528,3.2783365,3.2795804,3.2791758
>                 ,3.2787383,3.27825,3.2757215,3.2750494,3.273897
>                 ,3.273018,3.2730415,3.271333,3.2699947,3.2675872]

> test_bestDuration = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     bestDur <- runScoring w [] $ do
>         sf <- genScore ss
>         bestDuration sf starttime Nothing Nothing s
>     let expected = (s, 3.738552, 255)
>     assertEqual "test_bestDuration 1" expected bestDur
>     -- override the minimum and maximum
>     bestDur <- runScoring w [] $ do
>         sf <- genScore ss
>         bestDuration sf starttime (Just 0) (Just (4*60::Minutes)) s
>     let expected = (s, 3.7120736, 225)
>     assertEqual "test_bestDuration 2" expected bestDur
>   where
>     starttime = fromGregorian 2006 10 1 18 0 0

> test_bestDurations = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     let ss = concatMap sessions pTestProjects
>     bestDurs <- runScoring w [] $ do
>         sf <- genScore ss
>         bestDurations sf starttime Nothing Nothing ss
>     assertEqual "test_bestDurations 1" 10 (length bestDurs)
>     let (s, v, d) = bestDurs !! 1
>     assertEqual "test_bestDurations 2 n" "CV" (sName s)
>     assertEqual "test_bestDurations 2 v" 3.738552 v
>     assertEqual "test_bestDurations 2 d" 255 d
>     let (s, v, d) = bestDurs !! 6
>     assertEqual "test_bestDurations 3 n" "AS" (sName s)
>     assertEqual "test_bestDurations 3 v" 3.3932652 v
>     assertEqual "test_bestDurations 3 d" 375 d
>   where
>     starttime = fromGregorian 2006 10 1 18 0 0

> test_averageScore = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     let score' w dt = runScoring w [] $ do
>         fs <- genScore [sess]
>         s <- fs dt sess
>         return $ eval s
>     scores <- mapM (score' w) times
>     let scoreTotal = addScores scores
>     let expected = 0.0
>     assertEqual "test_score1" expected scoreTotal
>     avgScore <- runScoring w [] $ do
>         fs <- genScore [sess]
>         averageScore fs starttime sess
>     assertEqual "test_score2" expected avgScore
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     sess = defaultSession { sAlloted = 24*60 
>                           , minDuration = 2*60 
>                           , maxDuration = 6*60
>                           }
>     times = [(15*q) `addMinutes'` starttime | q <- [0..96]]

Look at the scores over a range where none are zero.

> test_averageScore2 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     (scoreTotal, scoreTotal', avgScore) <- runScoring w [] $ do
>         sf <- genScore [sess]
>         scores <- lift $ mapM (score' w sf) times
>         let scoreTotal = addScores scores
>         scoreTotal' <- totalScore sf dt dur sess
>         avgScore <- averageScore sf dt sess
>         return (scoreTotal, scoreTotal', avgScore)
>     --assertAlmostEqual "test_averageScore2_addScores" 3 expectedTotal scoreTotal
>     assertEqual "test_averageScore2_addScores" expectedTotal scoreTotal
>     assertAlmostEqual "test_averageScore2_totalScore" 3  expectedTotal scoreTotal'
>     assertAlmostEqual "test_averageScore2_avgScore" 3 expectedAvg avgScore
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     dur = 2*60
>     sess = defaultSession { sAlloted = 24*60 
>                           , minDuration = dur 
>                           , maxDuration = 6*60
>                           }
>     score' w sf dt = do
>         fs <- runScoring w [] (sf dt sess)
>         return $ eval fs
>     dt = (40*quarter) `addMinutes'` starttime -- start where scores /= 0
>     numQtrs = dur `div` quarter
>     times = [(q*quarter) `addMinutes'` dt | q <- [0..numQtrs-1]]
>     expectedTotal = 24.975002 :: Score  
>     expectedAvg = expectedTotal / (fromIntegral numQtrs)

> test_obsAvailable = TestCase $ do
>   assertEqual "test_obsAvailable_1" True (obsAvailable dt s)
>   assertEqual "test_obsAvailable_2" False (obsAvailable dt s2)
>   assertEqual "test_obsAvailable_3" True (obsAvailable dt2 s2)
>     where
>       dt  = fromGregorian 2006 2 1 0 0 0
>       dt2 = fromGregorian 2006 2 7 0 0 0
>       s   = defaultSession
>       s2  = defaultSession { project = p }
>       p   = defaultProject { observers = [o] }
>       o   = defaultObserver { blackouts = bs }
>       bs  = [(fromGregorian 2006 1 31 0 0 0, fromGregorian 2006 2 2 0 0 0)]

If none is sanctioned, then there should never be an observer available

> test_obsAvailable2 = TestCase $ do
>   assertEqual "test_obsAvailable2_1" True  (obsAvailable dt s)
>   assertEqual "test_obsAvailable2_2" False (obsAvailable dt s2)
>   assertEqual "test_obsAvailable2_3" False (obsAvailable dt2 s2)
>     where
>       dt  = fromGregorian 2006 2 1 0 0 0
>       dt2 = fromGregorian 2006 2 7 0 0 0
>       s   = defaultSession
>       s2  = defaultSession { project = p }
>       p   = defaultProject { observers = [o] }
>       o   = defaultObserver { blackouts = bs, sanctioned = False }
>       bs  = [(fromGregorian 2006 1 31 0 0 0, fromGregorian 2006 2 2 0 0 0)]

> test_observerAvailable = TestCase $ do
>   w <- getWeather Nothing
>   fs <- runScoring w [] (observerAvailable dt s)
>   assertEqual "test_observerAvailable_1" expTrue (eval fs)
>   fs <- runScoring w [] (observerAvailable dt s2)
>   assertEqual "test_observerAvailable_2" expFalse (eval fs)
>   fs <- runScoring w [] (observerAvailable dt2 s2)
>   assertEqual "test_observerAvailable_3" expTrue (eval fs)
>   fs <- runScoring w [] (observerAvailable dt s3)
>   assertEqual "test_observerAvailable_4" expFalse (eval fs)
>   fs <- runScoring w [] (observerAvailable dt2 s3)
>   assertEqual "test_observerAvailable_5" expTrue (eval fs)
>   fs <- runScoring w [] (observerAvailable dt3 s3)
>   assertEqual "test_observerAvailable_6" expTrue (eval fs)
>     where
>       dt  = fromGregorian 2006 2 1  0 0 0
>       dt2 = fromGregorian 2006 2 7  0 0 0
>       dt3 = fromGregorian 2006 2 11 0 0 0
>       s   = defaultSession
>       s2  = defaultSession { project = p }
>       p   = defaultProject { observers = [o] }
>       o   = defaultObserver { blackouts = bs }
>       bs  = [(fromGregorian 2006 1 31 0 0 0, fromGregorian 2006 2 2 0 0 0)]
>       bs2 = [(fromGregorian 2006 2 10 0 0 0, fromGregorian 2006 2 12 0 0 0)]
>       o2  = defaultObserver { blackouts = bs ++ bs2 }
>       p2  = defaultProject { observers = [o, o2] }
>       s3  = defaultSession { project = p2}
>       expTrue = 1.0
>       expFalse = 0.0

> test_needsLowRFI = TestCase $ do
>   w <- getWeather Nothing
>   assertEqual "test_needsLowRFI" True (isDayTime day)
>   assertEqual "test_needsLowRFI" False (isDayTime night)
>   fs <- runScoring w [] (needsLowRFI day sAnyTime)
>   assertEqual "test_needsLowRFI" 1.0 (eval fs)
>   fs <- runScoring w [] (needsLowRFI night sAnyTime)
>   assertEqual "test_needsLowRFI" 1.0 (eval fs)
>   fs <- runScoring w [] (needsLowRFI night sNightTime)
>   assertEqual "test_needsLowRFI" 1.0 (eval fs)
>   fs <- runScoring w [] (needsLowRFI day sNightTime)
>   assertEqual "test_needsLowRFI" 0.0 (eval fs)
>     where
>       day   = fromGregorian 2008 1 1 15 0 0 -- rfi day starts at 12:00 UT 
>       night = fromGregorian 2008 1 2 1 30 0 -- rfi night starts at 24:00 UT 
>       sAnyTime = findPSessionByName "CV"
>       sNightTime = sAnyTime { lowRFI = True }

> test_lstExcepted = TestCase $ do
>   w <- getWeather Nothing
>   fs <- runScoring w [] (lstExcepted dtClear sAnyTime)
>   assertEqual "test_lstExcpeted_1" 1.0 (eval fs)
>   fs <- runScoring w [] (lstExcepted dtClear sExclude1)
>   assertEqual "test_lstExcpeted_2" 1.0 (eval fs)
>   fs <- runScoring w [] (lstExcepted dtNotClear sExclude1)
>   assertEqual "test_lstExcpeted_3" 0.0 (eval fs)
>   fs <- runScoring w [] (lstExcepted dtClear sExclude2)
>   assertEqual "test_lstExcpeted_4" 0.0 (eval fs)
>   fs <- runScoring w [] (lstExcepted dtNotClear sExclude2)
>   assertEqual "test_lstExcpeted_5" 1.0 (eval fs)
>   assertEqual "test_lstExpected_6" True  (checkLst dt $ lstExclude sExclude1) 
>   assertEqual "test_lstExpected_7" False (checkLst dt $ lstExclude sExclude2) 
>   fs <- runScoring w [] (lstExcepted dt2 sExclude3)
>   assertEqual "test_lstExcpeted_8" 0.0 (eval fs)
>     where
>       dtClear   = fromGregorian 2008 1 1 15 0 0  
>       lstClear  = utc2lstHours dtClear 
>       dtNotClear= fromGregorian 2008 1 1 14 0 0  
>       lstNotClear  = utc2lstHours dtNotClear 
>       dt        = fromGregorian 2008 1 1 10 0 0  
>       dt2       = fromGregorian 2006 3 28 21 30  0
>       sAnyTime = findPSessionByName "CV"
>       sExclude1 = sAnyTime { lstExclude = [(12.0, 16.0)] }
>       sExclude2 = sAnyTime { lstExclude = [(16.0, 12.0)] }
>       sExclude3 = sAnyTime { lstExclude = [(4.4721766,8.396873)] }

> test_enoughTimeBetween = TestCase $ do
>   assertEqual "test_enoughTimeBetween_1" True r1
>   assertEqual "test_enoughTimeBetween_2" True r2
>   assertEqual "test_enoughTimeBetween_3" False r3
>   assertEqual "test_enoughTimeBetween_4" True r4
>   assertEqual "test_enoughTimeBetween_5" False r5
>   assertEqual "test_enoughTimeBetween_6" True r6
>   assertEqual "test_enoughTimeBetween_7" False r7
>   assertEqual "test_enoughTimeBetween_8" True r8
>   w <- getWeather Nothing
>   fs <- runScoring w [] (enoughTimeBetween tdt1 s1)
>   assertEqual "test_enoughTimeBetween_9" 1.0 (eval fs)
>   --fs <- runScoring w [] (lstExcepted dtClear sExclude1)
>     where
>       -- test times
>       tdt1 = fromGregorian 2006 1 1 1 0 0
>       -- session has no periods - no problem
>       s1 = defaultSession { timeBetween = 100, periods = [] }
>       r1 = enoughTimeBetween' tdt1 s1
>       -- now some seemingly innocent periods
>       dt1 = fromGregorian 2006 1 1 0 0 0 
>       dt2 = fromGregorian 2006 1 1 3 0 0 
>       dt3 = fromGregorian 2006 1 1 6 0 0 
>       --ps = map (mkPeriod s1') [dt1, dt2, dt3]
>       --s1 = makeSession s1' ps
>       -- session has no timebetween - no problem
>       s2' = defaultSession { timeBetween = 0 }
>       ps2 = map (mkPeriod s2') [dt1, dt2, dt3]
>       s2 = makeSession s2' ps2
>       r2 = enoughTimeBetween' tdt1 s2
>       -- now potential problems - session w/ timebetween & periods
>       s3' = defaultSession { timeBetween = 60 }
>       ps3 = map (mkPeriod s3') [dt1, dt2, dt3]
>       s3 = makeSession s3' ps3
>       -- overlap case
>       r3 = enoughTimeBetween' tdt1 s3
>       -- vanilla test - far after last period ends
>       tdt2 = fromGregorian 2006 1 1 12 0 0
>       r4 = enoughTimeBetween' tdt2 s3
>       --  too close after last period ends
>       tdt3 = fromGregorian 2006 1 1 7 30 0
>       r5 = enoughTimeBetween' tdt3 s3
>       -- between periods, but far enough away from all of them
>       tdt4 = fromGregorian 2006 1 1 5 0 0
>       r6 = enoughTimeBetween' tdt4 s3
>       -- between but too close
>       tdt5 = fromGregorian 2006 1 1 5 30 0
>       r7 = enoughTimeBetween' tdt5 s3
>       -- far enough back in the past
>       tdt6 = fromGregorian 2005 12 31 12 0 0
>       r8 = enoughTimeBetween' tdt6 s3
>       -- utility
>       mkPeriod s dt = defaultPeriod { session = s
>                                     , startTime = dt
>                                     , duration = 60 }
>       

TBF: this test assumes the Rcvr getting boosted is Rcvr_1070.

> test_receiverBoost = TestCase $ do
>   assertEqual "test_receiverBoost_1"  False (receiverBoost' s1)
>   assertEqual "test_receiverBoost_2"  False (receiverBoost' s2)
>   assertEqual "test_receiverBoost_3"  False (receiverBoost' s3)
>   assertEqual "test_receiverBoost_4"  False (receiverBoost' s4)
>   assertEqual "test_receiverBoost_5"  True  (receiverBoost' s5)
>   assertEqual "test_receiverBoost_6"  False (receiverBoost' s6)
>   assertEqual "test_receiverBoost_7"  True  (receiverBoost' s7)
>   assertEqual "test_receiverBoost_8"  False (receiverBoost' s8)
>   assertEqual "test_receiverBoost_9"  False (receiverBoost' s9)
>   assertEqual "test_receiverBoost_10" False (receiverBoost' s10)
>     where
>       boost = Rcvr_1070
>       s = defaultSession { grade = GradeA }
>       -- just L band
>       s1 = s { receivers = [[Rcvr1_2]] }
>       -- L or S
>       s2 = s { receivers = [[Rcvr1_2, Rcvr2_3]] }
>       -- L and S
>       s3 = s { receivers = [[Rcvr1_2], [Rcvr2_3]] }
>       -- L or (S and C)
>       s4 = s { receivers = [[Rcvr1_2,Rcvr4_6], [Rcvr1_2,Rcvr2_3]] }
>       -- now start including the boosted rcvr
>       s5 = s { receivers = [[boost]] }
>       -- L or boost 
>       s6 = s { receivers = [[Rcvr1_2, boost]] }
>       -- L and boost
>       s7 = s { receivers = [[Rcvr1_2], [boost]] }
>       -- boost or (S and C)
>       s8 = s { receivers = [[boost,Rcvr4_6], [boost,Rcvr2_3]] }
>       -- L or (boost and C)
>       s9 = s { receivers = [[Rcvr1_2,boost], [Rcvr1_2,Rcvr2_3]] }
>       -- Grade B's don't get the boost
>       s10 = defaultSession { receivers = [[boost]], grade = GradeB }

> test_receiverBoost2 = TestCase $ do
>   assertEqual "test_receiverBoost2_1"  False (receiverBoost' s1)
>   assertEqual "test_receiverBoost2_2"  False (receiverBoost' s2)
>   assertEqual "test_receiverBoost2_3"  False (receiverBoost' s3)
>   assertEqual "test_receiverBoost2_4"  True  (receiverBoost' s4)
>   assertEqual "test_receiverBoost2_5"  True  (receiverBoost' s5)
>   assertEqual "test_receiverBoost2_6"  False (receiverBoost' s6)
>     where
>       b1 = Rcvr_1070
>       b2 = Rcvr_450
>       s = defaultSession { grade = GradeA }
>       -- just L band
>       s1 = s { receivers = [[Rcvr1_2]] }
>       -- L or S
>       s2 = s { receivers = [[Rcvr1_2, Rcvr2_3]] }
>       -- L or boost
>       s3 = s { receivers = [[Rcvr1_2, b1]] }
>       -- boost 1 or 2
>       s4 = s { receivers = [[b1, b2]] }
>       -- boost 1 and 2
>       s5 = s { receivers = [[b1], [b2]] }
>       -- L or (boost 1 and 2)
>       s6 = s { receivers = [[Rcvr1_2, b1], [Rcvr1_2, b2]] }

> test_observerOnSite = TestCase $ do
>   assertEqual "test_observerOnSite_1" True  (obsOnSite dt  s1)
>   assertEqual "test_observerOnSite_2" False (obsOnSite dt2 s1)
>   assertEqual "test_observerOnSite_3" False (obsOnSite dt3 s1)
>   assertEqual "test_observerOnSite_4" True  (obsOnSite dt  s2)
>   assertEqual "test_observerOnSite_5" False (obsOnSite dt2 s2)
>   assertEqual "test_observerOnSite_6" True  (obsOnSite dt3 s2)
>     where
>       dt  = fromGregorian 2006 2 1  0 0 0
>       dt2 = fromGregorian 2006 2 7  0 0 0
>       dt3 = fromGregorian 2006 2 11 0 0 0
>       rs  = [(fromGregorian 2006 1 31 0 0 0, fromGregorian 2006 2 2 0 0 0)]
>       o   = defaultObserver
>       o2  = defaultObserver { reservations = rs }
>       pr1 = defaultProject { observers = [o,o2] }
>       s1  = defaultSession { project = pr1 }
>       rs2 = [(fromGregorian 2006 2 10 0 0 0, fromGregorian 2006 2 12 0 0 0)]
>       o3  = defaultObserver { reservations = rs2 }
>       pr2 = defaultProject { observers = [o2, o3] }
>       s2  = defaultSession { project = pr2 }

Test utilities

> assertAlmostEqual :: String -> Int -> Float -> Float -> IO ()
> assertAlmostEqual name places expected value =
>     assertBool name $ abs (value - expected) < epsilon
>   where
>     epsilon = 1.0 / 10.0 ** fromIntegral places

> assertScoringResult :: String -> Maybe DateTime -> Int -> Float -> Scoring Factors -> IO ()
> assertScoringResult name dt  digits expected scoref = do
>     w <- getTestWeather dt  
>     [(_, Just result)] <- runScoring w rSched scoref
>     assertAlmostEqual name digits expected result

> assertScoringResult' :: String -> Maybe DateTime -> Float -> Scoring Factors -> IO ()
> assertScoringResult' name dt expected scoref = do
>     w <- getTestWeather dt 
>     [(_, Just result)] <- runScoring w rSched scoref
>     assertEqual name expected result

> assertResult :: String -> Maybe DateTime -> Int -> Float -> Scoring (Maybe Float) -> IO ()
> assertResult name dt digits expected scoref = do
>     w <- getTestWeather dt
>     Just result <- runScoring w rSched scoref
>     assertAlmostEqual name digits expected result

> assertResult' :: String -> Maybe DateTime -> Float -> Scoring (Maybe Float) -> IO ()
> assertResult' name dt expected scoref = do
>     w <- getTestWeather dt
>     Just result <- runScoring w rSched scoref
>     assertEqual name expected result


> getTestWeather :: Maybe DateTime -> IO Weather
> getTestWeather dt | isJust dt == False = getWeather . Just $ fromGregorian 2006 10 13 22 0 0
>                   | isJust dt = getWeather dt

Test data generation

These are sessions that exposed bugs from the QuickCheck properties.

> bugSessions = zipWith5 genBugSessions names ras decs freqs rcvrs 
>   where names  = ["bug1",   "bug2"]
>         ras    = [ 2.67,  0.873562]
>         decs   = [ 0.13, -0.108025]
>         freqs  = [39.76,       2.0]
>         --rcvrs  = [[Rcvr26_40],[Rcvr1_2]]
>         rcvrs  = [[[Rcvr26_40]],[[Rcvr1_2]]]
>         genBugSessions n r d f rcvr = defaultSession {
>             sName = n, ra = r, dec = d, frequency = f, receivers = rcvr
>         }

> sessBug = bugSessions!!0
> sessBug2 = bugSessions!!1

> dtLP = fromGregorian 2006 10 15 12 0 0

> pSessions = zipWith5 genPSess tots useds ras bands grades
>   where tots   = [12*60, 18*60, 10*60, 20*60]
>         useds  = [ 2*60,  8*60,  5*60, 12*60]
>         ras    = [  5.4,  10.1,   4.9,  18.1]
>         bands  = [    L,     C,     X,     L]
>         grades = [GradeA, GradeA, GradeA, GradeA]
>         genPSess t u ra b g = defaultSession {
>             sAlloted = t
>           , periods = [defaultPeriod {duration = u}]
>           , ra = hrs2rad ra
>           , band = b
>           , grade = g
>         }

> rSessions = zipWith5 genPSess tots useds ras bands grades
>   where tots   = [12*60, 18*60, 10*60, 20*60]
>         useds  = [ 2*60,  8*60,  5*60, 12*60]
>         ras    = [  5.4,  10.1,   4.9,  18.1]
>         bands  = [    L,     C,     X,     L]
>         grades = [GradeA, GradeA, GradeB, GradeB]
>         genPSess t u ra b g = defaultSession {
>             sAlloted = t
>           , periods = [defaultPeriod {duration = u}]
>           , ra = hrs2rad ra
>           , band = b
>           , grade = g
>         }

> rSched = [ (fromGregorian 2006 6 14 12 0 0, [Rcvr1_2, Rcvr26_40])
>          , (fromGregorian 2006 6 21 12 0 0, [Rcvr1_2, Rcvr12_18])
>          , (fromGregorian 2006 6 24 16 0 0, [Rcvr4_6, Rcvr12_18])
>          , (fromGregorian 2006 7  1 12 0 0, [Rcvr1_2, Rcvr4_6])
>          ]
