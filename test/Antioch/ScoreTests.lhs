> module Antioch.ScoreTests where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.ReceiverTemperatures
> import Control.Monad.Trans  (lift, liftIO)
> import Test.HUnit
> import Data.List            (zip4, zipWith4, zipWith5)
> import Data.Maybe           (isJust, fromJust)
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
>   , test_initBins1
>   , test_initBins2
>   , test_receiver
>   , test_getReceivers
>   , test_zenithAngle
>   , test_zenithAngle2
>   , test_zenithAngleAtTransit
>   , test_minTsysPrime
>   , test_minTsysPrime
>   , test_systemNoiseTemperature
>   , test_minTsys'
>   , test_minimumObservingConditions
>   , test_observingEfficiency
>   , test_observingEfficiency2
>   , test_observingEfficiencyLimit
>   , test_minObservingEfficiencyFactor
>   , test_efficiency
>   , test_efficiency_below2GHz
>   , test_zenithOpticalDepth
>   , test_zenithOpticalDepth2
>   , test_positionValues
>   --, test_receiverTemperature
>   , test_minObservingEff
>   , test_kineticTemperature
>   , test_kineticTemperature2
>   , test_stringency
>   , test_scienceGrade
>   , test_projectCompletion
>   , test_politicalFactors
>   , test_trackingEfficiency
>   , test_trackingErrorLimit
>   , test_positionFactors
>   , test_subfactorFactors
>   , test_weatherFactors
>   , test_scoreFactors
>   , test_inWindows
>   , test_scoreElements
>   , test_zenithAngleLimit
>   , test_surfaceObservingEfficiency
>   , test_scoreCV
>   , test_scoreCV2
>   , test_avgScoreForTime
>   , test_avgScoreForTime2
>   , test_weightedMeanScore
>   , test_score
>   , test_score_window
>   , test_bestDuration
>   , test_bestDurations
>   , test_averageScore
>   , test_averageScore2
>   , test_obsAvailable
>   , test_obsAvailable2
>   , test_obsAvailable3
>   , test_observerAvailable
>   , test_needsLowRFI
>   , test_lstExcepted
>   , test_enoughTimeBetween
>   , test_receiverBoost
>   , test_receiverBoost2
>   , test_observerOnSite
>   , test_scorePeriod
>   --, test_mustang -- TBF: we took out the MUSTANG Hack
>   , test_elevationLimit
>   , test_atmosphericStability
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
>     rt <- getRT
>     scores <- mapM (score' w rt) times
>     assertEqual "test_hourAngleLimit" expected scores
>   where
>     sess = findPSessionByName "LP"
>     score' w rt dt = do
>         [(_, Just s)] <- runScoring w [] rt (hourAngleLimit dt sess)
>         return s
>     times = [(60*h) `addMinutes'` dtLP | h <- [0..23]]
>     expected = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0,
>                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

> test_frequencyPressure = TestCase $ do
>     freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure defaultStartTime pSessions
>     assertScoringResult "test_frequencyPressure" Nothing 5 2.1132288 (freqPressure undefined . head $ pSessions)

Test that a frequency NOT in the initial bins gives a pressure of 1.0

> test_frequencyPressureComparison = TestCase $ do
>     freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure defaultStartTime pSessions
>     assertScoringResult' "test_frequencyPressure comparison" Nothing 1.0 (freqPressure undefined . head $ ss)
>   where
>     ss = concatMap sessions pTestProjects

> test_rightAscensionPressure = TestCase $ do
>     raPressure <- runScoring undefined [] undefined $ genRightAscensionPressure defaultStartTime pSessions
>     assertScoringResult "test_rightAscensionPressure" Nothing 5 1.5259848 (raPressure undefined . head $ pSessions)

> test_initBins1 = TestCase $ do
>     assertEqual "test_initBins1" expected result
>   where
>     accessor s = (round . rad2hrs . ra $ s) `mod` 24
>     expected  = [(0,0),(0,0),(0,0),(0,0),(0,0),(1320,420)
>                 ,(0,0),(0,0),(0,0),(0,0),(1080,480),(0,0)
>                 ,(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)
>                 ,(1200,720),(0,0),(0,0),(0,0),(0,0),(0,0)]
>     result    = elems $ initBins startTime (0, 23) accessor pSessions
>     startTime = fromGregorian' 2008 1 15

> test_initBins2 = TestCase $ do
>     assertEqual "test_initBins2" expected result
>   where
>     expected  = [(1920,840),(0,0),(1080,480)
>                 ,(600,300),(0,0),(0,0)
>                 ,(0,0),(0,0),(0,0)]
>     result    = elems $ initBins startTime (minBound, maxBound) band pSessions
>     startTime = fromGregorian' 2008 1 15

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
>    Just result <- minTSysPrime w (frequency sess) (elevation dt sess)
>    assertAlmostEqual "test_minTsysPrime 1" 3 15.490067 result 
>    -- session AS
>    let sess = findPSessionByName "AS"
>    Just result <- minTSysPrime w (frequency sess) (elevation dt sess)
>    assertAlmostEqual "test_minTsysPrime 2" 3 25.958 result 
>    -- sessBug
>    Just result <- minTSysPrime w (frequency sessBug) (elevation dt sessBug)
>    assertAlmostEqual "test_minTsysPrime 3" 3 92.365046 result 
>    -- sessBug2
>    Just result <- minTSysPrime w (frequency sessBug2) (elevation dt sessBug2)
>    assertAlmostEqual "test_minTsysPrime 4" 4 29.858517 result 
>      where 
>        dt = fromGregorian 2006 10 15 12 0 0

> test_systemNoiseTemperature = TestCase $ do
>    w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 2
>    rt <- getRT
>    let dt = fromGregorian 2006 10 15 12 0 0
>    -- session LP
>    let sess = findPSessionByName "LP"
>    Just result <- systemNoiseTemperature w rt dt sess
>    assertEqual "test_systemNoiseTemperature 1" 15.348079 result 
>    Just result <- systemNoiseTemperature' w rt dt sess
>    assertEqual "test_systemNoiseTemperature' 1" 15.630218 result 
>    -- session AS
>    let sess = findPSessionByName "AS"
>    Just result <- systemNoiseTemperature w rt dt sess
>    assertEqual "test_systemNoiseTemperature 2" 25.468143 result 
>    Just result <- systemNoiseTemperature' w rt dt sess
>    assertEqual "test_systemNoiseTemperature' 2" 26.474463 result 

> test_minTsys' = TestCase $ do
>    w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 2
>    let dt = fromGregorian 2006 10 15 12 0 0
>    -- session LP
>    let sess = findPSessionByName "LP"
>    Just result <- minTsys' w dt sess
>    assertAlmostEqual "test_minTsys' 1" 3 15.490067 result 
>    -- session AS
>    let sess = findPSessionByName "AS"
>    Just result <- minTsys' w dt sess
>    assertAlmostEqual "test_minTsys' 2" 3 25.958 result 
>    -- sessBug
>    Just result <- minTsys' w dt sessBug
>    assertAlmostEqual "test_minTsys' 3" 3 92.365046 result 
>    -- sessBug2
>    Just result <- minTsys' w dt sessBug2
>    assertAlmostEqual "test_minTsys' 4" 4 29.858517 result 

> test_minimumObservingConditions = TestCase $ do
>    let dt = fromGregorian 2006 10 13 16 0 0
>    w <- getWeather . Just $ dt
>    rt <- getRT
>    mocs <- mapM (moc w rt dt) sess
>    assertEqual "test_minimumObservingConditions" expected mocs
>   where
>     moc w rt dt s = do
>         Just result <- runScoring w [] rt (minimumObservingConditions dt s)
>         return result
>     names = ["GB","CV","LP","TX","VA","WV","AS"]
>     sess = concatMap (\name -> findPSessionsByName name) names
>     expected = [False, True, True, False, False, False, True]

> test_observingEfficiency = TestCase $ do
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getRT
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     fs <- runScoring w [] rt (observingEfficiency dt s)
>     let result = eval fs
>     assertAlmostEqual "test_observingEfficiency" 4 0.8577623 result

> test_minObservingEfficiencyFactor = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 8 0 0
>     rt <- getRT
>     fs <- runScoring w [] rt (observingEfficiency dt s1)
>     assertEqual "test_minObservingEfficiencyFactor 1" 0.48535314 (eval fs)
>     fs <- runScoring w [] rt (atmosphericOpacity dt s1)
>     assertEqual "test_minObservingEfficiencyFactor 2" 0.5247221 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiencyLimit dt s1)
>     assertEqual "test_minObservingEfficiencyFactor 3" 3.173581e-13 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt s2)
>     assertEqual "test_minObservingEfficiencyFactor 4" 0.48535314  (eval fs)
>     fs <- runScoring w [] rt (atmosphericOpacity dt s2)
>     assertEqual "test_minObservingEfficiencyFactor 5" 0.5247221 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiencyLimit dt s2)
>     assertEqual "test_minObservingEfficiencyFactor 6" 3.173581e-13 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt s3)
>     assertEqual "test_minObservingEfficiencyFactor 7" 0.7583644 (eval fs)
>     fs <- runScoring w [] rt (atmosphericOpacity dt s3)
>     assertEqual "test_minObservingEfficiencyFactor 8" 0.81987834 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiencyLimit dt s3)
>     assertEqual "test_minObservingEfficiencyFactor 9" 1.0 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt s4)
>     assertEqual "test_minObservingEfficiencyFactor 10" 0.9249718 (eval fs)
>     fs <- runScoring w [] rt (atmosphericOpacity dt s4)
>     assertEqual "test_minObservingEfficiencyFactor 11" 1.0 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiencyLimit dt s4)
>     assertEqual "test_minObservingEfficiencyFactor 12" 1.0 (eval fs)
>     where
>      dt = fromGregorian 2006 10 15 12 0 0
>      s1 = defaultSession {sAllottedT = 24*60, minDuration = 2*60
>                         , maxDuration = 6*60, frequency = 16.9
>                         , dec = 0.71, band = K}
>      s2 = s1 {xi = 1.0}
>      s3 = s1 {xi = 1.25}
>      s4 = s1 {xi = 2.0}

BETA: TestObservingEfficiency.py test_efficiency

> test_observingEfficiency2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 8 0 0
>     rt <- getRT
>     let dt1 = fromGregorian 2006 10 15 12 0 0 -- sunup  
>     let dt2 = fromGregorian 2006 10 15 11 0 0 -- sundown
>     let sLP = findPSessionByName "LP" 
>     let sGB = findPSessionByName "GB" 
>     fs <- runScoring w [] rt (observingEfficiency dt1 sLP)
>     assertAlmostEqual "test_observingEfficiency2" 4 0.97434574 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt2 sLP)
>     -- BETA: difference due to Float vs. Double
>     assertAlmostEqual "test_observingEfficiency2_2" 2 0.97567 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt1 sGB)
>     assertAlmostEqual "test_observingEfficiency2_3" 2 0.71677315 (eval fs)

BETA: TestObservingEfficiencyLimit.testHaskell

> test_observingEfficiencyLimit = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getRT
>     let dt = fromGregorian 2006 9 2 14 30 0
>     -- BETA: differences probably due to Float vs. Double
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     [(_, Just result)] <- runScoring w [] rt (observingEfficiencyLimit dt s)
>     assertEqual "test_observingEfficiencyLimit <18" 3.0780464e-4 result
>     let s = findPSessionByName "GB"
>     [(_, Just result)] <- runScoring w [] rt (observingEfficiencyLimit dt s)
>     assertEqual "test_observingEfficiencyLimit >=18" 1.6728761e-4 result

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
>     assertResult "test_efficiency 5" (Just wdt) 2 0.997521 (efficiency dt sess) 
>     assertResult "test_efficiencyHA 6" (Just wdt) 2 0.94227356 (efficiencyHA dt sess)
>     assertResult "test_efficiency 7" (Just wdt) 2 0.935551 (efficiency dt sessBug)
>     assertResult "test_efficiency 8" (Just wdt) 4 0.95340 (efficiency dt sessBug2) 
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getRT
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     Just result <- runScoring w [] rt (efficiency dt s) 
>     assertEqual "test_efficiency 9" 0.87132007 result
>     Just result <- runScoring w [] rt (efficiencyHA dt s) 
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
>     rt <- getRT
>     let dt1 = fromGregorian 2006 10 15 11 0 0
>     let sLP = findPSessionByName "LP" 
>     Just zod <- runScoring w [] rt (zenithOpticalDepth dt1 sLP)
>     assertEqual "test_zenithOpticalDepth2" 0.007960711 zod 

> test_positionValues = TestCase $ do
>     assertEqual "test_positionValues hourAngle" 1.0507135 (hourAngle dt lp)
>     assertEqual "test_epositionValues elevation" 0.46234667 (elevation dt lp)
>   where
>     dt = fromGregorian 2009 12 9 16 24 0
>     ss = concatMap sessions pTestProjects
>     lp = head $ filter (\s -> "LP" == (sName s)) ss

> {-
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
> -}

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
>     rt <- getRT
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     Just result <- runScoring w [] rt (kineticTemperature dt s) 
>     assertEqual "test_kineticTemperatureCV" 271.3523 result

BETA: TestAtmosphericOpacity.py testHaskell

> test_kineticTemperature2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 8 0 0
>     rt <- getRT
>     let dt1 = fromGregorian 2006 10 15 11 0 0
>     let sLP = findPSessionByName "LP" 
>     Just kt <- runScoring w [] rt (kineticTemperature dt1 sLP)
>     assertEqual "test_kineticTemperature2" 257.41776 kt 

BETA: TestStringency.py testScore (first assert)

> test_stringency = TestCase $ do
>     let dt = fromGregorian 2006 10 15 18 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult "test_stringency" Nothing 5 1.40086 (stringency dt sess)
>     let sess = findPSessionByName "AS"
>     assertScoringResult "test_stringency" Nothing 5 1.03437 (stringency dt sess)

> makeTestProject :: Minutes -> Minutes -> Project
> makeTestProject tl tt = makeProject proj' tt tt ss'
>   where
>     proj' = defaultProject { pName = "time use test" }
>     ss''  = [
>         defaultSession {
>             periods = [defaultPeriod {duration = tt - tl, pDuration = tt - tl}]
>           , sAllottedT = tt
>           }
>       ]
>     ss'   = [ makeSession s [] (periods s) | s <- ss'' ]

> test_scienceGrade = TestCase $ do
>     let dt = fromGregorian 2006 10 15 18 0 0 -- don't need!
>     let p1 = makeTestProject' dt 501 1000 500
>     let s1 = head . sessions $ p1
>     assertScoringResult' "test_scienceGrade 1" Nothing 1.0 (scienceGrade dt s1)
>     let p2 = makeTestProject' dt 500 1000 500
>     let s2 = head . sessions $ p2
>     assertScoringResult' "test_scienceGrade 2" Nothing 0.51 (scienceGrade dt s2)

> makeTestProject' dt tl tt ts = makeProject proj' tt tt ss'
>   where
>     proj' = defaultProject { pName = "time use test" }
>     ss''  = [
>         defaultSession {
>             periods = [defaultPeriod {startTime = dt
>                                     , duration = tt - tl
>                                     , pDuration = tt - tl
>                                      }
>                       ]
>           , sAllottedT = tt
>           , sAllottedS = ts
>           }
>       ]
>     ss'   = [ makeSession s [] (periods s) | s <- ss'' ]


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
>     rt <- getRT
>     let dt = fromGregorian 2006 10 15 12 0 0
>     let s = head . filter (\s -> "CV" == (sName s)) . concatMap sessions $ pTestProjects
>     -- missing window, transit, observerOnSite, and ObserverAvailable
>     let politicalFactors = score [scienceGrade
>                                 , thesisProject
>                                 , projectCompletion]
>     fs <- runScoring w [] rt (politicalFactors dt s)
>     let result = eval fs
>     assertEqual "test_politicalFactors" 1.0052 result

BETA: TestTrackingEfficiency.py testefficiencyHaskell

> test_trackingEfficiency = TestCase $ do
>     -- session LP
>     let sess = findPSessionByName "LP"
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertScoringResult "test_trackingEfficiency lp" Nothing 4 0.9976445 (trackingEfficiency dt sess)
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getRT
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     [(_, Just result)] <- runScoring w [] rt (trackingEfficiency dt s)
>     assertAlmostEqual "test_trackingEfficiency cv" 3 0.9879579 result 

BETA: TestTrackingErrorLimit.py testHaskell testcomputedScore

> test_trackingErrorLimit = TestCase $ do
>     let dt = fromGregorian 2006 10 15 12 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult' "test_trackingErrorLimit" Nothing 1.0 (trackingErrorLimit dt sess)
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getRT
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     [(_, Just result)] <- runScoring w [] rt (trackingErrorLimit dt s)
>     assertEqual "test_trackingErrorLimit" 1.0 result

> test_positionFactors = TestCase $ do
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     factors <- positionFactors s dt
>     let hourAngle = fromJust . fromJust . lookup "hourAngle" $ factors
>     assertEqual "test_positionFactors hourAngle" (-4.349304) hourAngle
>     let elevation = fromJust . fromJust . lookup "elevation" $ factors
>     assertEqual "test_positionFactors elevation" 36.60029 elevation

> test_subfactorFactors = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     factors <- subfactorFactors s w dt
>     let sysNoiseTemp = fromJust . fromJust . lookup "sysNoiseTemp" $ factors
>     assertEqual "test_subfactorFactors sysNoiseTemp" 14.862213 sysNoiseTemp
>     let sysNoiseTempPrime = fromJust . fromJust . lookup "sysNoiseTempPrime" $ factors
>     assertEqual "test_subfactorFactors sysNoiseTempPrime" 15.093734 sysNoiseTempPrime
>     let minSysNoiseTempPrime = fromJust . fromJust . lookup "minSysNoiseTempPrime" $ factors
>     assertEqual "test_subfactorFactors minSysNoiseTempPrime" 14.089174 minSysNoiseTempPrime

> test_weatherFactors = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     factors <- weatherFactors s w dt
>     {-  TBF mph not in 2006
>     let wind_mph = fromJust . fromJust . lookup "wind_mph" $ factors
>     assertEqual "test_weatherFactors wind_mph" 1.0 wind_mph
>     -}
>     let wind_ms = fromJust . fromJust . lookup "wind_ms" $ factors
>     assertEqual "test_weatherFactors wind_ms" 5.6930013 wind_ms
>     let opacity = fromJust . fromJust . lookup "opacity" $ factors
>     assertEqual "test_weatherFactors opacity" 9.302652e-3 opacity
>     let tsys = fromJust . fromJust . lookup "tsys" $ factors
>     assertEqual "test_weatherFactors tsys" 271.3523 tsys

> test_scoreFactors = TestCase $ do
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     let dur = 15::Minutes
>     w <- getWeather . Just $ fromGregorian 2006 9 2 14 30 0 -- pick earlier
>     factors <- scoreFactors s w pSessions dt dur []
>     assertEqual "test_scoreFactors 1" 20 (length . head $ factors)
>     let haLimit = fromJust . fromJust . lookup "hourAngleLimit" . head $ factors
>     assertEqual "test_scoreFactors 2" 1.0 haLimit
>     let fPress = fromJust . fromJust . lookup "frequencyPressure" . head $ factors
>     assertEqual "test_scoreFactors 3" 1.9724026 fPress

> test_inWindows = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 9 20 1 0 0
>     rt <- getRT
>     -- test sessions with two windows,
>     --     first not satisfied, i.e., no chosen period
>     --     second is satisfied, i.e., has chosen period
>     let s = findPSessionByName "TestWindowed2"
>     -- dt is outside both windows
>     let dt = fromGregorian 2006 9 21 23 45 0
>     [(_, Just result)] <- runScoring w [] rt (inAvailWindows dt s)
>     assertEqual "test_inWindows 1" 0.0 result 
>     [(_, Just result)] <- runScoring w [] rt (inAnyWindows dt s)
>     assertEqual "test_inWindows 2" 0.0 result 
>     -- dt is just inside first window
>     let dt = fromGregorian 2006 9 22 0 0 0
>     [(_, Just result)] <- runScoring w [] rt (inAvailWindows dt s)
>     assertEqual "test_inWindows 3" 1.0 result 
>     [(_, Just result)] <- runScoring w [] rt (inAnyWindows dt s)
>     assertEqual "test_inWindows 4" 1.0 result 
>     -- dt is in second window, but it is satisfied
>     let dt = fromGregorian 2006 10 19 0 0 0
>     [(_, Just result)] <- runScoring w [] rt (inAvailWindows dt s)
>     assertEqual "test_inWindows 5" 0.0 result 
>     [(_, Just result)] <- runScoring w [] rt (inAnyWindows dt s)
>     assertEqual "test_inWindows 6" 1.0 result 

> test_scoreElements = TestCase $ do
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     let dur = 15::Minutes
>     w <- getWeather . Just $ fromGregorian 2006 9 2 14 30 0 -- pick earlier
>     factors <- scoreElements s w pSessions dt dur []
>     assertEqual "test_scoreElements 1" 29 (length . head $ factors)
>     let haLimit = fromJust . fromJust . lookup "hourAngleLimit" . head $ factors
>     assertEqual "test_scoreElements 2" 1.0 haLimit
>     let fPress = fromJust . fromJust . lookup "frequencyPressure" . head $ factors
>     assertEqual "test_scoreElements 3" 1.9724026 fPress
>     let opacity = fromJust . fromJust . lookup "opacity" . head $ factors
>     assertEqual "test_scoreElements 4" 7.844159e-3 opacity
>     let elevation = fromJust . fromJust . lookup "elevation" . head $ factors
>     assertEqual "test_scoreElements 5" 36.60029 elevation
>     let sysNoiseTemp = fromJust . fromJust . lookup "sysNoiseTemp" . head $ factors
>     assertEqual "test_scoreElements 6" 14.164635 sysNoiseTemp

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
>     assertScoringResult "test_surfaceObservingEfficienyLP" wdt 5 0.9943902 (surfaceObservingEfficiency dt sess)
>     let sess = findPSessionByName "WV"
>     assertScoringResult "test_surfaceObservingEfficienyWV" wdt 5 0.7905864 (surfaceObservingEfficiency dt sess)

> test_scoreCV = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getRT
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] rt $ genScore dt ss >>= \f -> f dt s
>     let result = eval fs
>     assertAlmostEqual "test_scoreCV" 5 1.2679951e-3 result  

New tests that do *not* match up to a 'beta test python code test', but rather
to use in conjunction with Pack tests.

> test_scoreCV2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 1 18 0 0
>     rt <- getRT
>     -- make sure that we don't use real wind!
>     let dt = fromGregorian 2006 10 1 18 1 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] rt $ genScore dt ss >>= \f -> f dt s
>     let result = eval fs
>     assertAlmostEqual "test_scoreCV2" 3 4.6854753 result  

> test_scoreForTime = TestCase $ do
>     -- score on top of weather
>     w <- getWeather $ Just dt
>     rt <- getRT
>     fs <- runScoring w [] rt $ do
>         sf <- genScore dt ss
>         sf dt s
>     let w1Score = eval fs
>     -- use different forecast; should get different score
>     w <- getWeather $ Just dt2
>     fs <- runScoring w [] rt $ do
>         sf <- genScore dt2 ss
>         sf dt s
>     let w2Score = eval fs
>     assert (w1Score /= w2Score) 
>     -- now try to get the original score again, despite current weather obj
>     w3Score <- runScoring w [] rt $ do
>         sf <- genScore dt ss
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
>     rt <- getRT
>     w <- getWeather $ Just dt
>     fs <- runScoring w [] rt $ do
>         sf <- genScore dt ss
>         sf dt s
>     let w1Score = eval fs
>     -- use different forecast; should get different score
>     w <- getWeather $ Just dt2
>     fs <- runScoring w [] rt $ do
>         sf <- genScore dt ss
>         sf dt s
>     let w2Score = eval fs
>     assert (w1Score /= w2Score) 
>     -- now try to get the original score again, despite current weather obj
>     w3Score <- runScoring w [] rt $ do
>         sf <- genScore dt ss
>         avgScoreForTimeRealWind sf dt 15 s
>     -- since we're using real (measured) wind, the scores should be the same
>     assertAlmostEqual "test_avgScoreForTime_2" 4 w1Score w3Score
>   where
>     dt = fromGregorian 2006 10 1 18 0 0
>     dt2 = fromGregorian 2006 10 1 0 0 0
>     ss = getOpenPSessions
>     s = findPSessionByName "CV"
> 

> test_avgScoreForTime2 = TestCase $ do
>     -- weather that shouldn't get used
>     rt <- getRT
>     w <- getWeather $ Just dummytime
>     -- score over a wide range of time, that includes zeros, and see
>     -- how it zeros out the scores.
>     avgScore <- runScoring w [] rt $ do
>         sf <- genScore starttime [s]
>         avgScoreForTimeRealWind sf starttime (24*60) s
>     -- now limit the time window to an area w/ non-zero scores
>     avgScore2 <- runScoring w [] rt $ do
>         sf <- genScore starttime2 [s]
>         avgScoreForTimeRealWind sf starttime2 (4*60) s
>     assertEqual "test_avgScoreForTime2_1" 0.0 avgScore
>     assertEqual "test_avgScoreForTime2_2" True (avgScore2 /= 0.0)
>   where
>     dummytime  = fromGregorian 2006 11 7 12 0 0
>     starttime  = fromGregorian 2006 11 8 12 0 0
>     starttime2 = fromGregorian 2006 11 8 22 0 0
>     s = defaultSession {sAllottedT = 24*60, minDuration=2*60, maxDuration=6*60}
>     -- scoring using a weather from starttime gives these scores for this
>     -- session in 24 hours
>     --expScores = (replicate 39 0.0) ++ defaultScores ++ (replicate 22 0.0) 
> 

> test_weightedMeanScore = TestCase $ do
>     assertEqual "test_weightedMeanScore 0" 0.0 (weightedMeanScore [])
>     assertEqual "test_weightedMeanScore 1" 0.0 (weightedMeanScore [17.0])
>     assertEqual "test_weightedMeanScore 2" 6.5 (weightedMeanScore [17.0, 13.0])
>     assertEqual "test_weightedMeanScore 3" 8.0 (weightedMeanScore [17.0, 13.0, 11.0])
>     assertEqual "test_weightedMeanScore 4" 7.75 (weightedMeanScore [17.0, 13.0, 11.0, 7.0])

Test the 24-hour scoring profile of the default session, per quarter.

> test_score = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     let score' w rt dt = runScoring w [] rt $ do
>         fs <- genScore dt [sess]
>         s <- fs dt sess
>         return $ eval s
>     scores <- mapM (score' w rt) times
>     assertEqual "test_score" expected scores
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     score' w rt dt = runScoring w [] rt $ do
>         fs <- genScore dt [sess]
>         s  <- fs dt sess
>         return $ eval s
>     times = [(15*q) `addMinutes'` starttime | q <- [0..96]]
>     sess = defaultSession { sName = "singleton"
>                           , sAllottedT = 24*60
>                           , minDuration = 2*60
>                           , maxDuration = 6*60
>                           , frequency = 2.0
>                           }
>     expected = (replicate 39 0.0) ++ defaultScores ++ (replicate 22 0.0)

> test_score_window = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     scores <- mapM (score' w rt) times
>     assertEqual "test_score_window" expected scores
>   where
>     starttime = fromGregorian 2006 9 27 9 45 0
>     score' w rt dt = runScoring w [] rt $ do
>         fs <- genScore dt [sess]
>         s  <- fs dt sess
>         return $ eval s
>     times = [(15*q) `addMinutes'` starttime | q <- [0..96]]
>     sess = findPSessionByName "TestWindowed2"
>     expected = take 97 $ [2.327,2.3243728,2.321247,2.3177772,2.3132033] ++ (repeat 0.0)

For defaultSession w/ sAllottedT = 24*60; start time is  2006 11 8 12 0 0
plus 40 quarters.

> --defaultScores = [0.5217929,0.5231148,0.5238463,0.4625782,0.49542525,0.5059413,0.51017797,0.5179315,0.5188393,0.51937324,0.5198584,0.5274695,0.52757597,0.5277754,0.5278681,0.5323486,0.5323486,0.53238064,0.53238064,0.532714,0.532714,0.5326842,0.53262156,0.5328553,0.5327895,0.5327549,0.5326802,0.5322283,0.53211904,0.5319976,0.53178895,0.5318738,0.5316135,0.5312978,0.5309066,0.53060484]

> defaultScores = [0.5209438,0.5222635,0.5229939,0.47278547,0.5001854,0.5072396,0.50930166,0.5170368,0.517943,0.518476,0.51896036,0.52645373,0.52656007,0.5267591,0.5273235,0.53213924,0.53213924,0.5321713,0.5321713,0.5325156,0.5325156,0.5324858,0.53242314,0.532653,0.5325872,0.5325526,0.5324779,0.5320406,0.53193134,0.53181005,0.5316014,0.5316953,0.5314352,0.5311195,0.53072846,0.5304281]

> test_bestDuration = TestCase $ do
>     w <- getWeather . Just $ origin 
>     rt <- getRT
>     -- best period length using session's min/max
>     bestDur <- runScoring w [] rt $ do
>         sf <- genScore starttime ss
>         bestDuration sf starttime Nothing Nothing s
>     let expected = (s, 4.3957114, 4*60 + 15)
>     assertEqual "test_bestDuration 1" expected bestDur
>     -- best period length overriding min/max
>     bestDur <- runScoring w [] rt $ do
>         sf <- genScore starttime ss
>         bestDuration sf starttime (Just 0) (Just (4*60::Minutes)) s
>     let expected = (s, 4.3792863, 4*60)
>     assertEqual "test_bestDuration 2" expected bestDur
>     -- best period length using session's min/max, but only 4 hours left
>     bestDur <- runScoring w [] rt $ do
>         sf <- genScore starttime ss
>         bestDuration sf starttime Nothing Nothing exht
>     let expected = (exht, 4.3792863, 4*60)
>     assertEqual "test_bestDuration 3" expected bestDur
>   where
>     origin = fromGregorian 2006 10 1 18 0 0
>     starttime = fromGregorian 2006 10 1 18 0 0
>     -- a nearly exhausted session, i.e., only 4 hours left
>     ss = concatMap sessions pTestProjects
>     s = head $ filter (\s -> "CV" == (sName s)) ss
>     exht = s { sAllottedT   = 8*60 
>               , minDuration = 2*60 
>               , maxDuration = 6*60
>               , periods = [
>                  defaultPeriod { pState = Scheduled
>                                , duration = 4*60
>                                , pDuration = 4*60
>                                }
>                ]
>               }

> test_bestDurations = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     let ss = concatMap sessions pTestProjects
>     bestDurs <- runScoring w [] rt $ do
>         sf <- genScore starttime ss
>         bestDurations sf starttime Nothing Nothing ss
>     assertEqual "test_bestDurations 1" 12 (length bestDurs)
>     let (s, v, d) = bestDurs !! 1
>     assertEqual "test_bestDurations 2 n" "CV" (sName s)
>     assertAlmostEqual "test_bestDurations 2 v" 5 4.3957114 v
>     assertEqual "test_bestDurations 2 d" 255 d
>     let (s, v, d) = bestDurs !! 6
>     assertEqual "test_bestDurations 3 n" "AS" (sName s)
>     assertAlmostEqual "test_bestDurations 3 v" 5 3.4960268 v
>     assertEqual "test_bestDurations 3 d" 450 d
>   where
>     starttime = fromGregorian 2006 10 1 18 0 0

> test_averageScore = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     let score' w dt = runScoring w [] rt $ do
>         fs <- genScore dt [sess]
>         s <- fs dt sess
>         return $ eval s
>     scores <- mapM (score' w) times
>     let scoreTotal = addScores scores
>     let expected = 0.0
>     assertEqual "test_score1" expected scoreTotal
>     avgScore <- runScoring w [] rt $ do
>         fs <- genScore starttime [sess]
>         averageScore fs starttime sess
>     assertEqual "test_score2" expected avgScore
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     sess = defaultSession { sAllottedT = 24*60 
>                           , minDuration = 2*60 
>                           , maxDuration = 6*60
>                           }
>     times = [(15*q) `addMinutes'` starttime | q <- [0..96]]

Look at the scores over a range where none are zero.

> test_averageScore2 = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     rt <- getRT
>     (scoreTotal, scoreTotal', avgScore) <- runScoring w [] rt $ do
>         sf <- genScore starttime [sess]
>         scores <- lift $ mapM (score' w rt sf) times
>         let scoreTotal = addScores scores
>         scoreTotal' <- totalScore sf dt dur sess
>         avgScore <- averageScore sf dt sess
>         return (scoreTotal, scoreTotal', avgScore)
>     assertEqual "test_averageScore2_addScores" expectedTotal scoreTotal
>     assertAlmostEqual "test_averageScore2_totalScore" 3  expectedTotal scoreTotal'
>     assertAlmostEqual "test_averageScore2_avgScore" 3 expectedAvg avgScore
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     dur = 2*60
>     sess = defaultSession { sAllottedT = 24*60 
>                           , minDuration = dur 
>                           , maxDuration = 6*60
>                           , frequency = 2.0
>                           }
>     score' w rt sf dt = do
>         fs <- runScoring w [] rt (sf dt sess)
>         return $ eval fs
>     dt = (40*quarter) `addMinutes'` starttime -- start where scores /= 0
>     numQtrs = dur `div` quarter
>     times = [(q*quarter) `addMinutes'` dt | q <- [0..numQtrs-1]]
>     expectedTotal = 4.0697494::Score --4.0578547 :: Score  
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

> test_obsAvailable3 = TestCase $ do
>   assertEqual "test_obsAvailable3_1" False  (obsAvailable dt s)
>   w <- getWeather Nothing
>   rt <- getRT
>   fs <- runScoring w [] rt (observerAvailable dt s)
>   assertEqual "test_obsAvailable3_2" expFalse (eval fs)
>     where
>       dt  = fromGregorian 2006 2 1 0 0 0
>       bdt1_1 = fromGregorian 2009 11 13 9 0 0
>       bdt1_2 = fromGregorian 2009 11 17 9 0 0
>       bdt2_1 = fromGregorian 2009  9 30 9 0 0
>       bdt2_2 = fromGregorian 2009 10  3 9 0 0
>       bs = [(bdt1_1, bdt1_2), (bdt2_1, bdt2_2)] 
>       o   = defaultObserver {oId = 264
>                            , firstName = "Bengt-Goran"
>                            , lastName = "Andersson"
>                            , username = "bgandersson"
>                            , pstId = 3113
>                            , sanctioned = False
>                            , reservations = []
>                            , blackouts = bs}
>       p   = defaultProject { observers = [o] }
>       s   = defaultSession { project = p}
>       expFalse = 0.0

> test_observerAvailable = TestCase $ do
>   w <- getWeather Nothing
>   rt <- getRT
>   fs <- runScoring w [] rt (observerAvailable dt s)
>   assertEqual "test_observerAvailable_1" expTrue (eval fs)
>   fs <- runScoring w [] rt (observerAvailable dt s2)
>   assertEqual "test_observerAvailable_2" expFalse (eval fs)
>   fs <- runScoring w [] rt (observerAvailable dt2 s2)
>   assertEqual "test_observerAvailable_3" expTrue (eval fs)
>   fs <- runScoring w [] rt (observerAvailable dt s3)
>   assertEqual "test_observerAvailable_4" expFalse (eval fs)
>   fs <- runScoring w [] rt (observerAvailable dt2 s3)
>   assertEqual "test_observerAvailable_5" expTrue (eval fs)
>   fs <- runScoring w [] rt (observerAvailable dt3 s3)
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
>   rt <- getRT
>   assertEqual "test_needsLowRFI" True (isDayTime day)
>   assertEqual "test_needsLowRFI" False (isDayTime night)
>   fs <- runScoring w [] rt (needsLowRFI day sAnyTime)
>   assertEqual "test_needsLowRFI" 1.0 (eval fs)
>   fs <- runScoring w [] rt (needsLowRFI night sAnyTime)
>   assertEqual "test_needsLowRFI" 1.0 (eval fs)
>   fs <- runScoring w [] rt (needsLowRFI night sNightTime)
>   assertEqual "test_needsLowRFI" 1.0 (eval fs)
>   fs <- runScoring w [] rt (needsLowRFI day sNightTime)
>   assertEqual "test_needsLowRFI" 0.0 (eval fs)
>     where
>       day   = fromGregorian 2008 1 1 15 0 0 -- rfi day starts at 12:00 UT 
>       night = fromGregorian 2008 1 2 1 30 0 -- rfi night starts at 24:00 UT 
>       sAnyTime = findPSessionByName "CV"
>       sNightTime = sAnyTime { lowRFI = True }

> test_lstExcepted = TestCase $ do
>   w <- getWeather Nothing
>   rt <- getRT
>   fs <- runScoring w [] rt (lstExcepted dtClear sAnyTime)
>   assertEqual "test_lstExcpeted_1" 1.0 (eval fs)
>   fs <- runScoring w [] rt (lstExcepted dtClear sExclude1)
>   assertEqual "test_lstExcpeted_2" 1.0 (eval fs)
>   fs <- runScoring w [] rt (lstExcepted dtNotClear sExclude1)
>   assertEqual "test_lstExcpeted_3" 0.0 (eval fs)
>   fs <- runScoring w [] rt (lstExcepted dtClear sExclude2)
>   assertEqual "test_lstExcpeted_4" 0.0 (eval fs)
>   fs <- runScoring w [] rt (lstExcepted dtNotClear sExclude2)
>   assertEqual "test_lstExcpeted_5" 1.0 (eval fs)
>   assertEqual "test_lstExpected_6" True  (checkLst dt $ lstExclude sExclude1) 
>   assertEqual "test_lstExpected_7" False (checkLst dt $ lstExclude sExclude2) 
>   fs <- runScoring w [] rt (lstExcepted dt2 sExclude3)
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
>   rt <- getRT
>   fs <- runScoring w [] rt (enoughTimeBetween tdt1 s1)
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
>       s2 = makeSession s2' [] ps2
>       r2 = enoughTimeBetween' tdt1 s2
>       -- now potential problems - session w/ timebetween & periods
>       s3' = defaultSession { timeBetween = 60 }
>       ps3 = map (mkPeriod s3') [dt1, dt2, dt3]
>       s3 = makeSession s3' [] ps3
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
>                                    , startTime = dt
>                                    , duration = 60
>                                    , pDuration = 60
>                                     }
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
>       s = defaultSession { grade = 4.0 }
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
>       s10 = defaultSession { receivers = [[boost]], grade = 3.0 }

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
>       s = defaultSession { grade = 4.0 }
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

> test_scorePeriod = TestCase $ do
>   -- do explicitly what scorePeriod is supposed to do
>   w <- getWeather $ Just startDt
>   rt <- getRT
>   scores <- mapM (scoreSession w rt) dts
>   let weightedAvgScore = (sum . tail $ scores) / 4.0
>   -- now 
>   periodScore <- scorePeriod p s ss w [] rt
>   assertEqual "test_scorePeriod_1" weightedAvgScore periodScore
>   where
>     startDt = fromGregorian 2006 2 1 0 0 0
>     scoreSession w rt dt = do
>       fs <- runScoring w [] rt $ genScore dt ss >>= \f -> f dt s
>       return $ eval fs
>     ss = pSessions
>     s = head ss
>     -- do this explicitly to avoid mistakes
>     mins = [0, 15, 30, 45] -- 60 minutes!
>     dts = map (\m -> addMinutes m startDt) mins
>     -- create a period that covers this same time range
>     p = defaultPeriod { session = s
>                       , startTime = startDt
>                       , duration = 60
>                       , pForecast = startDt
>                       }

TBF: this won't work until we start using the expanded weather frequencies
and irradiance

> test_mustang = TestCase $ do
>     --assertEqual "test_mustang_1" True (usesMustang ms)
>     --assertEqual "test_mustang_2" False (usesMustang ds)
>     w <- getWeather $ Just dtNight
>     rt <- getRT
>     -- Factor: Stringency
>     fs <- runScoring w [] rt (stringency dtDay ds)
>     assertEqual "test_mustang_3" True (eval fs /= 9.0)
>     fs <- runScoring w [] rt (stringency dtDay ms)
>     assertEqual "test_mustang_4" 9.0 (eval fs)
>
>     -- Factor: surfaceObservingEfficiency
>     fs <- runScoring w [] rt (surfaceObservingEfficiency dtNight ms)
>     assertEqual "test_mustang_5" 1.0 (eval fs) 
>     fs <- runScoring w [] rt (surfaceObservingEfficiency dtNight ds)
>     assertEqual "test_mustang_6" 1.0 (eval fs) 
>     fs <- runScoring w [] rt (surfaceObservingEfficiency dtDay ms)
>     -- TBF: 0.18 != 0.28
>     assertEqual "test_mustang_61" 0.20957701 (eval fs) 
>     fs <- runScoring w [] rt (surfaceObservingEfficiency dtDay ds)
>     assertEqual "test_mustang_62" 0.996918 (eval fs) 
>
>     -- Factor: trackingEfficiency
>     fs <- runScoring w [] rt (trackingEfficiency dtNight ds)
>     assertEqual "test_mustang_7" 0.9963897 (eval fs) --0.9980611 (eval fs) 
>     -- TBF: check this value
>     fs <- runScoring w [] rt (trackingEfficiency dtNight ms)
>     assertEqual "test_mustang_8" 0.73873913 (eval fs) -- 0.8946233 (eval fs) 

>     -- Factor: trackingErrorLimit
>     fs <- runScoring w [] rt (trackingErrorLimit dtNight ds)
>     assertEqual "test_mustang_9" 1.0 (eval fs) 
>     fs <- runScoring w [] rt (trackingErrorLimit dtDay ds)
>     assertEqual "test_mustang_10" 1.0 (eval fs) 
>     --wind_ms <- wind w dtDay
>     --wind_w2 <- w2_wind w dtDay
>     --print $ "wind: day " ++ (show $ wind_ms) ++ " " ++ (show wind_w2)
>     fs <- runScoring w [] rt (trackingErrorLimit dtDay ms)
>     assertEqual "test_mustang_11" 1.0 (eval fs) 
>     --wind_ms <- wind w dtNight
>     --wind_w2 <- w2_wind w dtNight
>     --print $ "wind: night " ++ (show $ wind_ms) ++ " " ++ (show wind_w2)
>     fs <- runScoring w [] rt (trackingErrorLimit dtNight ms)
>     assertEqual "test_mustang_12" 0.0 (eval fs) --1.0 (eval fs) 
>     
>     -- Factor: observingEfficiencyLimit
>     fs <- runScoring w [] rt (observingEfficiencyLimit dtNight ds)
>     assertEqual "test_mustang_13" 1.0 (eval fs) 
>     fs <- runScoring w [] rt (observingEfficiencyLimit dtDay ds)
>     assertEqual "test_mustang_14" 1.0 (eval fs) 
>     fs <- runScoring w [] rt (observingEfficiencyLimit dtNight ms)
>     assertEqual "test_mustang_15" False (eval fs < epsilon) 
>     fs <- runScoring w [] rt (observingEfficiencyLimit dtDay ms)
>     assertEqual "test_mustang_15_2" False (eval fs < epsilon) 
>
>     -- Factor: Hour Angle Limit
>     fs <- runScoring w [] rt (hourAngleLimit dtNight ds)
>     assertEqual "test_mustang_16" 1.0 (eval fs ) 
>     fs <- runScoring w [] rt (hourAngleLimit dtDay ds)
>     assertEqual "test_mustang_17" 1.0 (eval fs ) 
>     fs <- runScoring w [] rt (hourAngleLimit dtNight ms)
>     assertEqual "test_mustang_18" 1.0 (eval fs ) 
>     fs <- runScoring w [] rt (hourAngleLimit dtDay ms)
>     assertEqual "test_mustang_19" 1.0 (eval fs ) 
>     
>     -- Factor : atmosphericEfficiency
>     fs <- runScoring w [] rt (atmosphericOpacity dtNight ds)
>     assertEqual "test_mustang_20" 0.9742651 (eval fs ) 
>     fs <- runScoring w [] rt (atmosphericOpacity dtDay ds)
>     assertEqual "test_mustang_21" 0.9747749 (eval fs ) 
>     fs <- runScoring w [] rt (atmosphericOpacity dtNight ms)
>     assertEqual "test_mustang_22" 0.83687115 (eval fs ) 
>     fs <- runScoring w [] rt (atmosphericOpacity dtDay ms)
>     assertEqual "test_mustang_23" 0.7528943 (eval fs ) 
>     
>     -- Factor: Frequncy Pressure 

>   where
>     epsilon = 1.0e-5
>     ms = defaultSession { receivers = [[Rcvr_PAR]]
>                         , frequency = 90.0 
>                         , dec = 1.5 -- always up
>                         }
>     ds = defaultSession { frequency = 4.0 
>                         , dec = 1.5
>                         }
>     dtDay = fromGregorian 2006 2 1 14 0 0 
>     dtNight = fromGregorian 2006 2 1 5 0 0 
 

> test_elevationLimit = TestCase $ do
>   assertEqual "test_elevationLimit_1" True (elevationLimit' dt s1)
>   assertEqual "test_elevationLimit_2" True (elevationLimit' dt s2)
>   assertEqual "test_elevationLimit_3" False (elevationLimit' dt s3)
>   -- now make sure we override hour angle limit properly
>   rt <- getRT
>   w <- getWeather $ Just dt
>   fs <- runScoring w [] rt (hourAngleLimit dt s1)
>   assertEqual "test_elevationLimit_4" 1.0 (eval fs)
>   fs <- runScoring w [] rt (hourAngleLimit dt s2)
>   assertEqual "test_elevationLimit_5" 1.0 (eval fs)
>   fs <- runScoring w [] rt (hourAngleLimit dt s3)
>   assertEqual "test_elevationLimit_6" 0.0 (eval fs)
>     where
>   s1 = defaultSession { dec = 1.5 } -- always up
>   dt = fromGregorian 2006 3 1 0 0 0
>   s2 = defaultSession { dec = 1.5 -- always up
>                       , elLimit = Just . deg2rad $ 10.0  } 
>   s3 = defaultSession { dec = 1.5 -- always up
>                       , elLimit = Just . deg2rad $ 70.0  } 


> test_atmosphericStability = TestCase $ do
>     w <- getWeather $ Just dt
>     rt <- getRT
>     fs <- runScoring w [] rt $ atmosphericStabilityLimit dt s1  
>     -- TBF: will be False till we get irradiance into DB
>     assertEqual "test_atmosphericStability_1" 0.0 (eval fs)
>     fs <- runScoring w [] rt $ atmosphericStabilityLimit dt s2  
>     assertEqual "test_atmosphericStability_2" 1.0 (eval fs)
>   where
>     s1 = defaultSession { dec = 1.5, oType = Continuum } -- always up
>     dt = fromGregorian 2006 3 1 0 0 0
>     s2 = defaultSession { dec = 1.5, oType = SpectralLine } -- always up

> test_efficiency_below2GHz = TestCase $ do
>     w <- getWeather . Just $ wdt 
>     rt <- getRT
>     Just result <- runScoring w [] rt (efficiency dt s1) 
>     assertEqual "test_efficiency_below2GHz_1" 0.9722724 result
>     Just result <- runScoring w [] rt (efficiency dt s2) 
>     assertEqual "test_efficiency_below2GHz_2" 0.99295783 result
>     Just result <- runScoring w [] rt (efficiency dt s3) 
>     assertEqual "test_efficiency_below2GHz_3" 0.99823254 result
>   where
>     wdt = fromGregorian 2006 9 1 1 0 0
>     dt = fromGregorian 2006 9 2 14 30 0
>     s1 = defaultSession { dec = 1.5, frequency = 2.0 } 
>     s2 = defaultSession { dec = 1.5, frequency = 1.0 } 
>     s3 = defaultSession { dec = 1.5, frequency = 0.5 } 

Test utilities

> assertAlmostEqual :: String -> Int -> Float -> Float -> IO ()
> assertAlmostEqual name places expected value =
>     assertBool name $ abs (value - expected) < epsilon
>   where
>     epsilon = 1.0 / 10.0 ** fromIntegral places

> assertScoringResult :: String -> Maybe DateTime -> Int -> Float -> Scoring Factors -> IO ()
> assertScoringResult name dt  digits expected scoref = do
>     w <- getTestWeather dt  
>     rt <- getRT
>     [(_, Just result)] <- runScoring w rSched rt scoref
>     assertAlmostEqual name digits expected result

> assertScoringResult' :: String -> Maybe DateTime -> Float -> Scoring Factors -> IO ()
> assertScoringResult' name dt expected scoref = do
>     w <- getTestWeather dt 
>     rt <- getRT
>     [(_, Just result)] <- runScoring w rSched rt scoref
>     assertEqual name expected result

> assertResult :: String -> Maybe DateTime -> Int -> Float -> Scoring (Maybe Float) -> IO ()
> assertResult name dt digits expected scoref = do
>     w <- getTestWeather dt
>     rt <- getRT
>     Just result <- runScoring w rSched rt scoref
>     assertAlmostEqual name digits expected result

> assertResult' :: String -> Maybe DateTime -> Float -> Scoring (Maybe Float) -> IO ()
> assertResult' name dt expected scoref = do
>     w <- getTestWeather dt
>     rt <- getRT
>     Just result <- runScoring w rSched rt scoref
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
>         grades = [4.0, 4.0, 4.0, 4.0]
>         genPSess t u ra b g = defaultSession {
>             sAllottedS = t
>           , sAllottedT = t
>           , periods = [defaultPeriod {duration = u
>                                     , pState = Scheduled
>                                     , pDuration = u}]
>           , ra = hrs2rad ra
>           , band = b
>           , grade = g
>         }

> rSessions = zipWith5 genPSess tots useds ras bands grades
>   where tots   = [12*60, 18*60, 10*60, 20*60]
>         useds  = [ 2*60,  8*60,  5*60, 12*60]
>         ras    = [  5.4,  10.1,   4.9,  18.1]
>         bands  = [    L,     C,     X,     L]
>         grades = [4.0, 4.0, 4.0, 4.0]
>         genPSess t u ra b g = defaultSession {
>             sAllottedS = t
>           , sAllottedT = t
>           , periods = [defaultPeriod {duration = u
>                                     , pState = Scheduled
>                                     , pDuration = u}]
>           , ra = hrs2rad ra
>           , band = b
>           , grade = g
>         }

> rSched = [ (fromGregorian 2006 6 14 12 0 0, [Rcvr1_2, Rcvr26_40])
>          , (fromGregorian 2006 6 21 12 0 0, [Rcvr1_2, Rcvr12_18])
>          , (fromGregorian 2006 6 24 16 0 0, [Rcvr4_6, Rcvr12_18])
>          , (fromGregorian 2006 7  1 12 0 0, [Rcvr1_2, Rcvr4_6])
>          ]
