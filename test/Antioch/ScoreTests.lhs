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

> tests = TestList [
>     test_averageScore
>   , test_averageScore2
>   , test_efficiency
>   , test_frequencyPressure
>   , test_getReceivers
>   , test_hourAngleLimit
>   , test_kineticTemperature
>   , test_minObservingEff
>   , test_minimumObservingConditions
>   , test_minObservingEff
>   , test_minTsysPrime
>   , test_observingEfficiency
>   , test_observingEfficiency2
>   , test_observingEfficiencyLimit
>   , test_politicalFactors
>   , test_projectCompletion
>   , test_receiver
>   , test_receiverTemperature
>   , test_rightAscensionPressure
>   , test_score
>   , test_scoreCV
>   , test_scoreCV2
>   , test_stringency
>   , test_surfaceObservingEfficiency
>   , test_trackingEfficiency
>   , test_trackingErrorLimit
>   , test_zenithAngle
>   , test_zenithAngleAtTransit
>   , test_zenithAngleLimit
>   , test_zenithOpticalDepth
>   ]

> benchmark = do
>     start <- getCurrentTime
>     runTestTT tests
>     stop <- getCurrentTime
>     putStrLn $ "Test Execution Speed: " ++ show (diffSeconds stop start) ++ " seconds"

> test_hourAngleLimit = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 2
>     scores <- mapM (score' w) times
>     assertEqual "test_hourAngleLimit" expected scores
>   where
>     score' w dt = do
>         [(_, Just s)] <- runScoring w [] (hourAngleLimit dt sessLP)
>         return s
>     times = [(60*h) `addMinutes'` dtLP | h <- [0..23]]
>     expected = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0,
>                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

> test_frequencyPressure = TestCase $ do
>     freqPressure <- runScoring undefined [] $ genFrequencyPressure pSessions
>     assertScoringResult "test_frequencyPressure generated" Nothing 5 1.35154 (freqPressure undefined . head $ pSessions)

> test_frequencyPressureComparison = TestCase $ do
>     freqPressure <- runScoring undefined [] $ genFrequencyPressure pSessions
>     assertScoringResult' "test_frequencyPressure comparison" Nothing 2.64413777007 (freqPressure undefined . head $ ss)
>   where
>     ss = concatMap sessions pTestProjects
>     -- s = head $ filter (\s -> "CV" == (sName s)) ss

> test_rightAscensionPressure = TestCase $ do
>     raPressure <- runScoring undefined [] $ genRightAscensionPressure pSessions
>     assertScoringResult "test_rightAscensionPressure" Nothing 5 1.19812 (raPressure undefined . head $ pSessions)

> test_receiver = TestCase $ do
>     let dt = fromGregorian 2006 6 15 12 0 0
>     assertScoringResult' "test_receiver" Nothing 0.0 (receiver dt sessLP)
>     let dt = fromGregorian 2006 6 25 12 0 0
>     assertScoringResult' "test_receiver" Nothing 1.0 (receiver dt sessLP)
>     let dt = fromGregorian 2006 8 1 12 0 0
>     assertScoringResult' "test_receiver" Nothing 0.0 (receiver dt sessAS)

> test_getReceivers = TestCase $ do
>     assertEqual "test_getReceivers" [Rcvr4_6, Rcvr12_18] result
>       where result = getReceivers (fromGregorian 2006 6 24 16 0 0) rSched

> test_zenithAngle = TestCase $ do
>    let dt = fromGregorian 2006 10 15 12 0 0 
>    let result = zenithAngle dt sessLP
>    assertAlmostEqual "test_zenithAngle" 5 (deg2rad 63.704613) result 
>    let result = zenithAngle dt sessBug
>    assertAlmostEqual "test_zenithAngle" 4 (deg2rad 40.5076) result 
>    let result = zenithAngle dt sessBug2
>    assertAlmostEqual "test_zenithAngle" 4 (deg2rad 81.50164) result 

> test_zenithAngleAtTransit = TestCase $ do
>    let result = zenithAngleAtTransit sessLP
>    assertEqual "test_zenithAngleAtTransit" (deg2rad 33.03313) result 
>    let result = zenithAngleAtTransit sessBug
>    assertAlmostEqual "test_zenithAngleAtTransit" 5 (deg2rad 30.98467) result 
>    let result = zenithAngleAtTransit sessBug2
>    assertAlmostEqual "test_zenithAngleAtTransit" 5 (deg2rad 44.62250) result 

> test_minTsysPrime = TestCase $ do
>    w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 2
>    -- sessLP
>    Just result <- minTSysPrime w (frequency sessLP) (elevation sessLP)
>    assertAlmostEqual "test_minTsysPrime" 3 15.490067 result 
>    -- sessAS
>    Just result <- minTSysPrime w (frequency sessAS) (elevation sessAS)
>    assertAlmostEqual "test_minTsysPrime" 3 25.958 result 
>    -- sessBug
>    Just result <- minTSysPrime w (frequency sessBug) (elevation sessBug)
>    assertAlmostEqual "test_minTsysPrime" 3 92.365046 result 
>    -- sessBug2
>    Just result <- minTSysPrime w (frequency sessBug2) (elevation sessBug2)
>    assertAlmostEqual "test_minTsysPrime" 4 29.858517 result 
>      where 
>        -- TBF: gaurd against elevations < 5.0 degrees
>        elevation s = max (deg2rad 5.0)  (pi/2 - zenithAngle dt s)
>        dt = fromGregorian 2006 10 15 12 0 0

TBF: first part of this test passes, but match to python does not work.

> {-
> test_observingEfficiency = TestCase $ do
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] (observingEfficiency dt s)
>     let result = eval fs
>     assertEqual "test_observingEfficiency" 0.8661948 result
>     -- match to python
>     let dt = fromGregorian 2006 10 13 16 0 0
>     w <- getWeather . Just $ dt
>     fs <- runScoring w [] (observingEfficiency dt sGB)
>     let result = eval fs
>     assertEqual "test_observingEfficiency" 0.100085918826 result
>       where    
>         --names = ["GB","CV","LP","TX","VA","WV","AS"]
>         --sess = concatMap (\name -> findPSessionByName name) names
>         sGB = head $ findPSessionByName "GB"
>     -}

TBF: trackingErrorLimit seems to work, but the minObsEff doesn't seem too. 

> test_minimumObservingConditions = TestCase $ do
>    let dt = fromGregorian 2006 10 13 16 0 0
>    w <- getWeather . Just $ dt
>    -- effs <- mapM (eff w dt) sess
>    -- efls <- mapM (efl w dt) sess
>    mocs <- mapM (moc w dt) sess
>    let minObsEff = map (minObservingEff . frequency) sess 
>    -- print $ zip4 names mocs minObsEff effs
>    assertEqual "test_minimumObservingConditions" expected mocs
>   where
>     {-
>     efl w dt s = do
>         [(_,Just result)] <- runScoring w [] (observingEfficiencyLimit dt s)
>         return result
>     eff w dt s = do
>         fs <- runScoring w [] (observingEfficiency dt s)
>         return $ eval fs
>     -}
>     moc w dt s = do
>         Just result <- runScoring w [] (minimumObservingConditions dt s)
>         return result
>     names = ["GB","CV","LP","TX","VA","WV","AS"]
>     sess = concatMap (\name -> findPSessionByName name) names
>     expected = [False, True, True, False, False, False, True]

> test_observingEfficiency = TestCase $ do
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] (observingEfficiency dt s)
>     let result = eval fs
>     assertEqual "test_observingEfficiency" 0.86619097 result

Test against beta test code:

> test_observingEfficiency2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 8 0 0
>     let dt1 = fromGregorian 2006 10 15 12 0 0
>     let dt2 = fromGregorian 2006 10 15 11 0 0
>     let sLP = head $ findPSessionByName "LP" 
>     let sGB = head $ findPSessionByName "GB" 
>     fs <- runScoring w [] (observingEfficiency dt1 sLP)
>     assertEqual "test_observingEfficiency2"  0.9798773 (eval fs)
>     fs <- runScoring w [] (observingEfficiency dt2 sLP)
>     assertEqual "test_observingEfficiency2"  0.97485375 (eval fs)
>     fs <- runScoring w [] (observingEfficiency dt1 sGB)
>     assertEqual "test_observingEfficiency2"  0.83010453 (eval fs)

> test_observingEfficiencyLimit = TestCase $ do
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     -- result <- runScoring w [] (observingEfficiencyLimit dt s)
>     [(_, Just result)] <- runScoring w [] (observingEfficiencyLimit dt s)
>     assertEqual "test_observingEfficiencyLimit" (1.5337046e-3) result

> test_efficiency = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 2
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertResult "test_efficiency" (Just wdt) 2 0.98215 (efficiency dt sessLP)  
>     assertResult "test_efficiencyHA" (Just wdt) 2 0.72034 (efficiencyHA dt sessLP) 
>     assertResult "test_efficiency" (Just wdt) 2 0.89721 (efficiency dt sessWV) 
>     assertResult "test_efficiencyHA" (Just wdt) 2 0.70341 (efficiencyHA dt sessWV) 
>     assertResult "test_efficiency" (Just wdt) 2 0.9614 (efficiency dt sessAS) 
>     assertResult "test_efficiencyHA" (Just wdt) 2 0.4548 (efficiencyHA dt sessAS)
>     assertResult "test_efficiency" (Just wdt) 2 0.93555 (efficiency dt sessBug)
>     assertResult "test_efficiency" (Just wdt) 4 0.95340 (efficiency dt sessBug2) 
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     Just result <- runScoring w [] (efficiency dt s) 
>     assertEqual "test_efficiency" 0.87132007 result
>     Just result <- runScoring w [] (efficiencyHA dt s) 
>     assertEqual "test_efficiencyHA" 0.783711 result

> test_zenithOpticalDepth = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 2
>     assertResult "test_zenithOpticalDepth" (Just wdt) 5 0.00798 (zenithOpticalDepth dtLP sessLP)
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertResult "test_zenithOpticalDepth" (Just wdt) 5 0.0661772 (zenithOpticalDepth dt sessBug)
>     assertResult "test_zenithOpticalDepth" (Just wdt) 5 0.007394265 (zenithOpticalDepth dt sessBug2)

> test_receiverTemperature = TestCase $ do
>     assertEqual "test_receiverTemperature" 5.0 $ receiverTemperature dtLP sessLP
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertEqual "test_receiverTemperature" 60.0 $ receiverTemperature dt sessBug
>     assertEqual "test_receiverTemperature" 10.0 $ receiverTemperature dt sessBug2
>     -- pTestProjects session CV
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     let result = receiverTemperature dt s 
>     assertEqual "test_receiverTemperature" 5.0 result

> test_minObservingEff = TestCase $ do
>     -- pTestProjects session CV
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     let result = minObservingEff . frequency $ s
>     assertEqual "test_minObservingEff" 0.93819135 result

> test_kineticTemperature = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 0
>     assertResult' "test_kineticTemperatureLP" (Just wdt) 257.49832 (kineticTemperature dtLP sessLP) 
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertResult' "test_kineticTemperatureBug" (Just wdt) 256.9823 (kineticTemperature dt sessBug2) 
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     Just result <- runScoring w [] (kineticTemperature dt s) 
>     assertEqual "test_kineticTemperatureCV" 271.3523 result

> test_stringency = TestCase $ do
>     let dt = fromGregorian 2006 10 15 18 0 0
>     assertScoringResult "test_stringency" Nothing 5 1.40086 (stringency dt sessLP)
>     assertScoringResult "test_stringency" Nothing 5 1.03437 (stringency dt sessAS)

> makeTestProject :: Minutes -> Minutes -> Project
> makeTestProject tl tt = makeProject proj' tt ss'
>   where
>     proj' = defaultProject { pName = "time use test" }
>     ss''  = [
>         defaultOpen {
>             periods = [defaultPeriod {duration = tt - tl}]
>           , totalTime = tt
>           }
>       ]
>     ss'   = [ makeSession s (periods s) | s <- ss'' ]

> test_projectCompletion = TestCase $ do
>     let dt = fromGregorian 2006 10 15 18 0 0 -- don't need!
>     -- adjust the project's times to get desired results
>     let p = makeTestProject 28740 33812
>     let s = sessLP {project = p}
>     assertScoringResult "test_projectCompletion" Nothing 3 1.015 (projectCompletion dt s)

TBF are these partitions stil useful?

> test_politicalFactors = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 13 22 0 0
>     let dt = fromGregorian 2006 10 15 12 0 0
>     let s = head . filter (\s -> "CV" == (sName s)) . concatMap sessions $ pTestProjects
>     -- missing window, transit, observerOnSite, and ObserverAvailable
>     let politicalFactors = score [scienceGrade
>                           , thesisProject
>                           , projectCompletion]
>     fs <- runScoring w [] (politicalFactors dt s)
>     -- TBF: how to check individual results as well?
>     -- let expFs = [("scienceGrade", Just 1.0)
>     --          , ("thesisProject", Just 1.0)
>     --          , ("projectCompletion", Just 1.015)]
>     -- assertEqual "test_politicalFactors" expFs fs
>     let result = eval fs
>     assertEqual "test_politicalFactors" 1.0024 result

> test_trackingEfficiency = TestCase $ do
>     -- sessLP
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertScoringResult "test_trackingEfficiency" Nothing 4 0.99764 (trackingEfficiency dt sessLP)
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     [(_, Just result)] <- runScoring w [] (trackingEfficiency dt s)
>     assertEqual "test_trackingEfficiency" 0.99796414 result 

> test_trackingErrorLimit = TestCase $ do
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertScoringResult' "test_trackingErrorLimit" Nothing 1.0 (trackingErrorLimit dt sessLP)
>     -- pTestProjects session CV
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     [(_, Just result)] <- runScoring w [] (trackingErrorLimit dt s)
>     assertEqual "test_trackingErrorLimit" 1.0 result

> test_zenithAngleLimit = TestCase $ do
>     let dt = fromGregorian 2006 10 15 0 0 0
>     assertScoringResult' "test_zenithAngleLimit" Nothing 0.0 (zenithAngleLimit dt sessLP)

> test_surfaceObservingEfficiency = TestCase $ do
>     let dt  = fromGregorian 2006 4 15 16 0 0
>     let wdt = Just $ fromGregorian 2006 4 15 0 0 0
>     assertScoringResult "test_surfaceObservingEfficienyLP" wdt 5 0.99392 (surfaceObservingEfficiency dt sessLP)
>     assertScoringResult "test_surfaceObservingEfficienyWV" wdt 5 0.77517 (surfaceObservingEfficiency dt sessWV)

> test_scoreCV = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] $ genScore ss >>= \f -> f dt s
>     let result = eval fs
>     assertEqual "test_scoreCV" (5.408132e-3) result  

New tests that do *not* match up to a 'beta test python code test', but rather
to use in conjunction with Pack tests.

> test_scoreCV2 = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 1 18 0 0
>     let dt = fromGregorian 2006 10 1 18 0 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] $ genScore ss >>= \f -> f dt s
>     let result = eval fs
>     assertAlmostEqual "test_scoreCV2" 3 3.9875174 result  

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
>         avgScoreForTime sf dt 15 s
>     assertEqual "test_avgScoreForTime" w1Score w3Score
>   where
>     dt = fromGregorian 2006 10 1 18 0 0
>     dt2 = fromGregorian 2006 10 1 0 0 0
>     ss = getOpenPSessions
>     s = head $ findPSessionByName "CV"
> 

> test_avgScoreForTime2 = TestCase $ do
>     -- weather that shouldn't get used
>     w <- getWeather $ Just dummytime
>     avgScore <- runScoring w [] $ do
>         sf <- genScore [s]
>         avgScoreForTime sf starttime (24*60) s
>     assertEqual "test_avgScoreForTime2" exp avgScore
>   where
>     dummytime = fromGregorian 2006 11 7 12 0 0
>     starttime = fromGregorian 2006 11 8 12 0 0
>     s = defaultSession {totalTime = 24*60, minDuration=2*60, maxDuration=6*60}
>     expScores = (replicate 39 0.0) ++ defaultScores ++ (replicate 22 0.0) 
>     exp = (sum expScores) / (fromIntegral $ length expScores) 
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
>                           , totalTime = 24*60
>                           , minDuration = 2*60
>                           , maxDuration = 6*60
>                           }
>     expected = (replicate 39 0.0) ++ defaultScores ++ (replicate 23 0.0)

For defaultSession w/ totalTime = 24*60; start time is  2006 11 8 12 0 0
plus 39 quarters.

> defaultScores= [3.2114944,3.2196305,3.2261546,2.8470442,3.0492089
>                ,3.1299076,3.140008,3.1896837,3.1915457,3.1966023
>                ,3.1995883,3.2383318,3.239888,3.2477167,3.248886
>                ,3.2764618,3.2764618,3.2766595,3.2766595,3.2787113
>                ,3.2787113,3.278528,3.2783365,3.2795804,3.2791758
>                ,3.2787383,3.27825,3.2757215,3.2750494,3.273897
>                ,3.273018,3.2730415,3.271333,3.2699947,3.2675872]

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
>     sess = defaultSession { totalTime = 24*60 
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
>     assertAlmostEqual "test_averageScore2_addScores" 3 expectedTotal scoreTotal
>     assertAlmostEqual "test_averageScore2_totalScore" 3  expectedTotal scoreTotal'
>     assertAlmostEqual "test_averageScore2_avgScore" 3 expectedAvg avgScore
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     dur = 2*60
>     sess = defaultSession { totalTime = 24*60 
>                           , minDuration = dur 
>                           , maxDuration = 6*60
>                           }
>     score' w sf dt = do
>         fs <- runScoring w [] (sf dt sess)
>         return $ eval fs
>     dt = (39*quarter) `addMinutes'` starttime -- start where scores /= 0
>     numQtrs = dur `div` quarter
>     times = [(q*quarter) `addMinutes'` dt | q <- [0..numQtrs-1]]
>     expectedTotal = 25.0133826 :: Score 
>     expectedAvg = expectedTotal / (fromIntegral numQtrs)

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

From beta tests:

> sessLP = defaultSession {
>     sId       = 3
>   , sName     = "LP"
>   , ra        = hrs2rad 12.3
>   , dec       = deg2rad  5.4
>   , frequency = 5.4
>   , receivers = [Rcvr4_6]
>   }

> sessWV = defaultSession {
>     sId       = 6
>   , sName     = "WV"
>   , ra        = hrs2rad 4.2 
>   , dec       = deg2rad 17.4
>   , frequency = 34.9
>   , receivers = [Rcvr26_40]
>   }

> sessAS = defaultSession {
>     sId       = 7
>   , sName     = "AS"
>   , ra        = hrs2rad 14.3 
>   , dec       = deg2rad 18.4
>   , frequency = 0.5
>   , receivers = [Rcvr_450]
>   }

*Not* from the beta test code - these are sessions that exposed bugs from the
QuickCheck properties.

> bugSessions = zipWith5 genBugSessions names ras decs freqs rcvrs 
>   where names  = ["bug1",   "bug2"]
>         ras    = [ 2.67,  0.873562]
>         decs   = [ 0.13, -0.108025]
>         freqs  = [39.76,       2.0]
>         rcvrs  = [[Rcvr26_40],[Rcvr1_2]]
>         genBugSessions n r d f rcvr = defaultSession {
>             sName = n, ra = r, dec = d, frequency = f, receivers = rcvr
>         }

> sessBug = bugSessions!!0
> sessBug2 = bugSessions!!1

> dtLP = fromGregorian 2006 10 15 12 0 0

> pSessions = zipWith4 genPSess tots useds ras bands 
>   where tots  = [12*60, 18*60, 10*60, 20*60]
>         useds = [ 2*60,  8*60,  5*60, 12*60]
>         ras   = [  5.4,  10.1,   4.9,  18.1]
>         bands = [    L,     C,     X,     L]
>         genPSess t u ra b = defaultSession {
>             totalTime = t
>           , periods = [defaultPeriod {duration = u}]
>           , ra = ra
>           , band = b
>         }

> rSessions = zipWith4 genPSess tots useds ras bands 
>   where tots  = [12*60, 18*60, 10*60, 20*60]
>         useds = [ 2*60,  8*60,  5*60, 12*60]
>         ras   = [  5.4,  10.1,   4.9,  18.1]
>         bands = [    L,     C,     X,     L]
>         genPSess t u ra b = defaultSession {
>             totalTime = t
>           , periods = [defaultPeriod {duration = u}]
>           , ra = ra
>           , band = b
>         }

> rSched = [ (fromGregorian 2006 6 14 12 0 0, [Rcvr1_2, Rcvr26_40])
>          , (fromGregorian 2006 6 21 12 0 0, [Rcvr1_2, Rcvr12_18])
>          , (fromGregorian 2006 6 24 16 0 0, [Rcvr4_6, Rcvr12_18])
>          , (fromGregorian 2006 7  1 12 0 0, [Rcvr1_2, Rcvr4_6])
>          ]
