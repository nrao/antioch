> module Antioch.ScoreTests where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Test.HUnit
> import Data.List (zipWith4, zipWith5)

> tests = TestList [
>     test_hourAngleLimit
>   , test_rightAscensionPressure
>   , test_frequencyPressure
>   , test_efficiency
>   , test_zenithOpticalDepth
>   , test_receiverTemperature
>   , test_kineticTemperature
>   , test_projectCompletion
>   , test_stringency
>   , test_politicalFactors
>   , test_trackingEfficiency
>   , test_trackingErrorLimit
>   , test_zenithAngleLimit
>   ]

> benchmark = do
>     start <- getCurrentTime
>     runTestTT tests
>     stop <- getCurrentTime
>     putStrLn $ "Test Execution Speed: " ++ show (diffSeconds stop start) ++ " seconds"

> test_hourAngleLimit = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 0
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
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertScoringResult "test_frequencyPressure" 5 1.35154 (freqPressure dt . head $ pSessions)
>   where
>     freqPressure = genFrequencyPressure pSessions

> test_rightAscensionPressure = TestCase $ do
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertScoringResult "test_rightAscensionPressure" 5 1.19812 (raPressure dt . head $ pSessions)
>   where
>     raPressure = genRightAscensionPressure pSessions

> test_receiver = TestCase $ do
>     let dt = fromGregorian 2006 6 15 12 0 0
>     assertScoringResult' "test_receiver" 0.0 (receiver dt sessLP)
>     let dt = fromGregorian 2006 6 25 12 0 0
>     assertScoringResult' "test_receiver" 1.0 (receiver dt sessLP)
>     let dt = fromGregorian 2006 8 1 12 0 0
>     assertScoringResult' "test_receiver" 0.0 (receiver dt sessAS)

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

> test_efficiency = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 2
>     let dt = fromGregorian 2006 10 15 12 0 0
>     -- sessLP
>     Just result <- runScoring w [] (efficiency dt sessLP) 
>     assertAlmostEqual "test_efficiency" 2 0.98215 result
>     Just result <- runScoring w [] (efficiencyHA dt sessLP)
>     assertAlmostEqual "test_efficiencyHA" 2 0.72034 result
>     -- sessWV
>     Just result <- runScoring w [] (efficiency dt sessWV) 
>     assertAlmostEqual "test_efficiency" 2 0.89721 result 
>     Just result <- runScoring w [] (efficiencyHA dt sessWV) 
>     assertAlmostEqual "test_efficiencyHA" 2 0.70341 result 
>     -- sessAS
>     Just result <- runScoring w [] (efficiency dt sessAS) 
>     assertAlmostEqual "test_efficiency" 2 0.9614 result
>     Just result <- runScoring w [] (efficiencyHA dt sessAS)
>     assertAlmostEqual "test_efficiencyHA" 2 0.4548 result
>     -- sessBug
>     Just result <- runScoring w [] (efficiency dt sessBug) 
>     assertAlmostEqual "test_efficiency" 2 0.93555 result
>     -- sessBug2
>     Just result <- runScoring w [] (efficiency dt sessBug2) 
>     assertAlmostEqual "test_efficiency" 4 0.95340 result

>     -- sessLP
> test_zenithOpticalDepth = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 0
>     -- sessLP
>     Just result <- runScoring w [] (zenithOpticalDepth dtLP sessLP)
>     assertAlmostEqual "test_zenithOpticalDepth" 5 0.00798 result
>     -- sessBug
>     let dt = fromGregorian 2006 10 15 12 0 0
>     Just result <- runScoring w [] (zenithOpticalDepth dt sessBug)
>     assertAlmostEqual "test_zenithOpticalDepth" 5 0.0661772 result
>     Just result <- runScoring w [] (zenithOpticalDepth dt sessBug2)
>     assertAlmostEqual "test_zenithOpticalDepth" 5 0.007394265 result

> test_receiverTemperature = TestCase $ do
>     assertEqual "test_receiverTemperature" 5.0 $ receiverTemperature dtLP sessLP
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertEqual "test_receiverTemperature" 60.0 $ receiverTemperature dt sessBug
>     assertEqual "test_receiverTemperature" 10.0 $ receiverTemperature dt sessBug2

> test_kineticTemperature = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 0
>     Just result <- runScoring w [] (kineticTemperature dtLP sessLP)
>     assertEqual "test_kineticTemperature" 257.498 result
>     let dt = fromGregorian 2006 10 15 12 0 0
>     Just result <- runScoring w [] (kineticTemperature dt sessBug2)
>     assertEqual "test_kineticTemperature" 256.982 result

> test_stringency = TestCase $ do
>     let dt = fromGregorian 2006 10 15 18 0 0
>     assertScoringResult "test_stringency" 5 1.40086 (stringency dt sessLP)
>     assertScoringResult "test_stringency" 5 1.03437 (stringency dt sessAS)

> test_projectCompletion = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 13 22 0 0 -- don't need!
>     let dt = fromGregorian 2006 10 15 18 0 0 -- don't need!
>     -- adjust the project's times to get desired results
>     let p = defaultProject {timeLeft=28740, timeTotal=33812}
>     let s = sessLP {project = p}
>     [(_, Just result)] <- runScoring w [] (projectCompletion dt s)
>     assertAlmostEqual "test_projectCompletion" 3 1.015 result

TBF are these partitions stil useful?

> test_politicalFactors = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 13 22 0 0
>     let dt = fromGregorian 2006 10 15 12 0 0
>     -- adjust the project's times to get desired results
>     let p = defaultProject {timeLeft=28740, timeTotal=33812}
>     let s = sessLP {project = p}
>     -- missing window, transit, observerOnSite, and ObserverAvailable
>     let politicalFactors = score [scienceGrade
>                           , thesisProject
>                           , projectCompletion]
>     fs <- runScoring w [] (politicalFactors dt s)
>     -- TBF: check individual results as well
>     -- let expFs = [("scienceGrade", Just 1.0)
>     --           , ("thesisProject", Just 1.0)
>     --           , ("projectCompletion", Just 1.0)]
>     let result = eval fs
>     assertAlmostEqual "test_politicalFactors" 3 1.015 result

> test_trackingEfficiency = TestCase $ do
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertScoringResult "test_trackingEfficiency" 4 0.99764 (trackingEfficiency dt sessLP)

TBF: this unit test fails because 'wind w dt' fails

> test_trackingErrorLimit = TestCase $ do
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertScoringResult' "test_trackingErrorLimit" 1.0 (trackingErrorLimit dt sessLP)

> test_zenithAngleLimit = TestCase $ do
>     let dt = fromGregorian 2006 10 15 0 0 0
>     assertScoringResult' "test_zenithAngleLimit" 0.0 (zenithAngleLimit dt sessLP)

Test utilities

> assertAlmostEqual :: String -> Int -> Float -> Float -> IO ()
> assertAlmostEqual name places expected value =
>     assertBool name $ abs (value - expected) < epsilon
>   where
>     epsilon = 1.0 / 10.0 ** fromIntegral places

> assertScoringResult :: String -> Int -> Float -> Scoring Factors -> IO ()
> assertScoringResult name digits expected scoref = do
>     w <- getTestWeather
>     [(_, Just result)] <- runScoring w rSched scoref
>     assertAlmostEqual name digits expected result

> assertScoringResult' :: String -> Float -> Scoring Factors -> IO ()
> assertScoringResult' name expected scoref = do
>     w <- getTestWeather
>     [(_, Just result)] <- runScoring w rSched scoref
>     assertEqual name expected result

> getTestWeather :: IO Weather
> getTestWeather = getWeather . Just $ fromGregorian 2006 10 13 22 0 0

Test data generation

Alloc #6 in beta test

> sessWV = defaultSession {
>     sName     = "WV"
>   , ra        = hrs2rad 4.2 
>   , dec       = deg2rad 17.4
>   , frequency = 34.9
>   , receivers = [Rcvr26_40]
>   }

Alloc #7 in beta test

> sessAS = defaultSession {
>     sName     = "AS"
>   , ra        = hrs2rad 14.3 
>   , dec       = deg2rad 18.4
>   , frequency = 0.5
>   , receivers = [Rcvr_450]
>   }

Alloc #3 in beta test

> sessLP = defaultSession {
>     sName     = "LP"
>   , ra        = hrs2rad 12.3
>   , dec       = deg2rad  5.4
>   , frequency = 5.4
>   , receivers = [Rcvr4_6]
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
>             totalTime = t, totalUsed = u, ra = ra, band = b
>         }

> rSessions = zipWith4 genPSess tots useds ras bands 
>   where tots  = [12*60, 18*60, 10*60, 20*60]
>         useds = [ 2*60,  8*60,  5*60, 12*60]
>         ras   = [  5.4,  10.1,   4.9,  18.1]
>         bands = [    L,     C,     X,     L]
>         genPSess t u ra b = defaultSession {
>             totalTime = t, totalUsed = u, ra = ra, band = b
>         }

> rSched = [ (fromGregorian 2006 6 14 12 0 0, [Rcvr1_2, Rcvr26_40])
>          , (fromGregorian 2006 6 21 12 0 0, [Rcvr1_2, Rcvr12_18])
>          , (fromGregorian 2006 6 24 16 0 0, [Rcvr4_6, Rcvr12_18])
>          , (fromGregorian 2006 7  1 12 0 0, [Rcvr1_2, Rcvr4_6])
>          ]
