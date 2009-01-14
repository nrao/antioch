> module Antioch.ScoreTests where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Test.HUnit
> import Data.List (zipWith4)

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

> test_hourAngleLimit = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 0
>     let scores = map (score' w) times
>     assertEqual "test_hourAngleLimit" expected scores
>   where
>     score' w dt =
>         let [(_, Just s)] = runScoring w [] (hourAngleLimit dt sessLP) in s
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

> test_efficiency = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 0
>     let Just result = runScoring w [] (efficiency dtLP sessLP)
>     assertAlmostEqual "test_efficiency" 5 0.71758 result
>     let Just result = runScoring w [] (efficiencyHA dtLP sessLP)
>     assertAlmostEqual "test_efficiencyHA" 5 0.72540 result

> test_zenithOpticalDepth = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 0
>     let Just result = runScoring w [] (zenithOpticalDepth dtLP sessLP)
>     assertAlmostEqual "test_zenithOpticalDepth" 5 0.00798 result

> test_receiverTemperature = TestCase $ do
>     assertEqual "test_receiverTemperature" 5.0 $ receiverTemperature dtLP sessLP

> test_kineticTemperature = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 0
>     let Just result = runScoring w [] (kineticTemperature dtLP sessLP)
>     assertAlmostEqual "test_kineticTemperature" 3 257.546 result

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
>     let [(name, Just result)] = runScoring w [] (projectCompletion dt s)
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
>     let fs = runScoring w [] (politicalFactors dt s)
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
>     let [(name, Just result)] = runScoring w rSched scoref
>     assertAlmostEqual name digits expected result

> assertScoringResult' :: String -> Float -> Scoring Factors -> IO ()
> assertScoringResult' name expected scoref = do
>     w <- getTestWeather
>     let [(name, Just result)] = runScoring w rSched scoref
>     assertEqual name expected result

> getTestWeather :: IO Weather
> getTestWeather = getWeather . Just $ fromGregorian 2006 10 13 22 0 0

Test data generation

> sessAS = defaultSession {
>     sName     = "AS"
>   , ra        = hrs2rad 14.3 
>   , dec       = deg2rad 18.4
>   , frequency = 0.5
>   , receivers = [Rcvr_450]
>   }

> sessLP = defaultSession {
>     sName     = "LP"
>   , ra        = hrs2rad 12.3
>   , dec       = deg2rad  5.4
>   , frequency = 5.4
>   , receivers = [Rcvr4_6]
>   }

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
