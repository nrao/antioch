> module Antioch.ScoreTests where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather      (Weather, getWeatherTest, minTSysPrime)
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Filters
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Control.Monad.Trans  (lift, liftIO)
> import Test.HUnit
> import Data.List            (zip4, zipWith4, zipWith5)
> import Data.Maybe           (isJust, fromJust)
> import Data.Array.IArray    (elems)

> tests = TestList [
>     test_hourAngleLimit
>   , test_frequencyPressure
>   , test_frequencyPressureComparison
>   , test_xBandPressure
>   , test_kaBandPressure
>   , test_rightAscensionPressure
>   , test_initBins1
>   , test_initBins2
>   , test_residue
>   , test_receiver
>   , test_getReceivers
>   , test_zenithAngle'
>   , test_zenithAngle
>   , test_zenithAngle2
>   , test_zenithAngleAtTransit
>   , test_minTsysPrime
>   , test_systemNoiseTemperature
>   , test_systemNoiseTemperature'
>   , test_minTsys'
>   , test_minimumObservingConditions
>   , test_goodElective
>   , test_isLastPeriodOfElective
>   , test_getRealOrForecastedWind
>   , test_observingEfficiency
>   , test_observingEfficiency2
>   , test_observingEfficiencyLimit
>   , test_observingEfficiencyLimit'
>   , test_minObservingEfficiencyFactor
>   , test_efficiency
>   , test_efficiency_below2GHz
>   , test_tSysPrime
>   , test_tSysPrime'
>   , test_atmosphericOpacity
>   , test_zenithOpacity'
>   , test_zenithOpacity
>   , test_zenithOpacity2
>   , test_zenithOpacityDryAir
>   , test_positionValues
>   , test_minObservingEff
>   , test_avgObservingEff
>   , test_kineticTemperature
>   , test_kineticTemperature2
>   , test_stringency
>   , test_scienceGrade
>   , test_projectCompletion
>   , test_politicalFactors
>   , test_halfPwrBeamWidth
>   , test_calculateTE
>   , test_trackingObservingEfficiency
>   , test_trackingEfficiency
>   , test_trackErr
>   , test_trackErrArray
>   , test_trackingErrorLimit
>   , test_positionFactors
>   , test_subfactorFactors
>   , test_weatherFactors
>   , test_scoreFactors
>   , test_availWindows
>   , test_inWindows
>   , test_scoreElements
>   , test_zenithAngleLimit
>   , test_rmsTrackingError
>   , test_variableTrackingError
>   , test_surfaceObservingEfficiency'
>   , test_surfaceObservingEfficiency
>   , test_scoreCV
>   , test_scoreCV2
>   , test_scoreForTime
>   , test_avgScoreForTime
>   , test_avgScoreForTime2
>   , test_weightedMeanScore
>   , test_score
>   , test_score_maintenance
>   , test_score_window
>   , test_bestDuration
>   , test_bestDurations
>   , test_averageScore
>   , test_averageScore2
>   , test_averageScore'
>   , test_obsAvailable
>   , test_obsAvailable2
>   , test_obsAvailable3
>   , test_obsAvailable4
>   , test_observerAvailable
>   , test_projectBlackout
>   , test_needsLowRFI
>   , test_lstExcepted
>   , test_enoughTimeBetween
>   , test_receiverBoost
>   , test_receiverBoost2
>   , test_observerOnSite
>   , test_scorePeriodOverhead
>   , test_scorePeriod
>   , test_scoreVlbiPeriod
>   , test_elevationLimit
>   , test_atmosphericStability
>   , test_calculateAtmStabilityLimit
>   ]

> benchmark = do
>     start <- getCurrentTime
>     runTestTT tests
>     stop <- getCurrentTime
>     putStrLn $ "Test Execution Speed: " ++ show (diffSeconds stop start) ++ " seconds"

> test_hourAngleLimit = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 9 15 2
>     rt <- getReceiverTemperatures
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
>     assertScoringResult' "test_frequencyPressure" Nothing 2.1132288 (freqPressure undefined . head $ pSessions)

Test that a frequency NOT in the initial bins gives a pressure of 1.0

> test_frequencyPressureComparison = TestCase $ do
>     freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure defaultStartTime pSessions
>     assertScoringResult' "test_frequencyPressure comparison" Nothing 1.0 (freqPressure undefined . head $ ss)
>   where
>     ss = concatMap sessions pTestProjects

tests for frequency pressure from
https://safe.nrao.edu/wiki/bin/view/Main/DSSUseCases

> test_xBandPressure = TestCase $ do
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt1 ss0
>   assertScoringResult' "test_xBandPressure 1" Nothing (sqrt 4.6888795) (freqPressure undefined . head $ ss0)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt2 ss0
>   assertScoringResult' "test_xBandPressure 2" Nothing (sqrt 3.0794415) (freqPressure undefined . head $ ss0)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt3 ss0
>   assertScoringResult' "test_xBandPressure 3" Nothing (sqrt 1.8556662) (freqPressure undefined . head $ ss0)
>
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt4 ss1
>   assertScoringResult' "test_xBandPressure 4" Nothing (sqrt 4.970292) (freqPressure undefined . head $ ss1)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt5 ss1
>   assertScoringResult' "test_xBandPressure 5" Nothing (sqrt 3.5839977) (freqPressure undefined . head $ ss1)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt6 ss1
>   assertScoringResult' "test_xBandPressure 6" Nothing (sqrt 2.0799203) (freqPressure undefined . head $ ss1)
>
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt7 ss2
>   assertScoringResult' "test_xBandPressure 7" Nothing (sqrt 5.1743875) (freqPressure undefined . head $ ss2)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt8 ss2
>   assertScoringResult' "test_xBandPressure 8" Nothing (sqrt 1.916291) (freqPressure undefined . head $ ss2)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt9 ss2
>   assertScoringResult' "test_xBandPressure 9" Nothing (sqrt 1.6778798) (freqPressure undefined . head $ ss2)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt10 ss2
>   assertScoringResult' "test_xBandPressure 10" Nothing (sqrt 1.3677249) (freqPressure undefined . head $ ss2)
>     where   
>       dt1  = fromGregorian 2006  2  1 0 0 0
>       dt2  = fromGregorian 2006  3 25 0 0 0
>       dt3  = fromGregorian 2006  4 11 0 0 0
>       dt4  = fromGregorian 2006  6  1 0 0 0
>       dt5  = fromGregorian 2006  6 13 0 0 0
>       dt6  = fromGregorian 2006  7 15 0 0 0
>       dt7  = fromGregorian 2006 10  1 0 0 0
>       dt8  = fromGregorian 2006 11 12 0 0 0
>       dt9  = fromGregorian 2006 12 11 0 0 0
>       dt10 = fromGregorian 2006 12 26 0 0 0
>       ss0 = [tom {authorized = True}
>            , dick {authorized = False}
>            , harry {authorized = False}
>             ]
>       ss1 = [tom {authorized = True}
>            , dick {authorized = True}
>            , harry {authorized = False}
>             ]
>       ss2 = [tom {authorized = True}
>            , dick {authorized = True}
>            , harry {authorized = True}
>             ]
>       tom = defaultSession {
>               sName = "Tom"
>             , periods = [
>                   defaultPeriod {
>                       startTime = fromGregorian 2006 3 24 0 0 0
>                     , pState = Scheduled
>                     , duration = 5*60
>                     , pDuration = 5*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 4 10 0 0 0
>                     , pState = Scheduled
>                     , duration = 12*60
>                     , pDuration = 12*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 6 12 0 0 0
>                     , pState = Scheduled
>                     , duration = 4*60
>                     , pDuration = 4*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 11 11 0 0 0
>                     , pState = Scheduled
>                     , duration = 1*60
>                     , pDuration = 1*60
>                      }
>                         ]
>             , sAllottedT = 40*60
>             , sAllottedS = 40*60
>             , frequency = 9.0
>             , band = X
>                          }
>       dick = defaultSession {
>               sName = "Dick"
>             , periods = [
>                   defaultPeriod {
>                       startTime = fromGregorian 2006 7 14 0 0 0
>                     , pState = Scheduled
>                     , duration = 14*60
>                     , pDuration = 14*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 11 11 0 0 0
>                     , pState = Scheduled
>                     , duration = 15*60
>                     , pDuration = 15*60
>                      }
>                         ]
>             , sAllottedT = 30*60
>             , sAllottedS = 30*60
>             , frequency = 9.0
>             , band = X
>                          }
>       harry = defaultSession {
>               sName = "Harry"
>             , periods = [
>                   defaultPeriod {
>                       startTime = fromGregorian 2006 11 11 0 0 0
>                     , pState = Scheduled
>                     , duration = 10*60
>                     , pDuration = 10*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 12 10 0 0 0
>                     , pState = Scheduled
>                     , duration = 7*60
>                     , pDuration = 7*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 12 25 0 0 0
>                     , pState = Scheduled
>                     , duration = 12*60
>                     , pDuration = 12*60
>                      }
>                         ]
>             , sAllottedT = 30*60
>             , sAllottedS = 30*60
>             , frequency = 9.0
>             , band = X
>                          }

> test_kaBandPressure = TestCase $ do
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt1 ss0
>   assertScoringResult' "test_kaBandPressure 1" Nothing (sqrt 5.3820267) (freqPressure undefined . head $ ss0)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt2 ss0
>   assertScoringResult' "test_kaBandPressure 2" Nothing (sqrt 3.7725887) (freqPressure undefined . head $ ss0)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt3 ss0
>   assertScoringResult' "test_kaBandPressure 3" Nothing (sqrt 2.5488133) (freqPressure undefined . head $ ss0)
>
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt4 ss1
>   assertScoringResult' "test_kaBandPressure 4" Nothing (sqrt 5.8121843) (freqPressure undefined . head $ ss1)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt5 ss1
>   assertScoringResult' "test_kaBandPressure 5" Nothing (sqrt 4.42589) (freqPressure undefined . head $ ss1)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt6 ss1
>   assertScoringResult' "test_kaBandPressure 6" Nothing (sqrt 2.9218125) (freqPressure undefined . head $ ss1)
>
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt7 ss2
>   assertScoringResult' "test_kaBandPressure 7" Nothing (sqrt 6.1059456) (freqPressure undefined . head $ ss2)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt8 ss2
>   assertScoringResult' "test_kaBandPressure 8" Nothing (sqrt 2.8478491) (freqPressure undefined . head $ ss2)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt9 ss2
>   assertScoringResult' "test_kaBandPressure 9" Nothing (sqrt 2.609438) (freqPressure undefined . head $ ss2)
>   freqPressure <- runScoring undefined [] undefined $ genFrequencyPressure dt10 ss2
>   assertScoringResult' "test_kaBandPressure 10" Nothing (sqrt 2.299283) (freqPressure undefined . head $ ss2)
>     where   
>       dt1  = fromGregorian 2006  2  1 0 0 0
>       dt2  = fromGregorian 2006  3 25 0 0 0
>       dt3  = fromGregorian 2006  4 11 0 0 0
>       dt4  = fromGregorian 2006  6  1 0 0 0
>       dt5  = fromGregorian 2006  6 13 0 0 0
>       dt6  = fromGregorian 2006  7 15 0 0 0
>       dt7  = fromGregorian 2006 10  1 0 0 0
>       dt8  = fromGregorian 2006 11 12 0 0 0
>       dt9  = fromGregorian 2006 12 11 0 0 0
>       dt10 = fromGregorian 2006 12 26 0 0 0
>       ss0 = [tom {authorized = True}
>            , dick {authorized = False}
>            , harry {authorized = False}
>             ]
>       ss1 = [tom {authorized = True}
>            , dick {authorized = True}
>            , harry {authorized = False}
>             ]
>       ss2 = [tom {authorized = True}
>            , dick {authorized = True}
>            , harry {authorized = True}
>             ]
>       tom = defaultSession {
>               sName = "Tom"
>             , periods = [
>                   defaultPeriod {
>                       startTime = fromGregorian 2006 3 24 0 0 0
>                     , pState = Scheduled
>                     , duration = 5*60
>                     , pDuration = 5*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 4 10 0 0 0
>                     , pState = Scheduled
>                     , duration = 12*60
>                     , pDuration = 12*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 6 12 0 0 0
>                     , pState = Scheduled
>                     , duration = 4*60
>                     , pDuration = 4*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 11 11 0 0 0
>                     , pState = Scheduled
>                     , duration = 1*60
>                     , pDuration = 1*60
>                      }
>                         ]
>             , sAllottedT = 80*60
>             , sAllottedS = 80*60
>             , frequency = 30.0
>             , band = A
>                          }
>       dick = defaultSession {
>               sName = "Dick"
>             , periods = [
>                   defaultPeriod {
>                       startTime = fromGregorian 2006 7 14 0 0 0
>                     , pState = Scheduled
>                     , duration = 14*60
>                     , pDuration = 14*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 11 11 0 0 0
>                     , pState = Scheduled
>                     , duration = 15*60
>                     , pDuration = 15*60
>                      }
>                         ]
>             , sAllottedT = 60*60
>             , sAllottedS = 60*60
>             , frequency = 30.0
>             , band = A
>                          }
>       harry = defaultSession {
>               sName = "Harry"
>             , periods = [
>                   defaultPeriod {
>                       startTime = fromGregorian 2006 11 11 0 0 0
>                     , pState = Scheduled
>                     , duration = 10*60
>                     , pDuration = 10*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 12 10 0 0 0
>                     , pState = Scheduled
>                     , duration = 7*60
>                     , pDuration = 7*60
>                      }
>                 , defaultPeriod {
>                       startTime = fromGregorian 2006 12 25 0 0 0
>                     , pState = Scheduled
>                     , duration = 12*60
>                     , pDuration = 12*60
>                      }
>                         ]
>             , sAllottedT = 60*60
>             , sAllottedS = 60*60
>             , frequency = 30.0
>             , band = A
>                          }

> test_rightAscensionPressure = TestCase $ do
>     raPressure <- runScoring undefined [] undefined $ genRightAscensionPressure defaultStartTime pSessions
>     assertScoringResult' "test_rightAscensionPressure" Nothing 1.5259848 (raPressure undefined . head $ pSessions)

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
>     expected  = [(0,0),(1920,840),(0,0),(1080,480)
>                 ,(600,300),(0,0),(0,0)
>                 ,(0,0),(0,0),(0,0)]
>     result    = elems $ initBins startTime (minBound, maxBound) band pSessions
>     startTime = fromGregorian' 2008 1 15

> test_residue = TestCase $ do
>   -- determined by all periods and total allocation
>   assertEqual "test_residue 0" (60*60) (residue tenA1 sess1)
>   assertEqual "test_residue 1" (60*60) (residue tenB1 sess1)
>   assertEqual "test_residue 2" (35*60) (residue tenB2 sess1)
>   assertEqual "test_residue 3" (10*60) (residue tenB4 sess1)
>   -- determined by this semester's periods and semester allocation
>   assertEqual "test_residue 0" (10*60) (residue tenA1 sess2)
>   assertEqual "test_residue 1" (50*60) (residue tenB1 sess2)
>   assertEqual "test_residue 2" (25*60) (residue tenB2 sess2)
>   assertEqual "test_residue 3" ( 0*60) (residue tenB4 sess2)
>     where
>       -- 40-hour period at
>       tenA0 = fromGregorian 2006 5  5 0 0 0
>       tenA1 = fromGregorian 2006 5 15 0 0 0
>       -- semesters 10A/10B boundary
>       tenB0 = fromGregorian 2006 6  3 0 0 0
>       -- 25-hour period at
>       tenB1 = fromGregorian 2006 6  5 0 0 0
>       tenB2 = fromGregorian 2006 6 15 0 0 0
>       -- 25-hour period at
>       tenB3 = fromGregorian 2006 7  5 0 0 0
>       tenB4 = fromGregorian 2006 7 15 0 0 0
>       sess1 = defaultSession {
>           periods = [
>               defaultPeriod {
>                   startTime = tenA0
>                 , pState = Scheduled
>                 , duration = 40*60
>                 , pDuration = 40*60
>                  }
>             , defaultPeriod {
>                   startTime = tenB1
>                 , pState = Scheduled
>                 , duration = 25*60
>                 , pDuration = 25*60
>                  }
>             , defaultPeriod {
>                   startTime = tenB3
>                 , pState = Scheduled
>                 , duration = 25*60
>                 , pDuration = 25*60
>                  }
>             ]
>         , sAllottedT = 100*60
>         , sAllottedS = 100*60
>                              }
>       sess2 = defaultSession {
>           periods = [
>               defaultPeriod {
>                   startTime = tenA0
>                 , pState = Scheduled
>                 , duration = 40*60
>                 , pDuration = 40*60
>                  }
>             , defaultPeriod {
>                   startTime = tenB1
>                 , pState = Scheduled
>                   , duration = 25*60
>                 , pDuration = 25*60
>                  }
>             , defaultPeriod {
>                   startTime = tenB3
>                 , pState = Scheduled
>                 , duration = 25*60
>                 , pDuration = 25*60
>                  }
>             ]
>         , sAllottedT = 100*60
>         , sAllottedS = 50*60
>                              }

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
>     assertEqual "test_getReceivers 1" [Rcvr4_6, Rcvr12_18] result1
>     assertEqual "test_getReceivers 2" [Rcvr1_2, Rcvr12_18] result2
>       where 
>         result1 = getReceivers (fromGregorian 2006 6 24 16 0 0) rSched
>         result2 = getReceivers (fromGregorian 2006 6 22 16 0 0) rSched

Equation 5

> test_zenithAngle' = TestCase $ do
>     assertEqual "test_zenithAngle' 1" 0.6707847  (zenithAngle' 0.0   0.0)
>     assertEqual "test_zenithAngle' 2" 0.11421602 (zenithAngle' 0.785 0.0)
>     assertEqual "test_zenithAngle' 3" 0.8992154  (zenithAngle' 1.57  0.0)
>     assertEqual "test_zenithAngle' 4" 0.58927834 (zenithAngle' 0.785 0.785)
>     assertEqual "test_zenithAngle' 5" 0.9000113  (zenithAngle' 1.57  1.57)
>     assertEqual "test_zenithAngle' 6" 1.1154156  (zenithAngle' 0.785 1.57)
>     assertEqual "test_zenithAngle' 7" 0.8994485  (zenithAngle' 1.57  0.785)
>     assertEqual "test_zenithAngle' 8" 0.9834893  (zenithAngle' 0.0   0.785)
>     assertEqual "test_zenithAngle' 9" 1.5701725  (zenithAngle' 0.0   1.57)

> test_zenithAngle = TestCase $ do
>     let dt = fromGregorian 2006 10 15 12 0 0 
>     let sess = findPSessionByName "LP"
>     let result = zenithAngle dt sess
>     assertEqual "test_zenithAngle 1" 1.1118553 result 
>     let result = zenithAngle dt sessBug
>     assertEqual "test_zenithAngle 2" 0.7069927 result 
>     let result = zenithAngle dt sessBug2
>     assertEqual "test_zenithAngle 3"  1.422472 result 

> test_zenithAngle2 = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 8 0 0
>     let dt1 = fromGregorian 2006 10 15 11 0 0
>     let sLP = findPSessionByName "LP" 
>     let za = zenithAngle dt1 sLP
>     assertEqual "test_zenithAngle2" 1.3142384 za 

> test_zenithAngleAtTransit = TestCase $ do
>     let sess = findPSessionByName "LP"
>     let result = zenithAngleAtTransit sess
>     assertEqual "test_zenithAngleAtTransit 1" 0.5765369 result 
>     let result = zenithAngleAtTransit sessBug
>     assertEqual "test_zenithAngleAtTransit 2" 0.54078466 result 
>     let result = zenithAngleAtTransit sessBug2
>     assertEqual "test_zenithAngleAtTransit 3" 0.7788097 result 

> test_minTsysPrime = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 9 15 2
>     -- session LP
>     let sess = findPSessionByName "LP"
>     Just result <- minTSysPrime w (frequency sess) (elevation dt sess) (r sess)
>     assertEqual "test_minTsysPrime 1" 17.102324 result 
>     -- session AS
>     let sess = findPSessionByName "AS"
>     Just result <- minTSysPrime w (frequency sess) (elevation dt sess) (r sess)

>     assertEqual "test_minTsysPrime 2" 29.415066 result 
>     -- sessBug
>     Just result <- minTSysPrime w (frequency sessBug) (elevation dt sessBug) (r sessBug)
>     assertEqual "test_minTsysPrime 3" 97.92687 result 
>     -- sessBug2
>     Just result <- minTSysPrime w (frequency sessBug2) (elevation dt sessBug2) (r sessBug2)
>     assertEqual "test_minTsysPrime 4" 27.511566  result 
>       where 
>         dt = fromGregorian 2006 10 15 12 0 0
>         r s = fromJust $ getPrimaryReceiver s

> test_systemNoiseTemperature = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 9 15 2
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 10 15 12 0 0
>     -- session LP
>     let sess = findPSessionByName "LP"
>     Just result <- systemNoiseTemperature w rt dt sess
>     assertEqual "test_systemNoiseTemperature 1" 16.881832 result 
>     Just result <- systemNoiseTemperaturePrime w rt dt sess
>     assertEqual "test_systemNoiseTemperature' 2" 17.197424 result 
>     -- session AS
>     let sess = findPSessionByName "AS"
>     Just result <- systemNoiseTemperature w rt dt sess
>     assertEqual "test_systemNoiseTemperature 3" 28.790943 result 
>     Just result <- systemNoiseTemperaturePrime w rt dt sess
>     assertEqual "test_systemNoiseTemperature' 4" 29.945415 result 

> test_minTsys' = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 9 15 2
>     let dt = fromGregorian 2006 10 15 12 0 0
>     -- session LP
>     let sess = findPSessionByName "LP"
>     Just result <- minTsys' w dt sess
>     assertEqual "test_minTsys' 1" 17.102324 result 
>     -- session AS
>     let sess = findPSessionByName "AS"
>     Just result <- minTsys' w dt sess
>     assertEqual "test_minTsys' 2" 29.415066 result 
>     -- sessBug
>     Just result <- minTsys' w dt sessBug
>     assertEqual "test_minTsys' 3" 97.92687 result 
>     -- sessBug2
>     Just result <- minTsys' w dt sessBug2
>     assertEqual "test_minTsys' 4" 27.511566 result 

> test_getRealOrForecastedWind = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 13 1 0 0
>     rt <- getReceiverTemperatures
>     -- forecast because in future
>     let dt1 = fromGregorian 2006 10 13 16 0 0
>     r1 <- runScoring w [] rt (getRealOrForecastedWind dt1)
>     assertEqual "test_getRealOrForecastedWind 1" (Just 6.5983596) r1
>     -- measured because in past
>     let dt2 = fromGregorian 2006 9 13 0 0 0
>     r2 <- runScoring w [] rt (getRealOrForecastedWind dt2)
>     assertEqual "test_getRealOrForecastedWind 2" (Just 4.073865) r2
>     -- forecast because measured is unavailable
>     let dt3 = fromGregorian 2006 6 22 12 0 0
>     r3 <- runScoring w [] rt (getRealOrForecastedWind dt3)
>     assertEqual "test_getRealOrForecastedWind 3" (Just 3.8565624) r3

Here we change the origin of the weather and watch it's affects on the
results of MOC (we also test VLBI sessions).  To make sure the correct
weather (gbt or forecasted) is being used:
   * comment out asserts
   * test only one session
   * use printouts in Weather.lhs

> test_minimumObservingConditions = TestCase $ do
>     let dt = fromGregorian 2006 10 13 16 0 0
>     w <- getWeatherTest . Just $ dt
>     rt <- getReceiverTemperatures
>     -- compute moc w/ origin of weather == start of period (gives W2 winds)
>     mocs <- mapM (moc w rt dt dur) sess
>     assertEqual "test_minimumObservingConditions_1" expected mocs
>     -- now see how setting the weather origin differently makes *NO* difference
>     -- now set origin of weather to be one hour before periods
>     --print "2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
>     w <- getWeatherTest . Just $ addMinutes' (-60) dt
>     mocs <- mapM (moc w rt dt dur) sess
>     assertEqual "test_minimumObservingConditions_2" exp2 mocs
>     -- now set origin of weather to be one hour after periods
>     --print "3!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
>     w <- getWeatherTest . Just $ addMinutes' 60 dt
>     mocs <- mapM (moc w rt dt dur) sess
>     assertEqual "test_minimumObservingConditions_3" expected mocs
>     -- now set the origina of weather way in the past
>     --print "4!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
>     w <- getWeatherTest . Just $ addMinutes' (-60*24*2) dt
>     mocs <- mapM (moc w rt dt dur) sess
>     assertEqual "test_minimumObservingConditions_4" exp2 mocs
>     -- back to weather dt == start of period, but w/ 45 min periods
>     -- see how the result doesn't change
>     --print "4_1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
>     w <- getWeatherTest . Just $ dt
>     mocs <- mapM (moc w rt dt 45) sess
>     assertEqual "test_minimumObservingConditions_4_1" expected mocs
>     -- back to weather dt == start of period, but w/ VLBI sessions
>     --print "5!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
>     w <- getWeatherTest . Just $ dt
>     mocs <- mapM (moc w rt dt 45) vlbis
>     assertEqual "test_minimumObservingConditions_5" exp2 mocs
>     -- now watch how making the period to short for VLBI is handled
>     --print "6!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
>     mocResult <- runScoring w [] rt $ minimumObservingConditions dt 30 (head vlbis)
>     assertEqual "test_minimumObservingConditions_6" Nothing mocResult
>   where
>     moc w rt dt dur s = do
>       Just result <- runScoring w [] rt (minimumObservingConditions dt dur s)
>       return result
>     names = ["GB","CV","LP","TX","VA","WV","AS"]
>     sess = concatMap (\name -> findPSessionsByName name) names
>     dur = 30
>     vlbis = map (\s -> s {oType = Vlbi}) sess
>     expected = [False,True,True,False,False,False,True]
>     exp2     = [False,True,True,True,True,False,True]



> test_goodElective = TestCase $ do
>   w <- getWeatherTest . Just $ fromGregorian 2006 2 1 0 0 0
>   let rs = []
>   rt <- getReceiverTemperatures
>   result <- mapM (goodElective' w rs rt) ps
>   assertEqual "test_goodElective_1" exp result
>   -- move the third period from False to True by making it scheduled
>   let scheduledPeriod = (mkPeriod es1 dt 60 3) { pState = Scheduled }
>   result <- mapM (goodElective' w rs rt) [scheduledPeriod]
>   assertEqual "test_goodElective_2" [True] result
>   -- move the period #5 from True to False by making it NOT gauranteed
>   let es2' = es1 { guaranteed = False }
>   let ps' = [mkPeriod es2' dt 60 5]
>   result <- mapM (goodElective' w rs rt) ps'
>   assertEqual "test_goodElective_3" [False] result
>     where
>   exp = [True, True, False, True, True, True]
>   mkPeriod s dt dur id = defaultPeriod { session   = s
>                                        , startTime = dt
>                                        , duration  = dur
>                                        , peId      = id
>                                        }
>   gb = head $ findPSessionsByName "GB"
>   cv = head $ findPSessionsByName "CV"
>   e1 = Electives 1 True [3, 5] 
>   e2 = Electives 2 True [4, 6] 
>   es1 = gb { sType = Elective, electives = [e1], sId = 100 }
>   es2 = cv { sType = Elective, electives = [e2], sId = 101}
>   -- use the date & sessions from test_minimumObservingConditions
>   -- to get predictable results
>   dt = fromGregorian 2006 10 13 16 0 0
>   ps = [mkPeriod gb dt 60 1
>       , mkPeriod cv dt 60 2
>       , mkPeriod es1 dt 60 3 -- 1st in elective
>       , mkPeriod es2 dt 60 4 -- 1st
>       , mkPeriod es1 dt 60 5 -- last in elective
>       , mkPeriod es2 dt 60 6 -- last
>        ]
>   goodElective' w rs rt p = runScoring w rs rt $ goodElective p
>   

> test_goodDefaultPeriod = TestCase $ do
>   w <- getWeatherTest . Just $ fromGregorian 2006 2 1 0 0 0
>   let rs = []
>   rt <- getReceiverTemperatures
>   -- easy case - all open sessions
>   result <- mapM (goodDefaultPeriod' w rs rt) ps1
>   assertEqual "test_goodDefaultPeriod_1" [True, True] result
>   -- now do it again, but with a guaranteed session
>   result <- mapM (goodDefaultPeriod' w rs rt) ps2
>   assertEqual "test_goodDefaultPeriod_2" [True, True, True] result
>   -- now try a non-guaranteed session, and see what happens
>   result <- mapM (goodDefaultPeriod' w rs rt) ps3
>   assertEqual "test_goodDefaultPeriod_3" [True, True, True, False] result
>     where
>   mkPeriod s dt dur id = defaultPeriod { session   = s
>                                        , startTime = dt
>                                        , duration  = dur
>                                        , peId      = id
>                                        }
>   gb = head $ findPSessionsByName "GB"
>   cv = head $ findPSessionsByName "CV"
>   goodDefaultPeriod' w rs rt p = runScoring w rs rt $ goodDefaultPeriod p
>   dt = fromGregorian 2006 10 13 16 0 0
>   ps1 = [mkPeriod gb dt 60 1, mkPeriod cv dt 60 2]
>   ranges = [(fromGregorian' 2006 10 22, fromGregorian' 2006 10 27)]
>   dpId = 100
>   dpId2 = 200
>   win = defaultWindow { wRanges = ranges, wPeriodId = Just dpId, wTotalTime = 60 }
>   ws = gb { sType = Windowed, windows = [win], sId = 200, guaranteed = True }
>   defaultPd = mkPeriod ws dt 60 dpId
>   ps2 = ps1 ++ [defaultPd]
>   ws2 = gb { sType = Windowed, windows = [win], sId = 300, guaranteed = False }
>   defaultPd2 = mkPeriod ws2 dt 60 dpId2
>   ps3 = ps2 ++ [defaultPd2]
>   

> test_isLastPeriodOfElective = TestCase $ do
>     assertEqual "test_isLastPeriodOfElective_1" False (isLastPeriodOfElective p1) 
>     assertEqual "test_isLastPeriodOfElective_2" True (isLastPeriodOfElective p2) 
>     assertEqual "test_isLastPeriodOfElective_3" False (isLastPeriodOfElective p3) 
>     assertEqual "test_isLastPeriodOfElective_4" True (isLastPeriodOfElective p4) 
>     assertEqual "test_isLastPeriodOfElective_5" False (isLastPeriodOfElective p5) 
>   where
>     -- order of period Ids is assumed to be by ASC startTime (DSSData)
>     e1 = Electives 1 False [100, 101] 
>     e2 = Electives 2 False [102, 103] 
>     s' = defaultSession { sType = Elective
>                         , electives = [e1, e2] }
>     p1 = defaultPeriod { session = s', peId = 100 }
>     p2 = defaultPeriod { session = s', peId = 101 }
>     p3 = defaultPeriod { session = s', peId = 102 }
>     p4 = defaultPeriod { session = s', peId = 103 }
>     p5 = defaultPeriod { session = s', peId = 105 }
>     s  = makeSession s' [] [p1, p2, p3, p4] -- not used, WTF

> test_observingEfficiency = TestCase $ do
>     -- pTestProjects session CV
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     fs <- runScoring w [] rt (observingEfficiency dt s)
>     let result = eval fs
>     assertEqual "test_observingEfficiency" 0.9285591 result

> test_minObservingEfficiencyFactor = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 8 0 0
>     rt <- getReceiverTemperatures
>     fs <- runScoring w [] rt (observingEfficiency dt s1)
>     assertEqual "test_minObservingEfficiencyFactor 1" 0.45012027 (eval fs)
>     fs <- runScoring w [] rt (atmosphericEfficiency dt s1)
>     assertEqual "test_minObservingEfficiencyFactor 2" 0.4698731 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiencyLimit dt s1)
>     assertEqual "test_minObservingEfficiencyFactor 3" 1.0551452e-19 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt s2)
>     assertEqual "test_minObservingEfficiencyFactor 4" 0.45012027 (eval fs)
>     fs <- runScoring w [] rt (atmosphericEfficiency dt s2)
>     assertEqual "test_minObservingEfficiencyFactor 5" 0.4698731 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiencyLimit dt s2)
>     assertEqual "test_minObservingEfficiencyFactor 6" 1.0551452e-19 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt s3)
>     assertEqual "test_minObservingEfficiencyFactor 7" 0.7033129 (eval fs)
>     fs <- runScoring w [] rt (atmosphericEfficiency dt s3)
>     assertEqual "test_minObservingEfficiencyFactor 8" 0.73417664 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiencyLimit dt s3)
>     assertEqual "test_minObservingEfficiencyFactor 9" 1.0 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt s4)
>     assertEqual "test_minObservingEfficiencyFactor 10" 0.9579614 (eval fs)
>     fs <- runScoring w [] rt (atmosphericEfficiency dt s4)
>     assertEqual "test_minObservingEfficiencyFactor 11" 1.0 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiencyLimit dt s4)
>     assertEqual "test_minObservingEfficiencyFactor 12" 1.0 (eval fs)
>     where
>      dt = fromGregorian 2006 10 15 12 0 0
>      s1 = defaultSession {sAllottedT = 24*60, minDuration = 2*60
>                         , maxDuration = 6*60, frequency = 16.9
>                         , dec = 0.71, band = K
>                         , receivers = [[Rcvr12_18]]}
>      s2 = s1 {xi = 1.0}
>      s3 = s1 {xi = 1.25}
>      s4 = s1 {xi = 2.0}

> test_observingEfficiency2 = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 8 0 0
>     rt <- getReceiverTemperatures
>     let dt1 = fromGregorian 2006 10 15 12 0 0 -- sunup  
>     let dt2 = fromGregorian 2006 10 15 11 0 0 -- sundown
>     let sLP = findPSessionByName "LP" 
>     let sGB = findPSessionByName "GB" 
>     fs <- runScoring w [] rt (observingEfficiency dt1 sLP)
>     assertEqual "test_observingEfficiency2_1" 0.9846228 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt2 sLP)
>     assertEqual "test_observingEfficiency2_2" 0.98363626 (eval fs)
>     fs <- runScoring w [] rt (observingEfficiency dt1 sGB)
>     assertEqual "test_observingEfficiency2_3" 0.77861434 (eval fs)

> test_observingEfficiencyLimit = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     [(_, Just result)] <- runScoring w [] rt (observingEfficiencyLimit dt s)
>     assertEqual "test_observingEfficiencyLimit <18" 0.8904976 result
>     let s = findPSessionByName "GB"
>     [(_, Just result)] <- runScoring w [] rt (observingEfficiencyLimit dt s)
>     assertEqual "test_observingEfficiencyLimit >=18" 0.0 result

Equation 24

> test_observingEfficiencyLimit' = TestCase $ do
>     assertEqual "test_oel_1" 3.0780464e-4 (oel 0.8577623 0.93819135 4.3)
>     assertEqual "test_oel_2" 0.0 (oel 0.105431244 0.52246356 27.5)
>   where
>     oel = observingEfficiencyLimit'

> test_efficiency = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 2
>     let dt = fromGregorian 2006 10 15 12 0 0
>     let sess = findPSessionByName "LP"
>     assertResult' "test_efficiency 1" (Just wdt) 0.9889707 (efficiency dt sess)
>     assertResult' "test_efficiencyHA 2" (Just wdt) 0.73852307 (efficiencyHA dt sess) 
>     let sess = findPSessionByName "WV"
>     assertResult' "test_efficiency 3" (Just wdt) 0.9164896 (efficiency dt sess) 
>     assertResult' "test_efficiencyHA 4" (Just wdt) 0.6651696 (efficiencyHA dt sess) 
>     let sess = findPSessionByName "AS"
>     assertResult' "test_efficiency 5" (Just wdt) 0.9648925 (efficiency dt sess) 
>     assertResult' "test_efficiencyHA 6" (Just wdt) 0.496098 (efficiencyHA dt sess)
>     assertResult' "test_efficiency 7" (Just wdt) 0.32182154 (efficiency dt sessBug)
>     assertResult' "test_efficiency 8" (Just wdt) 0.9464483 (efficiency dt sessBug2) 
>     -- pTestProjects session CV
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     Just result <- runScoring w [] rt (efficiency dt s) 
>     assertEqual "test_efficiency 9" 0.9357649 result
>     Just result <- runScoring w [] rt (efficiencyHA dt s) 
>     assertEqual "test_efficiencyHA 10" 0.86103255 result

> test_tSysPrime  = TestCase $ do
>     let dt = fromGregorian 2006 10 15 12 0 0
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 12 15 0
>     rt <- getReceiverTemperatures
>     res <- runScoring w [] rt (tSysPrime w rt Rcvr_RRI 0.8 (deg2rad 35.0) dt)
>     let exp = Just 313.11163
>     assertEqual "test_tSysPrime 1" exp res
>     res <- runScoring w [] rt (tSysPrime w rt Rcvr1_2 1.2 (deg2rad 45.0) dt)
>     let exp = Just 30.867044
>     assertEqual "test_tSysPrime 2" exp res
>     res <- runScoring w [] rt (tSysPrime w rt Rcvr2_3 2.2 (deg2rad 65.0) dt)
>     let exp = Just 16.452452
>     assertEqual "test_tSysPrime 3" exp res
>     res <- runScoring w [] rt (tSysPrime w rt Rcvr26_40 22.2 (deg2rad 70.0) dt)
>     let exp = Just 48.491142
>     assertEqual "test_tSysPrime 4" exp res

Equation 4

> test_atmosphericOpacity = TestCase $ do
>     assertEqual "test_atmosphericOpacity 1" 8.1687495e-3 (atmosphericOpacity 8.124e-3 0.1)
>     assertEqual "test_atmosphericOpacity 2" 8.525134e-2 (atmosphericOpacity 6.434e-2 0.707)
>     assertEqual "test_atmosphericOpacity 3" 0.3369283 (atmosphericOpacity 9.287e-2 1.3)
>     let x = atmosphericOpacity 7.527e-3 1.5
>     let y = atmosphericOpacity 7.527e-3 1.6
>     assertEqual "test_atmosphericOpacity 4" x y

> test_tSysPrime'  = TestCase $ do
>     let res = tSysPrime' 31.3 244.0 2.1 (deg2rad 47.0)
>     let exp = 5865.138
>     assertEqual "test_tSysPrime' 1" exp res
>     let res = tSysPrime' 3.1 297.1 0.01 (deg2rad 37.0)
>     let exp = 12.6543665
>     assertEqual "test_tSysPrime' 2" exp res

Equation 7

> test_systemNoiseTemperature' = TestCase $ do
>     assertEqual "test_snt_1" 15.348079 (snt  5.0 257.49832 1.8215785e-2)
>     assertEqual "test_snt_3" 25.468143 (snt  10.0 256.9823 3.8752194e-2)
>   where
>     snt  = systemNoiseTemperature'

> test_zenithOpacity' = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 2
>     assertResult' "test_zenithOpacity' 1" (Just wdt) 7.500619e-3 (zenithOpacity' dtLP 0.3)
>     assertResult' "test_zenithOpacity' 2" (Just wdt) 8.3219055e-3 (zenithOpacity' dtLP 5.8)
>     assertResult' "test_zenithOpacity' 3" (Just wdt) 8.119332e-3 (zenithOpacity' dtLP 5.4)
>     assertResult' "test_zenithOpacity' 4" (Just wdt) 4.8523504e-2 (zenithOpacity' dtLP 22.6)
>     assertResult' "test_zenithOpacity' 5" (Just wdt) 0.5769451 (zenithOpacity' dtLP 66.6)

> test_zenithOpacity = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 2
>     let sess = findPSessionByName "LP"
>     assertResult' "test_zenithOpacity 1" (Just wdt) 8.119332e-3 (zenithOpacity dtLP sess)
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertResult' "test_zenithOpacity 1" (Just wdt) 6.637155e-2 (zenithOpacity dt sessBug)
>     assertResult' "test_zenithOpacity 1" (Just wdt) 7.527518e-3 (zenithOpacity dt sessBug2)

> test_zenithOpacity2 = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 8 0 0
>     rt <- getReceiverTemperatures
>     let dt1 = fromGregorian 2006 10 15 11 0 0
>     let sLP = findPSessionByName "LP" 
>     Just zod <- runScoring w [] rt (zenithOpacity dt1 sLP)
>     assertEqual "test_zenithOpacity2" 8.1259385e-3 zod 

> test_zenithOpacityDryAir = TestCase $ do
>     assertEqual "test_zenithOpacityDryAir 1" (Just 7.48875e-3) (zenithOpacityDryAir (Just 0.007) 0.3)
>     assertEqual "test_zenithOpacityDryAir 2" (Just 2.4220312) (zenithOpacityDryAir (Just 4.3) 1.5)
>     assertEqual "test_zenithOpacityDryAir 3" (Just 0.16798124) (zenithOpacityDryAir (Just 0.8) 0.9)

> test_positionValues = TestCase $ do
>     assertEqual "test_positionValues hourAngle" 1.0507135 (hourAngle dt lp)
>     assertEqual "test_epositionValues elevation" 0.46234667 (elevation dt lp)
>   where
>     dt = fromGregorian 2009 12 9 16 24 0
>     ss = concatMap sessions pTestProjects
>     lp = head $ filter (\s -> "LP" == (sName s)) ss

Equation 22

> test_avgObservingEff = TestCase $ do
>     assertEqual "test_avgObservingEffLo 1" 0.9690155 (avgObservingEffLo 3.0)
>     assertEqual "test_avgObservingEffLo 2" 0.7445902 (avgObservingEffLo 15.0)
>     assertEqual "test_avgObservingEffLo 3" 0.581184 (avgObservingEffLo 22.0)
>     assertEqual "test_avgObservingEffLo 4" 0.65445566 (avgObservingEffLo 48.0)
>     assertEqual "test_avgObservingEffHi 1" 0.48187032 (avgObservingEffHi 63.0)
>     assertEqual "test_avgObservingEffHi 2" 0.48187032 (avgObservingEffHi 75.0)
>     assertEqual "test_avgObservingEffHi 3" 0.48187032 (avgObservingEffHi 82.0)
>     assertEqual "test_avgObservingEffHi 4" 0.48187032 (avgObservingEffHi 116.0)
>     assertEqual "test_avgObservingEff 1" (avgObservingEff 22.0) (avgObservingEffLo 22.0)
>     assertEqual "test_avgObservingEff 2" (avgObservingEff 82.0) (avgObservingEffHi 82.0)

Equation 23

> test_minObservingEff = TestCase $ do
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     let result = minObservingEff . frequency $ s
>     assertEqual "test_minObservingEff" 0.93819135 result

> test_kineticTemperature = TestCase $ do
>     let wdt = fromGregorian 2006 10 14 9 15 0
>     let sess = findPSessionByName "LP"
>     assertResult' "test_kineticTemperatureLP" (Just wdt) 257.57712 (kineticTemperature dtLP sess) 
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertResult' "test_kineticTemperatureBug" (Just wdt) 257.0764 (kineticTemperature dt sessBug2) 
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = findPSessionByName "CV"
>     Just result <- runScoring w [] rt (kineticTemperature dt s) 
>     assertEqual "test_kineticTemperatureCV" 270.8776 result

> test_kineticTemperature2 = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 14 8 0 0
>     rt <- getReceiverTemperatures
>     let dt1 = fromGregorian 2006 10 15 11 0 0
>     let sLP = findPSessionByName "LP" 
>     Just kt <- runScoring w [] rt (kineticTemperature dt1 sLP)
>     assertEqual "test_kineticTemperature2" 257.526 kt 

> test_stringency = TestCase $ do
>     let dt = fromGregorian 2006 10 15 18 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult' "test_stringency 1" Nothing 1.2835165 (stringency dt sess)
>     let sess = findPSessionByName "AS"
>     assertScoringResult' "test_stringency 2" Nothing 1.0 (stringency dt sess)
>     -- test stringencies outside the receiver's frequency range
>     w <- getWeatherTest . Just $ dt 
>     rt <- getReceiverTemperatures
>     let cv = findPSessionByName "CV"
>     let sess = cv { frequency = 3.0 }
>     [(_, Just low)] <- runScoring w rSched rt (stringency dt sess)
>     let sess = cv { frequency = 7.0 }
>     [(_, Just high)] <- runScoring w rSched rt (stringency dt sess)
>     let sess = cv { frequency = 0.5 }
>     assertScoringResult' "test_stringency 3" Nothing low (stringency dt sess)
>     let sess = cv { frequency = 30.0 }
>     assertScoringResult' "test_stringency 4" Nothing high (stringency dt sess)
>     let sess = findPSessionByName "AS"
>     assertScoringResult' "test_stringency 5" Nothing 1.0 (stringency dt sess)

> makeTestProject :: Minutes -> Minutes -> Project
> makeTestProject tl tt = makeProject proj' tt tt ss'
>   where
>     proj' = defaultProject { pName = "time use test" }
>     ss''  = [
>         defaultSession {
>             periods = [defaultPeriod {duration = tt - tl, pDuration = tt - tl}]
>           , sAllottedT = tt
>           , receivers = [[Rcvr1_2]] 
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
>           , receivers = [[Rcvr1_2]] 
>           }
>       ]
>     ss'   = [ makeSession s [] (periods s) | s <- ss'' ]


> test_projectCompletion = TestCase $ do
>     let dt = fromGregorian 2006 10 15 18 0 0 -- don't need!
>     -- adjust the project's times to get desired results
>     let p = makeTestProject 28740 33812
>     let sess = findPSessionByName "LP"
>     let s = sess {project = p}
>     assertScoringResult' "test_projectCompletion" Nothing 1.0150006 (projectCompletion dt s)

> test_politicalFactors = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 13 22 0 0
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 10 15 12 0 0
>     let s = head . filter (\s -> "CV" == (sName s)) . concatMap sessions $ pTestProjects
>     -- missing window, transit, observerOnSite, and ObserverAvailable
>     let politicalFactors = score [scienceGrade
>                                 , thesisProject
>                                 , projectCompletion]
>     fs <- runScoring w [] rt (politicalFactors dt s)
>     let result = eval fs
>     assertEqual "test_politicalFactors" 1.0024 result

Equation 14

> test_halfPwrBeamWidth = TestCase $ do
>     assertEqual "test_halfPwrBeamWidth" 33.035713 (halfPwrBeamWidth 22.4)

> test_calculateTE = TestCase $ do
>     assertEqual "test_calculateTE 1" 1.4436142    (calculateTE  0.4)
>     assertEqual "test_calculateTE 2" 9.983187     (calculateTE  1.8)
>     assertEqual "test_calculateTE 3" 19.742699    (calculateTE  2.6)

Equation 12

> test_trackingObservingEfficiency = TestCase $ do
>     assertEqual "test_trackingObservingEfficiency 1" (Just 0.99909395)  (trackingObservingEfficiency wind1 dt1 False freq1)
>     assertEqual "test_trackingObservingEfficiency 2" (Just 0.99999285)  (trackingObservingEfficiency wind1 dt1 True freq1)
>     assertEqual "test_trackingObservingEfficiency 3" (Just 0.9980345)  (trackingObservingEfficiency wind2 dt2 False freq2)
>     assertEqual "test_trackingObservingEfficiency 4" (Just 0.99860287)  (trackingObservingEfficiency wind2 dt2 True freq2)
>      where
>        freq1 = 5.4
>        freq2 = 4.3
>        dt1 = fromGregorian 2006 10 15 12 0 0
>        dt2 = fromGregorian 2006 9 2 14 30 0
>        wind1 = Just 1.2388499
>        wind2 = Just 5.2077017

> test_trackingEfficiency = TestCase $ do
>     let sess = findPSessionByName "LP"
>     let dt = fromGregorian 2006 10 15 12 0 0
>     assertScoringResult' "test_trackingEfficiency lp" Nothing 0.9986691 (trackingEfficiency dt sess)
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     [(_, Just result)] <- runScoring w [] rt (trackingEfficiency dt s)
>     assertEqual "test_trackingEfficiency cv" 0.9940742 result 

Equation 13

> test_trackErr = TestCase $ do
>     assertEqual "test_trackErr 1" 2.4107518e-2 (trackErr dt1 wind1 freq1)
>     assertEqual "test_trackErr 2" 2.4898745e-2 (trackErr dt2 wind2 freq2)
>      where
>        freq1 = 5.4
>        freq2 = 4.3
>        dt1 = fromGregorian 2006 10 15 12 0 0
>        dt2 = fromGregorian 2006 9 2 14 30 0
>        wind1 = 1.2388499
>        wind2 = 5.2077017

Equation 16

> test_trackErrArray = TestCase $ do
>     assertEqual "test_trackErrArray 1" 8.829199e-3 (trackErrArray wind1 freq1)
>     assertEqual "test_trackErrArray 2" 1.7345414e-2 (trackErrArray wind2 freq2)
>      where
>        freq1 = 5.4
>        freq2 = 4.3
>        wind1 = 1.2388499
>        wind2 = 5.2077017

> test_trackingErrorLimit = TestCase $ do
>     let dt = fromGregorian 2006 10 15 12 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult' "test_trackingErrorLimit" Nothing 1.0 (trackingErrorLimit dt sess)
>     -- pTestProjects session CV
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getReceiverTemperatures
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
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     factors <- subfactorFactors s w rt dt
>     let sysNoiseTemp = fromJust . fromJust . lookup "sysNoiseTemp" $ factors
>     assertEqual "test_subfactorFactors sysNoiseTemp" 23.39033 sysNoiseTemp
>     let sysNoiseTempPrime = fromJust . fromJust . lookup "sysNoiseTempPrime" $ factors
>     assertEqual "test_subfactorFactors sysNoiseTempPrime" 23.740269 sysNoiseTempPrime
>     let minSysNoiseTempPrime = fromJust . fromJust . lookup "minSysNoiseTempPrime" $ factors
>     assertEqual "test_subfactorFactors minSysNoiseTempPrime" 22.965136 minSysNoiseTempPrime

> test_weatherFactors = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     factors <- weatherFactors s w dt
>     let wind_mph = fromJust . fromJust . lookup "wind_mph" $ factors
>     assertEqual "test_weatherFactors wind_mph" 12.485167 wind_mph
>     --
>     let wind_ms = fromJust . fromJust . lookup "wind_ms" $ factors
>     assertEqual "test_weatherFactors wind_ms" 7.294977 wind_ms
>     let opacity = fromJust . fromJust . lookup "opacity" $ factors
>     assertEqual "test_weatherFactors opacity" 8.936982e-3 opacity
>     let tsys = fromJust . fromJust . lookup "tsys" $ factors
>     assertEqual "test_weatherFactors tsys" 270.8776 tsys

> test_scoreFactors = TestCase $ do
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let s = findPSessionByName "CV"
>     let dur = 15::Minutes
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 2 14 30 0 -- pick earlier
>     factors <- scoreFactors s w pSessions dt dur []
>     assertEqual "test_scoreFactors 1" 21 (length . head $ factors)
>     mapM_ (assertFactor factors) exp 
>   where
>     lookup' factors name = fromJust . fromJust . lookup name . head $ factors
>     assertFactor factors (key, value) = assertEqual ("test_scoreFactors " ++ key) value (lookup' factors key)
>     exp = [("stringency",1.0954108)
>           ,("atmosphericEfficiency",0.9439829)
>           ,("surfaceObservingEfficiency",0.9982148)
>           ,("trackingEfficiency",0.99917835)
>           ,("rightAscensionPressure",1.0) 
>           ,("frequencyPressure",1.9724026)
>           ,("observingEfficiencyLimit",1.0)
>           ,("hourAngleLimit",1.0)
>           ,("zenithAngleLimit",1.0)
>           ,("trackingErrorLimit",1.0)
>           ,("atmosphericStabilityLimit",1.0)
>           ,("scienceGrade",1.0)
>           ,("thesisProject",1.0)
>           ,("projectCompletion",1.0024)
>           ,("observerOnSite",1.0)
>           ,("receiver",1.0)
>           ,("needsLowRFI",1.0)
>           ,("lstExcepted",1.0)
>           ,("observerAvailable",1.0)
>           ,("projectBlackout",1.0)
>           ,("inWindows",1.0)]

> test_availWindows = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 20 1 0 0
>     rt <- getReceiverTemperatures
>     let s = findPSessionByName "TestWindowed1"
>     let results = availWindows s
>     -- should never return any window which is complete
>     assertBool "test_availWindows_1" (not . or . map wComplete $ results)
>     let s = findPSessionByName "TestWindowed2"
>     let results = availWindows s
>     -- should never return any window which is complete
>     assertBool "test_availWindows_2" (not . or . map wComplete $ results)

> test_inWindows = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 20 1 0 0
>     rt <- getReceiverTemperatures
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
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 2 14 30 0 -- pick earlier
>     rt <- getReceiverTemperatures
>     factors <- scoreElements s w rt pSessions dt dur []
>     assertEqual "test_scoreElements 1" 31 (length . head $ factors)
>     let haLimit = fromJust . fromJust . lookup "hourAngleLimit" . head $ factors
>     assertEqual "test_scoreElements 2" 1.0 haLimit
>     let fPress = fromJust . fromJust . lookup "frequencyPressure" . head $ factors
>     assertEqual "test_scoreElements 3" 1.9724026 fPress
>     let opacity = fromJust . fromJust . lookup "opacity" . head $ factors
>     assertEqual "test_scoreElements 4" 8.786574e-3 opacity
>     let elevation = fromJust . fromJust . lookup "elevation" . head $ factors
>     assertEqual "test_scoreElements 5" 36.60029 elevation
>     let sysNoiseTemp = fromJust . fromJust . lookup "sysNoiseTemp" . head $ factors
>     assertEqual "test_scoreElements 6" 23.294113 sysNoiseTemp

> test_zenithAngleLimit = TestCase $ do
>     let dt = fromGregorian 2006 10 15 0 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult' "test_zenithAngleLimit" Nothing 0.0 (zenithAngleLimit dt sess)

Equation 11

> test_rmsTrackingError = TestCase $ do
>     let dt  = fromGregorian 2006 4 15 16 0 0
>     let res = rmsTrackingError dt 3.4
>     let exp = 3.499613 
>     assertEqual "test_rmsTrackingError 1" exp res
>     let res = rmsTrackingError dt 17.2
>     let exp = 29.997143 
>     assertEqual "test_rmsTrackingError 2" exp res
>     let res = rmsTrackingError dt 0.1
>     let exp = 3.3
>     assertEqual "test_rmsTrackingError 3" exp res
>     let dt  = fromGregorian 2006 4 16 4 0 0
>     let res = rmsTrackingError dt 3.4
>     let exp = 3.0327039 
>     assertEqual "test_rmsTrackingError 4" exp res
>     let res = rmsTrackingError dt 17.2
>     let exp = 29.946262 
>     assertEqual "test_rmsTrackingError 5" exp res
>     let res = rmsTrackingError dt 0.1
>     let exp = 2.8000002
>     assertEqual "test_rmsTrackingError 6" exp res

Equation 15

> test_variableTrackingError = TestCase $ do
>     let res = variableTrackingError 3.4
>     let exp = 1.6725109
>     assertEqual "test_variableTrackingError 1" exp res
>     let res = variableTrackingError 17.2
>     let exp = 29.839212
>     assertEqual "test_variableTrackingError 2" exp res
>     let res = variableTrackingError 0.1
>     let exp = 1.2000005
>     assertEqual "test_variableTrackingError 3" exp res

Equation 9

> test_surfaceObservingEfficiency' = TestCase $ do
>     assertEqual "test_surfaceObservingEfficiency' 1" 0.99913067 (surfaceObservingEfficiency' dt1 3.0)
>     assertEqual "test_surfaceObservingEfficiency' 2" 0.9784915 (surfaceObservingEfficiency' dt1 15.0)
>     assertEqual "test_surfaceObservingEfficiency' 3" 0.95430493 (surfaceObservingEfficiency' dt1 22.0)
>     assertEqual "test_surfaceObservingEfficiency' 4" 0.8003946 (surfaceObservingEfficiency' dt1 48.0)
>     assertEqual "test_surfaceObservingEfficiency' 5" 1.0 (surfaceObservingEfficiency' dt2 48.0)
>       where
>         dt1 = fromGregorian 2006 7 4 0 0 0          -- warming time
>         dt2 = fromGregorian 2006 7 4 9 0 0          -- not warming time

> test_surfaceObservingEfficiency = TestCase $ do
>     let dt  = fromGregorian 2006 4 15 16 0 0
>     let wdt = Just $ fromGregorian 2006 4 15 0 0 0
>     let sess = findPSessionByName "LP"
>     assertScoringResult' "test_surfaceObservingEfficienyLP" wdt 0.99718606 (surfaceObservingEfficiency dt sess)
>     let sess = findPSessionByName "WV"
>     assertScoringResult' "test_surfaceObservingEfficienyWV" wdt 0.888959 (surfaceObservingEfficiency dt sess)

> test_scoreCV = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] rt $ genScore dt ss >>= \f -> f dt s
>     let result = eval fs
>     assertEqual "test_scoreCV" 3.4976518 result  

New tests that do *not* match up to a 'beta test python code test', but rather
to use in conjunction with Pack tests.

> test_scoreCV2 = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 10 1 18 0 0
>     rt <- getReceiverTemperatures
>     -- make sure that we don't use real wind!
>     let dt = fromGregorian 2006 10 1 18 32 0
>     let ss = concatMap sessions pTestProjects
>     let s = head $ filter (\s -> "CV" == (sName s)) ss
>     fs <- runScoring w [] rt $ genScore dt ss >>= \f -> f dt s
>     let result = eval fs
>     assertEqual "test_scoreCV2" 4.1611986 result  

> test_scoreForTime = TestCase $ do
>     -- score on top of weather
>     w <- getWeatherTest $ Just dt
>     rt <- getReceiverTemperatures
>     fs <- runScoring w [] rt $ do
>         sf <- genScore dt ss
>         sf dt s
>     let w1Score = eval fs
>     -- use different forecast; should get different score
>     w <- getWeatherTest $ Just dt2
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
>     rt <- getReceiverTemperatures
>     w <- getWeatherTest $ Just dt
>     fs <- runScoring w [] rt $ do
>         sf <- genScore dt ss
>         sf dt s
>     let w1Score = eval fs
>     -- use different forecast; should get different score
>     w <- getWeatherTest $ Just dt2
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
>     assertEqual "test_avgScoreForTime_2" w1Score w3Score
>   where
>     dt = fromGregorian 2006 10 1 18 0 0
>     dt2 = fromGregorian 2006 10 1 0 0 0
>     ss = getOpenPSessions
>     s = findPSessionByName "CV"
> 

> test_avgScoreForTime2 = TestCase $ do
>     -- weather that shouldn't get used
>     rt <- getReceiverTemperatures
>     w <- getWeatherTest $ Just dummytime
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
>     s = defaultSession {frequency = 1.5
>                       , sAllottedT = 24*60
>                       , minDuration=2*60
>                       , maxDuration=6*60
>                       , receivers = [[Rcvr1_2]]}
> 

> test_weightedMeanScore = TestCase $ do
>     assertEqual "test_weightedMeanScore 0" 0.0 (weightedMeanScore SpectralLine [])
>     assertEqual "test_weightedMeanScore 1" 0.0 (weightedMeanScore Continuum [17.0])
>     assertEqual "test_weightedMeanScore 2" 6.5 (weightedMeanScore Radar [17.0, 13.0])
>     assertEqual "test_weightedMeanScore 3" 8.0 (weightedMeanScore SpectralLine[17.0, 13.0, 11.0])
>     assertEqual "test_weightedMeanScore 4" 7.75 (weightedMeanScore Continuum [17.0, 13.0, 11.0, 7.0])
>     assertEqual "test_weightedMeanScore 4" 4.5 (weightedMeanScore Vlbi [17.0, 13.0, 11.0, 7.0])

Test the 24-hour scoring profile of the default session, per quarter.

> test_score = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getReceiverTemperatures
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
>                           , receivers = [[Rcvr1_2]]
>                           }
>     expected = (replicate 40 0.0) ++ defaultScores ++ (replicate 23 0.0)

> test_score_maintenance = TestCase $ do
>     w <- getWeatherTest . Just $ fromGregorian 2006 9 1 1 0 0
>     rt <- getReceiverTemperatures
>     let dt = fromGregorian 2006 9 2 14 30 0
>     let ss = concatMap sessions pTestProjects
>     fs <- runScoring w [] rt $ genScore dt ss >>= \f -> f dt sess
>     let result = eval fs
>     assertEqual "test_score_maintenance" 0.0 result  
>   where
>     sess = defaultSession { sName = "Shutdown"
>                           , sAllottedT = 24*60
>                           , minDuration = 2*60
>                           , maxDuration = 6*60
>                           , frequency = 0.0
>                           , ra = 1.3962634016
>                           , dec = 7.2722052166400002e-06
>                           , receivers = [[]]
>                           , oType = Maintenance
>                           }

> test_score_window = TestCase $ do
>     w <- getWeatherTest . Just $ (-60) `addMinutes` starttime 
>     rt <- getReceiverTemperatures
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
>     expected = [2.3009086,2.2980537,2.2946627,2.292648,2.2878835] ++ (replicate 92 0.0)

For defaultSession w/ sAllottedT = 24*60; start time is  2006 11 8 12 0 0
plus 40 quarters.

> defaultScores = [0.5058345,0.5073376,0.38199204,0.44000292,0.46694967,0.49243346,0.5100889,0.51120037,0.5118567,0.5124549,0.5183046,0.5184878,0.518829,0.518989,0.5247254,0.5247254,0.52481264,0.52481264,0.52643895,0.52643895,0.52636725,0.5262142,0.5256728,0.5254833,0.5253835,0.52516615,0.52396524,0.5236562,0.52331305,0.522728,0.5231732,0.52246106,0.5216063,0.52056277]

> test_bestDuration = TestCase $ do
>     w <- getWeatherTest . Just $ origin 
>     rt <- getReceiverTemperatures
>     -- best period length using session's min/max
>     bestDur <- runScoring w [] rt $ do
>         sf <- genScore starttime ss
>         bestDuration sf starttime Nothing Nothing s
>     let expected = (s, 4.0253725, 8*60)
>     assertEqual "test_bestDuration 1" expected bestDur
>     -- best period length overriding min/max
>     bestDur <- runScoring w [] rt $ do
>         sf <- genScore starttime ss
>         bestDuration sf starttime (Just (1*60+45::Minutes)) (Just (4*60::Minutes)) s
>     let expected = (s, 3.884861, 4*60)
>     assertEqual "test_bestDuration 2" expected bestDur
>     -- best period length using session's min/max, but only 4 hours left
>     bestDur <- runScoring w [] rt $ do
>         sf <- genScore starttime ss
>         bestDuration sf starttime Nothing Nothing exht
>     let expected = (exht, 3.884861, 4*60)
>     assertEqual "test_bestDuration 3" expected bestDur
>   where
>     origin = fromGregorian 2006 10 1 10 30 0
>     starttime = fromGregorian 2006 10 1 11 0 0
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
>     w <- getWeatherTest . Just $ (-60) `addMinutes` starttime
>     rt <- getReceiverTemperatures
>     let ss = concatMap sessions pTestProjects
>     bestDurs <- runScoring w [] rt $ do
>         sf <- genScore starttime ss
>         bestDurations sf starttime Nothing Nothing ss
>     assertEqual "test_bestDurations 1" 12 (length bestDurs)
>     let (s, v, d) = bestDurs !! 1
>     assertEqual "test_bestDurations 2 n" "CV" (sName s)
>     assertEqual "test_bestDurations 2 v" 3.9800308 v
>     assertEqual "test_bestDurations 2 d" 360 d
>     let (s, v, d) = bestDurs !! 6
>     assertEqual "test_bestDurations 3 n" "AS" (sName s)
>     assertEqual "test_bestDurations 3 v" 3.3165581 v
>     assertEqual "test_bestDurations 3 d" 450 d
>     -- now test it the way it gets used with Nominees:
>     -- reuse the same weather that is an hour earlier ... 
>     bestDurs <- runScoring w [] rt $ do
>         sf <- genPartScore starttime [] $ scoringSessions starttime undefined ss
>         bestDurations sf starttime Nothing Nothing ss
>     -- ... and nothing changes
>     assertEqual "test_bestDurations 1" 12 (length bestDurs)
>     let (s, v, d) = bestDurs !! 1
>     assertEqual "test_bestDurations 2 n" "CV" (sName s)
>     assertEqual "test_bestDurations 2 v" 3.9800308 v
>     assertEqual "test_bestDurations 2 d" 360 d
>     let (s, v, d) = bestDurs !! 6
>     assertEqual "test_bestDurations 3 n" "AS" (sName s)
>     assertEqual "test_bestDurations 3 v" 3.3165581 v
>     assertEqual "test_bestDurations 3 d" 450 d  

>   where
>     starttime = fromGregorian 2006 10 1 17 0 0

> test_averageScore = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getReceiverTemperatures
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
>                           , receivers = [[Rcvr12_18]]
>                           , frequency = 12.8
>                           }
>     times = [(15*q) `addMinutes'` starttime | q <- [0..96]]

Look at the scores over a range where none are zero.

> test_averageScore2 = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getReceiverTemperatures
>     (scoreTotal, scoreTotal', avgScore) <- runScoring w [] rt $ do
>         sf <- genScore starttime [sess]
>         scores <- lift $ mapM (score' w rt sf) times
>         let scoreTotal = addScores scores
>         scoreTotal' <- totalScore sf dt dur sess
>         avgScore <- averageScore sf dt sess
>         return (scoreTotal, scoreTotal', avgScore)
>     assertEqual "test_averageScore2_addScores" expectedTotal scoreTotal
>     assertEqual "test_averageScore2_totalScore" expectedTotal scoreTotal'
>     assertEqual "test_averageScore2_avgScore" expectedAvg avgScore
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     dur = 2*60
>     sess = defaultSession { sAllottedT = 24*60 
>                           , minDuration = dur 
>                           , maxDuration = 6*60
>                           , frequency = 2.0
>                           , receivers = [[Rcvr1_2]] 
>                           }
>     score' w rt sf dt = do
>         fs <- runScoring w [] rt (sf dt sess)
>         return $ eval fs
>     dt = (40*quarter) `addMinutes'` starttime -- start where scores /= 0
>     numQtrs = dur `div` quarter
>     times = [(q*quarter) `addMinutes'` dt | q <- [0..numQtrs-1]]
>     expectedTotal = 3.8158395 
>     expectedAvg = expectedTotal / (fromIntegral numQtrs)

> test_averageScore' = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getReceiverTemperatures
>     let score' w dt = runScoring w [] rt $ do
>         fs <- genScore dt [sess]
>         s <- fs dt sess
>         return $ eval s
>     scores <- mapM (score' w) times
>     let scoreTotal = addScores scores
>     let expected = 0.0
>     assertEqual "test_averageScore'_1" expected scoreTotal
>     avgScore <- runScoring w [] rt $ do
>         fs <- genScore starttime [sess]
>         averageScore' fs starttime dur sess
>     assertEqual "test_averageScore'_2" expected avgScore
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     dur = 2*60
>     sess = defaultSession { sAllottedT = 24*60 
>                           , minDuration = dur 
>                           , maxDuration = 6*60
>                           , receivers = [[Rcvr12_18]]
>                           , frequency = 12.8
>                           }
>     times = [(15*q) `addMinutes'` starttime | q <- [0..96]]

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
>   w <- getWeatherTest Nothing
>   rt <- getReceiverTemperatures
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
>                            , pstId = 3113
>                            , sanctioned = False
>                            , reservations = []
>                            , blackouts = bs}
>       p   = defaultProject { observers = [o] }
>       s   = defaultSession { project = p}
>       expFalse = 0.0

Like test_obsAvailbe, but with required friends

> test_obsAvailable4 = TestCase $ do
>   assertEqual "test_obsAvailable4_1" True  (obsAvailable dt s)
>   assertEqual "test_obsAvailable4_2" False (obsAvailable dt s2)
>   assertEqual "test_obsAvailable4_3" True  (obsAvailable dt2 s2)
>   assertEqual "test_obsAvailable4_4" False (obsAvailable dt3 s2)
>     where
>       dt  = fromGregorian 2006 2 1 0 0 0
>       dt2 = fromGregorian 2006 2 7 0 0 0
>       dt3 = fromGregorian 2006 2 3 3 0 0
>       s   = defaultSession
>       s2  = defaultSession { project = p }
>       p   = defaultProject { observers = [o], requiredFriends = [f1, f2] }
>       o   = defaultObserver { blackouts = bs }
>       bs  = [(fromGregorian 2006 1 31 0 0 0, fromGregorian 2006 2 2 0 0 0)]
>       f1 = defaultObserver {blackouts = fbs1}
>       fbs1 = [(fromGregorian 2006 2 3 0 0 0, fromGregorian 2006 2 4 0 0 0)]
>       f2 = defaultObserver

> test_observerAvailable = TestCase $ do
>   w <- getWeatherTest Nothing
>   rt <- getReceiverTemperatures
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

> test_projectBlackout = TestCase $ do
>   w <- getWeatherTest Nothing
>   rt <- getReceiverTemperatures
>   -- no blackouts
>   fs <- runScoring w [] rt (projectBlackout dt s)
>   assertEqual "test_projectBlackout_1" expTrue (eval fs)
>   -- blackouts, dt in range
>   fs <- runScoring w [] rt (projectBlackout dt s2)
>   assertEqual "test_projectBlackout_2" expFalse (eval fs)
>   -- blackouts, dt out of range
>   fs <- runScoring w [] rt (projectBlackout dt2 s2)
>   assertEqual "test_projectBlackout_3" expTrue (eval fs)
>   -- more blackouts, dt in range
>   fs <- runScoring w [] rt (projectBlackout dt s3)
>   assertEqual "test_projectBlackout_4" expFalse (eval fs)
>   -- more blackouts, dt out of range
>   fs <- runScoring w [] rt (projectBlackout dt2 s3)
>   assertEqual "test_projectBlackout_5" expTrue (eval fs)
>   -- more blackouts, dt in range
>   fs <- runScoring w [] rt (projectBlackout dt3 s3)
>   assertEqual "test_projectBlackout_6" expFalse (eval fs)
>     where
>       dt  = fromGregorian 2006 2 1  0 0 0
>       dt2 = fromGregorian 2006 2 7  0 0 0
>       dt3 = fromGregorian 2006 2 11 0 0 0
>       s   = defaultSession
>       p   = defaultProject { pBlackouts = bs }
>       s2  = defaultSession { project = p }
>       --o   = defaultObserver { blackouts = bs }
>       bs  = [(fromGregorian 2006 1 31 0 0 0, fromGregorian 2006 2 2 0 0 0)]
>       bs2 = [(fromGregorian 2006 2 10 0 0 0, fromGregorian 2006 2 12 0 0 0)]
>       --o2  = defaultObserver { blackouts = bs ++ bs2 }
>       p2  = defaultProject { pBlackouts = bs ++ bs2 }
>       s3  = defaultSession { project = p2}
>       expTrue = 1.0
>       expFalse = 0.0


> test_needsLowRFI = TestCase $ do
>   w <- getWeatherTest Nothing
>   rt <- getReceiverTemperatures
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
>   w <- getWeatherTest Nothing
>   rt <- getReceiverTemperatures
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
>   w <- getWeatherTest Nothing
>   rt <- getReceiverTemperatures
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

Assumes the Rcvr getting boosted is Rcvr_1070.

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

> test_scorePeriodOverhead = TestCase $ do
>   -- do explicitly what scorePeriod is supposed to do
>   w <- getWeatherTest $ Just startDt
>   rt <- getReceiverTemperatures
>   scores <- mapM (scoreSession s' w rt) dts
>   let scores_0 = take 4 . tail . tail $ scores
>   let scores_1 = take 4 . tail $ scores
>   let scores_2 = take 4 scores
>   let avg_score_0 = (sum . tail $ scores_0) / 4.0
>   let avg_score_1 = (sum . tail $ scores_1) / 4.0
>   let avg_score_2 = (sum . tail .tail $ scores_2) / 4.0
>   -- to verify the initial quarter(s) in test data are zero
>   assertEqual "test_scorePeriodOverhead_1" es scores
>   -- now test if overheads for periods are being handled correctly
>   periodScore <- scorePeriod p0 s_sl ss w [] rt
>   assertEqual "test_scorePeriodOverhead_2" avg_score_0 periodScore
>   periodScore <- scorePeriod p1 s_sl ss w [] rt
>   assertEqual "test_scorePeriodOverhead_3" avg_score_1 periodScore
>   periodScore <- scorePeriod p2 s_ov ss w [] rt
>   assertEqual "test_scorePeriodOverhead_4" avg_score_2 periodScore
>   where
>     startDt = fromGregorian 2006 2 1 20 45 0
>     es = [0.0,0.0,1.6101582,1.6151073,1.6185511,1.621222]
>     scoreSession s w rt dt = do
>       fs <- runScoring w [] rt $ genScore dt ss >>= \f -> f dt s
>       return $ eval fs
>     ss = pSessions
>     s' = head ss
>     s_sl = s' { oType = SpectralLine }
>     s_ov = s' { oType = Vlbi }
>     mins = [0, 15 .. 75]
>     dts = map (\m -> addMinutes m startDt) mins
>     -- create periods that covers the time ranges
>     p0 = defaultPeriod { session = s_sl
>                        , startTime = 30 `addMinutes` startDt
>                        , duration = 60
>                        , pForecast = startDt
>                        }
>     p1 = defaultPeriod { session = s_sl
>                        , startTime = 15 `addMinutes` startDt
>                        , duration = 60
>                        , pForecast = startDt
>                        }
>     p2 = defaultPeriod { session = s_ov
>                        , startTime = startDt
>                        , duration = 60
>                        , pForecast = startDt
>                        }

> test_scorePeriod = TestCase $ do
>   -- do explicitly what scorePeriod is supposed to do
>   w <- getWeatherTest $ Just startDt
>   rt <- getReceiverTemperatures
>   scores <- mapM (scoreSession w rt) dts
>   let weightedAvgScore = (sum . tail $ scores) / 4.0
>   -- now 
>   periodScore <- scorePeriod p s ss w [] rt
>   assertEqual "test_scorePeriod_1" weightedAvgScore periodScore
>   where
>     startDt = fromGregorian 2006 2 1 0 30 0
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

> test_scoreVlbiPeriod = TestCase $ do
>   -- do explicitly what scorePeriod is supposed to do
>   w <- getWeatherTest $ Just startDt
>   rt <- getReceiverTemperatures
>   scores <- mapM (scoreSession w rt) dts
>   let weightedAvgScore = (sum . tail . tail $ scores) / 4.0
>   -- now 
>   periodScore <- scorePeriod p s ss w [] rt
>   assertEqual "test_scorePeriod_1" weightedAvgScore periodScore
>   where
>     startDt = fromGregorian 2006 2 1 0 30 0
>     scoreSession w rt dt = do
>       fs <- runScoring w [] rt $ genScore dt ss >>= \f -> f dt s
>       return $ eval fs
>     ss = pSessions
>     s = (head ss) {oType = Vlbi}
>     -- do this explicitly to avoid mistakes
>     mins = [0, 15, 30, 45] -- 60 minutes!
>     dts = map (\m -> addMinutes m startDt) mins
>     -- create a period that covers this same time range
>     p = defaultPeriod { session = s
>                       , startTime = startDt
>                       , duration = 60
>                       , pForecast = startDt
>                       }

> test_elevationLimit = TestCase $ do
>   assertEqual "test_elevationLimit_1" True (elevationLimit' dt s1)
>   assertEqual "test_elevationLimit_2" True (elevationLimit' dt s2)
>   assertEqual "test_elevationLimit_3" False (elevationLimit' dt s3)
>   -- now make sure we override hour angle limit properly
>   rt <- getReceiverTemperatures
>   w <- getWeatherTest $ Just dt
>   fs <- runScoring w [] rt (hourAngleLimit dt s1)
>   assertEqual "test_elevationLimit_4" 1.0 (eval fs)
>   fs <- runScoring w [] rt (hourAngleLimit dt s2)
>   assertEqual "test_elevationLimit_5" 1.0 (eval fs)
>   fs <- runScoring w [] rt (hourAngleLimit dt s3)
>   assertEqual "test_elevationLimit_6" 0.0 (eval fs)
>     where
>   dt = fromGregorian 2006 3 1 0 0 0
>   s1 = defaultSession { receivers = [[Rcvr1_2]]
>                       , frequency = 1.9
>                       , dec = 1.5 } -- always up
>   s2 = defaultSession { receivers = [[Rcvr1_2]]
>                       , dec = 1.5 -- always up
>                       , elLimit = Just . deg2rad $ 10.0  } 
>   s3 = defaultSession { receivers = [[Rcvr1_2]]
>                       , dec = 1.5 -- always up
>                       , elLimit = Just . deg2rad $ 70.0  } 


> test_atmosphericStability = TestCase $ do
>     w <- getWeatherTest $ Just dt
>     rt <- getReceiverTemperatures
>     fs <- runScoring w [] rt $ atmosphericStabilityLimit dt s1  
>     assertEqual "test_atmosphericStability_1" 1.0 (eval fs)
>     fs <- runScoring w [] rt $ atmosphericStabilityLimit dt s2  
>     assertEqual "test_atmosphericStability_2" 1.0 (eval fs)
>   where
>     s1 = defaultSession { dec = 1.5, oType = Continuum } -- always up
>     dt = fromGregorian 2006 3 1 0 0 0
>     s2 = defaultSession { dec = 1.5, oType = SpectralLine } -- always up

> test_calculateAtmStabilityLimit = TestCase $ do
>     assertEqual "test_calculateAtmStabilityLimit 1" (Just False) (calculateAtmStabilityLimit (Just 330) Continuum 2.1)
>     assertEqual "test_calculateAtmStabilityLimit 2" (Just True) (calculateAtmStabilityLimit (Just 329) Continuum 2.1)
>     assertEqual "test_calculateAtmStabilityLimit 3" (Just True) (calculateAtmStabilityLimit (Just 330) SpectralLine 2.1)
>     assertEqual "test_calculateAtmStabilityLimit 4" (Just True) (calculateAtmStabilityLimit (Just 330) Continuum 1.9)

> test_efficiency_below2GHz = TestCase $ do
>     w <- getWeatherTest . Just $ wdt 
>     rt <- getReceiverTemperatures
>     Just result <- runScoring w [] rt (efficiency dt s1)  --fromJust error
>     assertEqual "test_efficiency_below2GHz_1" 0.97584516 result
>     Just result <- runScoring w [] rt (efficiency dt s2) 
>     assertEqual "test_efficiency_below2GHz_2" 0.9816546 result
>     Just result <- runScoring w [] rt (efficiency dt s3) 
>     assertEqual "test_efficiency_below2GHz_3" 0.9699389 result
>   where
>     wdt = fromGregorian 2006 9 1 1 0 0
>     dt = fromGregorian 2006 9 2 14 30 0
>     s1 = defaultSession { receivers = [[Rcvr1_2]], dec = 1.5, frequency = 2.0 } 
>     s2 = defaultSession { receivers = [[Rcvr1_2]], dec = 1.5, frequency = 1.0 } 
>     s3 = defaultSession { receivers = [[Rcvr_342]], dec = 1.5, frequency = 0.5 } 

Test utilities

> assertAlmostEqual :: String -> Int -> Float -> Float -> IO ()
> assertAlmostEqual name places expected value =
>     assertBool name $ abs (value - expected) < epsilon
>   where
>     epsilon = 1.0 / 10.0 ** fromIntegral places

> assertScoringResult :: String -> Maybe DateTime -> Int -> Float -> Scoring Factors -> IO ()
> assertScoringResult name dt  digits expected scoref = do
>     w <- getTestWeather dt  
>     rt <- getReceiverTemperatures
>     [(_, Just result)] <- runScoring w rSched rt scoref
>     assertAlmostEqual name digits expected result

> assertScoringResult' :: String -> Maybe DateTime -> Float -> Scoring Factors -> IO ()
> assertScoringResult' name dt expected scoref = do
>     w <- getTestWeather dt 
>     rt <- getReceiverTemperatures
>     [(_, Just result)] <- runScoring w rSched rt scoref
>     assertEqual name expected result

> assertResult :: String -> Maybe DateTime -> Int -> Float -> Scoring (Maybe Float) -> IO ()
> assertResult name dt digits expected scoref = do
>     w <- getTestWeather dt
>     rt <- getReceiverTemperatures
>     Just result <- runScoring w rSched rt scoref
>     assertAlmostEqual name digits expected result

> assertResult' :: String -> Maybe DateTime -> Float -> Scoring (Maybe Float) -> IO ()
> assertResult' name dt expected scoref = do
>     w <- getTestWeather dt
>     rt <- getReceiverTemperatures
>     Just result <- runScoring w rSched rt scoref
>     assertEqual name expected result


> getTestWeather :: Maybe DateTime -> IO Weather
> getTestWeather dt | isJust dt == False = getWeatherTest . Just $ fromGregorian 2006 10 13 22 0 0
>                   | isJust dt = getWeatherTest dt

Test data generation

These are sessions that exposed bugs from the QuickCheck properties.

> bugSessions = zipWith5 genBugSessions names ras decs freqs rcvrs 
>   where names  = ["bug1",   "bug2"]
>         ras    = [ 2.67,  0.873562]
>         decs   = [ 0.13, -0.108025]
>         freqs  = [39.76,       2.0]
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
>           , receivers = [[Rcvr1_2]] 
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
>           , receivers = [[Rcvr1_2]] 
>         }

> rSched = [ (fromGregorian 2006 6 14 12 0 0, [Rcvr1_2, Rcvr26_40])
>          , (fromGregorian 2006 6 21 12 0 0, [Rcvr1_2, Rcvr12_18])
>          , (fromGregorian 2006 6 24 16 0 0, [Rcvr4_6, Rcvr12_18])
>          , (fromGregorian 2006 7  1 12 0 0, [Rcvr1_2, Rcvr4_6])
>          ]
