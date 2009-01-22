> module Antioch.Schedule.PackTests where

> import Antioch.Schedule.Pack
> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Weather
> import Test.HUnit
> import Control.Monad.Reader

> tests = TestList [
>     test_NumSteps
>   , test_Unwind1
>   , test_Unwind2
>   , test_Candidates1
>   , test_Candidates2
>   , test_Best
>   , test_Madd1
>   , test_Madd2
>   , test_Pack1
>   , test_PackWorker'1
>   , test_PackWorker'3
>   , test_PackWorker1
>   , test_PackWorker2
>   , test_PackWorker3
>   , test_PackWorker4
>   , test_RandomScore
>   , test_RandomScore2
>   , test_ToItem
>   , test_ToPeriod
>   ]

> test_NumSteps = TestCase . assertEqual "test_NumSteps" 192 . numSteps $ 48 * 60

> test_Unwind1 = TestCase . assertEqual "test_Unwind1" xs . unwind $ ys
>   where
>     xs = [Candidate 1 0 2 1.0, Candidate 2 2 2 1.0]
>     ys = [Just (Candidate 2 0 2 2.0), Nothing, Just (Candidate 1 0 2 1.0), Nothing, Nothing]

> test_Unwind2 = TestCase . assertEqual "test_Unwind2" xs . unwind $ ys
>   where
>     xs = [Candidate 1 0 1 1.0, Candidate 2 1 2 1.0, Candidate 3 3 3 1.0]
>     ys = [Just (Candidate 3 0 3 3.0), Nothing, Nothing, Just (Candidate 2 0 2 2.0), Nothing, Just (Candidate 1 0 1 1.0), Nothing]

> test_Candidates1 = TestCase . assertEqual "test_Candidates1" xs . candidates $ ys
>   where
>     xs = [Nothing, Just (Candidate 1 0 2 2.0), Just (Candidate 1 0 3 3.0), Just (Candidate 1 0 4 4.0)]
>     ys = Item 1 2 4 [] (replicate 6 1.0)

> test_Candidates2 = TestCase . assertEqual "test_Candidates2" xs . candidates $ ys
>   where
>     xs = []
>     ys = Item 1 2 4 [] (0.0 : replicate 5 1.0)

> test_Best = TestCase . assertEqual "test_Best" xs . best $ ys
>   where
>     xs = Just (Candidate 1 0 4 4.0)
>     ys = [Nothing, Nothing, Just (Candidate 1 0 3 3.0), Just (Candidate 1 0 4 4.0)]

> test_Madd1 = TestCase . assertEqual "test_Madd1" xs . best . zipWith madd ys $ zs
>   where
>     xs = Just (Candidate 1 0 4 4.0)
>     ys = [Nothing, Nothing, Just (Candidate 1 0 3 3.0), Just (Candidate 1 0 4 4.0)]
>     zs = replicate 4 Nothing

> test_Madd2 = TestCase . assertEqual "test_Madd2" xs . best . zipWith madd ys $ zs
>   where
>     xs = Nothing
>     ys = [Nothing, Nothing, Just (Candidate 1 0 3 3.0), Just (Candidate 1 0 4 4.0)]
>     zs = replicate 2 Nothing

> test_PackWorker'1 = TestCase . assertEqual "test_PackWorker'1" xs . packWorker' ys zs $ ws
>   where
>     xs = [Just (Candidate 1 0 4 4.0), Just (Candidate 1 0 3 3.0), Just (Candidate 1 0 2 2.0), Nothing, Nothing]
>     ys = replicate 4 Nothing
>     zs = [Nothing]
>     ws = map step [Item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker'3 = TestCase . assertEqual "test_PackWorker'3" xs . packWorker' ys zs $ ws
>   where
>     xs = [Just (Candidate 2 0 1 1.0), Just (Candidate 1 0 2 3.1), Nothing, Just (Candidate 3 0 1 1.1), Nothing]
>     ys = [Just (Candidate 3 0 1 1.1), Nothing, Nothing, Just (Candidate 2 0 1 1.0)]
>     zs = [Nothing]
>     ws = map step [Item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker1 = TestCase . assertEqual "test_PackWorker1" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 1 0 4 4.0]
>     ys = replicate 4 Nothing
>     ws = [Item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker2 = TestCase . assertEqual "test_PackWorker2" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 1 0 3 3.0, Candidate 2 3 1 1.0]
>     ys = replicate 3 Nothing ++ [Just (Candidate 2 0 1 4.0)]
>     ws = [Item 1 2 4 (replicate 6 1.0) []]

> test_PackWorker3 = TestCase . assertEqual "test_PackWorker3" xs . packWorker ys $ ws
>   where
>     xs = [Candidate 3 0 1 1.1, Candidate 1 1 2 1.9999999, Candidate 2 3 1 1.0]
>     ys = [Just (Candidate 3 0 1 1.1), Nothing, Nothing, Just (Candidate 2 0 1 4.1)]
>     ws = [Item 1 2 4 (replicate 6 1.0) []]

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
>     result = [ Candidate "A"  0 2 2.0
>              , Candidate "F1" 2 2 0.0
>              , Candidate "F2" 5 2 0.0
>              , Candidate "C"  7 2 2.0
>              , Candidate "D"  9 2 2.0
>              ]
>     fixed  = [ Nothing                        --  0
>              , Nothing                        --  1
>              , Just (Candidate "F1" 0 1 1.0)  --  2
>              , Just (Candidate "F1" 0 2 2.0)  --  3
>              , Nothing                        --  4
>              , Just (Candidate "F2" 0 1 1.0)  --  5
>              , Just (Candidate "F2" 0 2 2.0)  --  6
>              , Nothing                        --  7
>              , Nothing                        --  8
>              , Nothing                        --  9
>              , Nothing                        -- 10
>              ]
>     --                       0    1    2    3    4    5    6    7    8    9    10
>     open   = [ Item "A" 2 8 [1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 0.0, 0.0, 0.0] []
>              , Item "B" 2 8 [0.0, 0.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 0.0, 0.0, 0.0] []
>              , Item "C" 2 8 [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 0.5] []
>              , Item "D" 2 8 [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0] []
>              ]


> test_Pack1 = TestCase $ do
>     let periods = pack (fs candidate) starttime duration [] [candidate]
>     w <- getWeather . Just $ starttime 
>     periods' <- runScoring w [] $ periods
>     assertEqual "test_Pack1_1" 1 (length periods')
>     assertEqual "test_Pack1_2" expPeriod (head periods')
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     duration = 12*60
>     fs s = genScore [s]
>     candidate = defaultSession { sName = "singleton"
>                                , totalTime = 24*60
>                                , minDuration = 2*60
>                                , maxDuration = 6*60
>                                }
>     expStartTime = fromGregorian 2006 11 8 21 15 0
>     expPeriod = defaultPeriod { session = candidate
>                               , startTime = expStartTime
>                               , duration = 165
>                               , pScore = 34.97986
>                               }

> test_ToItem = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     -- create an item without a mask, i.e. no scoring
>     let item = toItem (fs session) [] session
>     item' <- runScoring w [] item
>     assertEqual "test_ToItem_1" result1 item'
>     assertEqual "test_ToItem_2" 0 (length . iFuture $ item')
>     -- now try it with the mask (dts)
>     let item = toItem (fs session) dts session
>     item' <- runScoring w [] item
>     assertEqual "test_ToItem_3" result2 item'
>     assertEqual "test_ToItem_4" 48 (length . iFuture $ item') 
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     duration = 12 * 60
>     fs s = genScore [s]
>     -- the 'mask' is just a list of datetimes to score at
>     dts' = quarterDateTimes starttime duration 
>     dts = [(Just dt) | dt <- dts']
>     session = defaultSession { sName = "singleton"
>                              , totalTime = 24*60
>                              , minDuration = 2*60
>                              , maxDuration = 6*60
>                              }
>     result1 = Item { iId = session
>                    , iMinDur = 8
>                    , iMaxDur = 24
>                    , iFuture = []
>                    , iPast = []
>                    }
>     -- this expected result for the scoring of the session in 15-min
>     -- increments starting at starttime is taken from the ScoreTests.lhs
>     futureResult = (replicate 37 0.0) ++ [ 3.2315328, 3.204887,  3.211515
>                                          , 3.219639,  3.2261572, 3.1090422
>                                          , 3.1223507, 3.133507,  3.1399984
>                                          , 3.1896782, 3.1915512
>                                          ]
>     result2 = result1 { iFuture = futureResult }

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
>     result = toPeriod dt c
>     expected = defaultPeriod { session = defaultSession
>                              , startTime = dt1
>                              , duration = quarter * 12
>                              , pScore = 20.0
>                              }

Test against python unit tests from beta test code:

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
>     [(_, Just result)] <- runScoring w [] (randomScoreFactor dt defaultSession)
>     assertEqual "test_RandomScore" hr1Score result
>     [(_, Just result)] <- runScoring w [] (randomScoreFactor dt1 defaultSession)
>     assertEqual "test_RandomScore" hr1Score result
>     [(_, Just result)] <- runScoring w [] (randomScoreFactor dt2 defaultSession)
>     assertEqual "test_RandomScore" hr2Score result
>     [(_, Just result)] <- runScoring w [] (randomScoreFactor dt3 defaultSession)
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
>     scores <- mapM (score' w) times
>     assertEqual "test_RandomScore2" expScores scores
>   where
>     dt = pythonTestStarttime
>     times = [(15*q) `addMinutes'` dt | q <- [0..23]]
>     score' w dt = do
>         [(_, Just result)] <- runScoring w [] (randomScoreFactor dt defaultSession)
>         return result
>     expScores = concat [(replicate 4 x) | x <- (take 6 randomList)]

The next three tests are packing a single session into a duration.  The main
difference between tests is the packing duration:

Here, packing duration (6 hrs) == session maxDur (6 hrs)

TBF: none of these results match the python results!

> test_TestPack_pack1 = TestCase $ do
>     let periods = pack randomScore starttime duration [] [testSession]
>     w <- getWeather Nothing
>     periods' <- runScoring w [] $ periods
>     putStrLn . show $ periods'
>     assertEqual "test_Pack1_2" [expPeriod] periods'
>   where
>     starttime = pythonTestStarttime --fromGregorian 2006 11 8 12 0 0
>     duration = 6*60
>     expScore = 5.542216 * (6*4) -- python: mean, here: sum
>     expPeriod = Period testSession starttime  (6*60) expScore

Here, packing duration (9 hrs) > session maxDur (6 hrs)

> test_TestPack_pack2 = TestCase $ do
>     let periods = pack randomScore starttime duration [] [testSession]
>     w <- getWeather Nothing
>     periods' <- runScoring w [] $ periods
>     putStrLn . show $ periods'
>     assertEqual "test_TestPack" expPeriods periods'
>   where
>     starttime = pythonTestStarttime
>     starttime2 = (3*60) `addMinutes'` pythonTestStarttime 
>     duration = 9*60
>     expScore1 = 5.761259 * (3 * 4) -- python: mean, here: sum
>     expScore2 = 5.128810 * (6 * 4) 
>     expPeriod1 = Period testSession starttime  (3*60) expScore1 
>     expPeriod2 = Period testSession starttime2 (6*60) expScore2
>     expPeriods = [expPeriod1, expPeriod2]

Here, packing duration (7 hrs) > session maxDur (6 hrs)

> test_TestPack_pack3 = TestCase $ do
>     let periods = pack randomScore starttime duration [] [testSession]
>     w <- getWeather Nothing
>     periods' <- runScoring w [] $ periods
>     putStrLn . show $ periods'
>     assertEqual "test_TestPack" expPeriods periods'
>   where
>     starttime = pythonTestStarttime
>     starttime2 = (2*60) `addMinutes'` pythonTestStarttime 
>     duration = 7*60
>     expScore1 = 4.8132718634 * (2 * 4) -- python: mean, here: sum
>     expScore2 = 5.2704045983 * (5 * 4) 
>     expPeriod1 = Period testSession starttime  (2*60) expScore1
>     expPeriod2 = Period testSession starttime2 (5*60) expScore2
>     expPeriods = [expPeriod1, expPeriod2]

Now, we change the test by packing using TWO sessions:

> test_TestPack_pack8 = TestCase $ do
>     let periods = pack randomScore starttime duration [] sessions 
>     w <- getWeather Nothing
>     periods' <- runScoring w [] $ periods
>     putStrLn . show $ periods'
>     assertEqual "test_TestPack_pack8" 3 (length periods')
>   where
>     starttime = pythonTestStarttime
>     --starttime2 = (2*60) `addMinutes'` pythonTestStarttime 
>     duration = 12*60
>     sessions = [testSession, testSession2]
>     --expPeriod1 = Period testSession starttime  (2*60) 5.54222
>     --expPeriod2 = Period testSession starttime2 (5*60) 5.54222
>     --expPeriods = [expPeriod1, expPeriod2]

Session data to pack:

> testSession  = defaultSession { sName = "singleton"
>                               , totalTime = 24*60
>                               , minDuration = 2*60
>                               , maxDuration = 6*60
>                              }

> testSession2 = defaultSession { sName = "second"
>                               , totalTime = 24*60
>                               , minDuration = 4*60
>                               , maxDuration = 8*60
>                               }

This is the list of random numbers generated on the python side:

> randomList :: [Score]
> randomList = [7.1331340485018409, 2.4934096782883213, 7.6572318406256947, 5.046714456152789, 6.8446511584066618, 4.0781524926716983, 2.7252730440470305, 4.9143871264557122, 7.1636843840447568, 6.9446361985339973, 4.8230123064175849, 3.4473390258899297, 6.3350439397544198, 2.8207298844712874, 5.1058061299127466, 2.4985974189931035, 7.7080423642050198, 7.158122187895521, 2.5448732889679264, 5.0495207342152231, 2.6078672746394629, 4.5245768464312714, 4.6630376376658127, 4.9814692299184458, 3.9230995086978351, 3.124772317749299, 4.3545291190078173, 3.9156803332050671, 4.7649071147900779, 3.2957866086525902, 2.5266837648837353, 4.1279381958049832, 2.846086056357267, 7.9501503718916222, 5.0040843232701224, 6.2997134589932822, 2.8066033004458157, 3.3695805540586292, 7.1911605255609041, 5.1902010664882869, 6.0641085042114264, 3.1763244030347106, 5.5648306304235842, 4.8999056732443051, 4.8385202083992347, 7.821359353269389, 6.8195409456787983, 6.5591857654180128, 6.0411887011958951, 7.3687373406644578, 3.925478958851746, 6.1593368290906056, 6.2553947135435362, 2.9056687203569784, 2.0240197872208707, 7.0209407927591698, 7.5301119472143458, 6.5565343260879541, 7.4360080633805605, 5.5085736431979573, 3.2467669017752971, 2.4987826901996266, 2.5630089003230587, 2.7377288186642774, 5.1937658979896675, 3.8563605554932829, 4.4845133909067876, 2.130284284547066, 2.9602689950032728, 5.0062212541991116, 5.9676442585520162, 2.2570001356632856, 6.8411971054101093, 2.7563438298968426, 4.7276830627264941, 3.582367067990142, 3.9523405698149894, 6.8553413853738157, 5.0858901299373809, 4.1812254209649007, 7.2192209080032637, 6.4402617123341059, 6.6274389533438569, 6.3186576885368311, 4.6516827521820217, 4.0545997777170779, 6.865594825435954, 6.4993202696106422, 5.6206213173954378, 4.597663643263302, 5.3082458395844654, 6.4621121691512515, 2.8828921454728942, 2.8137617782918687, 4.6148063504374415, 3.3878648645377645, 5.3193346648162638, 2.1265679616606326, 4.3173508768876703, 2.477299227172681]
