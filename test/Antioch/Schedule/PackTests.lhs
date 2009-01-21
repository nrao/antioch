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
>   let periods = pack (fs candidate) starttime duration [] [candidate]
>   w <- getWeather . Just $ starttime 
>   periods' <- runScoring w [] $ periods
>   assertEqual "test_Pack1_1" 1 (length periods')
>   assertEqual "test_Pack1_2" expPeriod (head periods')
>     where
>       starttime = fromGregorian 2006 11 8 12 0 0
>       duration = 12*60
>       fs s = genScore [s]
>       candidate = defaultSession { sName = "singleton"
>                                  , totalTime = 24*60
>                                  , minDuration = 2*60
>                                  , maxDuration = 6*60
>                                  }
>       expStartTime = fromGregorian 2006 11 8 21 15 0
>       expPeriod = defaultPeriod { session = candidate
>                                 , startTime = expStartTime
>                                 , duration = 180
>                                 , pScore = 38.176468
>                                 }

> test_ToItem = TestCase $ do
>   w <- getWeather . Just $ starttime 
>   -- create an item without a mask, i.e. no scoring
>   let item = toItem (fs session) [] session
>   item' <- runScoring w [] item
>   assertEqual "test_ToItem_1" result1 item'
>   assertEqual "test_ToItem_2" 0 (length . iFuture $ item')
>   -- now try it with the mask (dts)
>   let item = toItem (fs session) dts session
>   item' <- runScoring w [] item
>   assertEqual "test_toItem_3" result2 item'
>   assertEqual "test_toItem_4" 49 (length . iFuture $ item') 
>     where
>       starttime = fromGregorian 2006 11 8 12 0 0
>       duration = 12 * 60
>       fs s = genScore [s]
>       -- the 'mask' is just a list of datetimes to score at
>       dts' = quarterDateTimes starttime duration 
>       dts = [(Just dt) | dt <- dts']
>       session = defaultSession { sName = "singleton"
>                                  , totalTime = 24*60
>                                  , minDuration = 2*60
>                                  , maxDuration = 6*60
>                                  }
>       result1 = Item { iId = session
>                     , iMinDur = 8
>                     , iMaxDur = 24
>                     , iFuture = []
>                     , iPast = [] }
>       -- this expected result for the scoring of the session in 15-min
>       -- increments starting at starttime is taken from the ScoreTests.lhs
>       futureResult = (replicate 37 0.0) ++ [3.2315328,3.204887,3.211515
>                                              ,3.219639,3.2261572,3.1090422
>                                              ,3.1223507,3.133507,3.1399984
>                                              ,3.1896782,3.1915512,3.196607]
>       result2 = result1 { iFuture = futureResult }

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

TBF: the beta test code runs packing using TScore, which is essentially a
random score generator.  So to match the two code bases we have to choose:
   * find some way to use a test scorer here in haskell that produces the same scoring results
      * one way to do this is to add the list of desired scores (from the
        python code) to a funtion of time.  This function could then be used
        by a test scoring factor, which replaces 'score genScore'.
   * write new unit tests in the beta code and try and match those results

