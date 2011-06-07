> module Antioch.RunDailyScheduleTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather              (getWeatherTest)
> import Antioch.Score
> import Antioch.ReceiverTemperatures
> import Antioch.RunDailySchedule
> import Antioch.PProjects
> import Test.HUnit

> tests = TestList [
>      test_periodObsAvailable
>    , test_filterElectives
>     ]

> test_periodObsAvailable = TestCase $ do
>   assertEqual "test_periodObsAvailable_1" False (periodObsAvailable p1 [s])
>   assertEqual "test_periodObsAvailable_2" True  (periodObsAvailable p2 [s])
>     where
>       s'  = defaultSession { project = p }
>       p   = defaultProject { observers = [o] }
>       o   = defaultObserver { blackouts = bs }
>       bs  = [(fromGregorian 2006 1 31 0 0 0, fromGregorian 2006 2 2 0 0 0)]
>       p1  = defaultPeriod {session = s'
>                          , startTime = fromGregorian 2006 2 1 23 45 0
>                          , duration = 60
>                          , pDuration = 60
>                            }
>       p2  = defaultPeriod {session = s'
>                          , startTime = fromGregorian 2006 2 2 0 0 0
>                          , duration = 60
>                          , pDuration = 60
>                            }
>       s = makeSession s' [] [p1, p2]

> test_filterElectives = TestCase $ do
>   w <- getWeatherTest . Just $ fromGregorian 2006 10 13 0 0 0
>   let rs = []
>   rt <- getReceiverTemperatures
>   let aeps = ps
>   results <- cleanElectives w rs rt [] aeps
>   -- result <- mapM (goodElective' w rs rt) [ mkPeriod es2 d 60 4 | d <- dates]
>   -- printList $ [(toSqlString d, r) | (d, r) <- zip dates result]
>   let exp = [7, 6]
>   let res = [peId p | p <- results]
>   assertEqual "test_filterElectives_1" exp res
>   let aps = ((mkPeriod gb (fromGregorian 2006 10 13 12 0 0) 60 2):ps)
>   results <- filterElectives w rs rt aps
>   let exp = [2, 6, 7]
>   let res = [peId p | p <- results]
>   assertEqual "test_filterElectives_2" exp res
>     where
>       mkPeriod s dt dur id = defaultPeriod { session   = s
>                                            , startTime = dt
>                                            , duration  = dur
>                                            , peId      = id
>                                            }
>       gb = head $ findPSessionsByName "GB"
>       cv = head $ findPSessionsByName "CV"
>       e1 = Electives 1 False [3, 5, 7] 
>       e2 = Electives 2 False [4, 6, 8] 
>       es1 = gb { sType = Elective, electives = [e1], sId = 100 }
>       es2 = cv { sType = Elective, electives = [e2], sId = 101}
>       dt = fromGregorian 2006 10 15 0 0 0
>       ps = [mkPeriod es1 (fromGregorian 2006 10 13 17 0 0) 60 3 -- bad
>           , mkPeriod es2 (fromGregorian 2006 10 13 23 0 0) 60 4 -- bad
>           , mkPeriod es1 (fromGregorian 2006 10 14 13 0 0) 60 5 -- bad
>           , mkPeriod es2 (fromGregorian 2006 10 15  9 0 0) 60 6 -- good
>           , mkPeriod es1 (fromGregorian 2006 10 15 14 0 0) 60 7 -- good
>           , mkPeriod es2 (fromGregorian 2006 10 15 23 0 0) 60 8 -- bad
>            ]
>       -- dates = [d | d <- [dt, (60 `addMinutes` dt) .. ((24*60) `addMinutes` dt)]]
>       -- goodElective' w rs rt p = runScoring w rs rt $ goodElective p

