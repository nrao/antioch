Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

> module Antioch.RunDailyScheduleTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather              (getWeatherTest)
> import Antioch.Score
> import Antioch.Schedule
> import Antioch.ReceiverTemperatures
> import Antioch.RunDailySchedule
> import Antioch.Utilities
> import Antioch.PProjects
> import Test.HUnit

> tests = TestList [
>      test_runDailySchedule
>    , test_filterHistory
>    , test_periodObsAvailable
>    , test_cleanElectives'
>    , test_filterElectives
>     ]

> test_runDailySchedule = TestCase $ do
>   -- schedule where there are no pre-scheduled periods, and one 
>   -- open session can get scheduled a lot
>   (newPeriods, periodsToDelete) <- runDailySchedule Pack dt1 2 True
>   assertEqual "test_runDailySchedule 1" 4 (length newPeriods)
>   assertEqual "test_runDailySchedule 2" 0 (length periodsToDelete)
>   assertEqual "test_runDailySchedule 3" exp newPeriods
>   -- now schedule before the start of the rx schedule (nobody
>   -- can get scheduled), and a pre-scheduled period gets deleted.
>   (newPeriods, periodsToDelete) <- runDailySchedule Pack dt2 2 True
>   assertEqual "test_runDailySchedule 4" 0 (length newPeriods)
>   assertEqual "test_runDailySchedule 5" 1 (length periodsToDelete)
>   assertEqual "test_runDailySchedule 6" exp2 periodsToDelete
>     where
>       dt1 = fromGregorian 2006 6 1 0 0 0 
>       dt2 = fromGregorian 2006 1 1 0 0 0 
>       ranges = [(fromGregorian 2006 6 1  1 45 0, 480)
>               , (fromGregorian 2006 6 1 19 30 0, 480)
>               , (fromGregorian 2006 6 2 11 30 0, 480)
>               , (fromGregorian 2006 6 3  5 15 0, 480)
>                ]
>       s = defaultSession { sName = "GBT09A-001-05", sId = 4 }
>       mkPeriod (dt, dur) = defaultPeriod { session = s
>                                          , startTime = dt
>                                          , duration = dur
>                                          }
>       exp = map mkPeriod ranges
>       s2 = defaultSession { sName = "GBT09A-001-02", sId = 1 }
>       exp2 = [defaultPeriod { session = s2
>                             , startTime = fromGregorian 2006 1 1 0 0 0
>                             , duration = 240 } ]

> test_filterHistory = TestCase $ do
>   w <- getWeatherTest . Just $ fromGregorian 2006 10 13 0 0 0
>   let rs = []
>   rt <- getReceiverTemperatures
>   --results <- filterElectives w rs rt ps
>   results <- filterHistory w rs rt ps [gb] pTestProjects True
>   print "results:"
>   printList results
>   let res = [peId p | p <- results]
>   assertEqual "test_filterHistory_1" [3, 8] res
>     where
>       -- just some sugar to help make a period
>       mkPeriod s dt dur st id = defaultPeriod { session    = s
>                                               , startTime  = dt
>                                               , duration   = dur
>                                               , pState     = st
>                                               , peId       = id
>                                               }
>       -- create electives
>       e1 = Electives 1 False [3, 5, 7] 
>       e2 = Electives 2 False [1, 2, 4] 
>       gb = head $ findPSessionsByName "GB"
>       es = gb { sType = Elective, electives = [e1, e2], sId = 100, guaranteed = False }
>       -- create periods
>       ps = [
>          mkPeriod es (fromGregorian 2006 10 13 17 0 0) 60 Scheduled 3
>        , mkPeriod es (fromGregorian 2006 10 14 13 0 0) 60 Pending 5
>        , mkPeriod es (fromGregorian 2006 10 15 14 0 0) 60 Pending 7
>        , mkPeriod es (fromGregorian 2006 10 16 17 0 0) 60 Pending 1
>        , mkPeriod es (fromGregorian 2006 10 17 13 0 0) 60 Pending 2
>        , mkPeriod es (fromGregorian 2006 10 18 14 0 0) 60 Pending 4
>        , mkPeriod gb (fromGregorian 2006 10 19 14 0 0) 60 Scheduled 8
>         ]

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

> test_cleanElectives' = TestCase $ do
>   let exp = [2, 4, 8]
>   let res = cleanElectives' (head ps) (tail ps)
>   assertEqual "test_cleanElectives'_1" exp [peId p | p <- res]
>     where
>       s' = defaultSession { sType = Elective, electives = [o, e] }
>       s = makeSession s' [] ps
>       o = defaultElective { eId = 101, ePeriodIds = [1, 3, 7] }
>       e = defaultElective { eId = 102, ePeriodIds = [2, 4, 8] }
>       ps = [defaultPeriod { peId = 1, session = s' }
>           , defaultPeriod { peId = 2, session = s' }
>           , defaultPeriod { peId = 4, session = s' }
>           , defaultPeriod { peId = 3, session = s' }
>           , defaultPeriod { peId = 8, session = s' }
>           , defaultPeriod { peId = 7, session = s' }
>            ]

> test_filterElectives = TestCase $ do
>   w <- getWeatherTest . Just $ fromGregorian 2006 10 13 0 0 0
>   let rs = []
>   rt <- getReceiverTemperatures
>   let aeps = ps
>   results <- cleanElectives w rs rt [] aeps
>   -- result <- mapM (goodElective' w rs rt) [ mkPeriod es2 d 60 4 | d <- dates]
>   -- printList $ [(toSqlString d, r) | (d, r) <- zip dates result]
>   -- Period 11 is retained because it is the last period of a guaranteed
>   -- elective, 7 and 6 are retained because they are the first periods
>   -- of their respective electives to pass the moc threshold.
>   let exp = [11, 7, 6]
>   let res = [peId p | p <- results]
>   assertEqual "test_filterElectives_1" exp res
>   results <- filterElectives w rs rt ps
>   -- Same as test 1 above except period 9 is retained because it is
>   -- already scheduled, but it does not prevent 7 from being scheduled
>   -- since previously scheduled periods in themselves to not deactivate
>   -- other pending periods of its elective.
>   let exp = [11, 6, 7]
>   let res = [peId p | p <- results]
>   assertEqual "test_filterElectives_2" exp res
>     where
>       -- just some sugar to help make a period
>       mkPeriod s dt dur st id = defaultPeriod { session    = s
>                                               , startTime  = dt
>                                               , duration   = dur
>                                               , pState     = st
>                                               , peId       = id
>                                               }
>       -- create electives
>       e1 = Electives 1 False [3, 5, 7] 
>       e2 = Electives 2 False [4, 6, 8] 
>       e3 = Electives 1 False [10, 11]
>       -- create session "templates"
>       gb = head $ findPSessionsByName "GB"
>       cv = head $ findPSessionsByName "CV"
>       es1' = gb { sType = Elective, electives = [e1], sId = 100, guaranteed = False }
>       es2' = cv { sType = Elective, electives = [e2], sId = 101, guaranteed = False}
>       es3' = gb { sType = Elective, electives = [e3], sId = 102, guaranteed = True }
>       -- create periods
>       ps = [
>          mkPeriod es1' (fromGregorian 2006 10 13 17 0 0) 60 Pending 3 -- bad
>        , mkPeriod es2' (fromGregorian 2006 10 13 23 0 0) 60 Pending 4 -- bad
>        , mkPeriod es1' (fromGregorian 2006 10 14 13 0 0) 60 Pending 5 -- bad
>        , mkPeriod es2' (fromGregorian 2006 10 15  9 0 0) 60 Pending 6 -- good
>        , mkPeriod es1' (fromGregorian 2006 10 15 14 0 0) 60 Pending 7 -- good
>        , mkPeriod es2' (fromGregorian 2006 10 15 23 0 0) 60 Pending 8 -- bad
>        , mkPeriod es1' (fromGregorian 2006 10 16 17 0 0) 60 Pending 9 -- good
>        , mkPeriod es3' (fromGregorian 2006 10 13 17 0 0) 60 Pending 10 -- bad
>        , mkPeriod es3' (fromGregorian 2006 10 14 13 0 0) 60 Pending 11 -- bad
>            ]
>       -- construct final sessions with most their knots tied
>       es1 = makeSession es1' [] . filter (\p -> elem (peId p) [3, 5, 7, 9]) $ ps
>       es2 = makeSession es2' [] . filter (\p -> elem (peId p) [4, 6, 8]) $ ps
>       es3 = makeSession es3' [] . filter (\p -> elem (peId p) [10, 11]) $ ps
>       -- debugging stuff:
>       -- dt = fromGregorian 2006 10 15 0 0 0
>       -- dates = [d | d <- [dt, (60 `addMinutes` dt) .. ((24*60) `addMinutes` dt)]]
>       -- goodElective' w rs rt p = runScoring w rs rt $ goodElective p

