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


> module Antioch.RunScoresTests where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather      (Weather, getWeatherTest, minTSysPrime)
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Antioch.RunScores
> import Antioch.ScoreTests (pSessions)

> import Test.HUnit
> import Data.List
> import Data.Maybe
> import Control.Monad.Trans  (lift, liftIO)

> tests = TestList [ 
>    test_runFactors
>   , test_runNominees
>   , test_runMOC
>   , test_runPeriodMOC
>   , test_runUpdatePeriods
>     ]

> test_runUpdatePeriods = TestCase $ do
>   scores <- runUpdatePeriods pids pTestProjects True
>   assertEqual "test_runScorePeriods_1" exp scores
>   scores <- runUpdatePeriods [450] projs True
>   assertEqual "test_runScorePeriods_2" exp2 scores
>     where
>   -- try the periods in PProjects wich mostly score zero
>   pids = [100, 101, 200, 201] -- from PProjects.lhs
>   scores' = [0.0, 0.0, 2.126854, 0.0]::[Score]
>   -- moc's are calculated since these are all currently Nothing
>   mocs = [Just False, Just False, Just True, Just True]
>   exp = zipWith3 mkScores pids scores' mocs
>   mkScores pid score moc = (pid, score, Nothing, moc)
>   -- now create a period that should score non-zero
>   -- but init pMoc, and re-init the historical score
>   s = defaultSession { receivers = [[Rcvr1_2]] 
>                      , frequency = 1.1
>                      , dec = 1.5 -- always up
>                      }
>   p = defaultPeriod { session = s
>                     , startTime = fromGregorian 2006 2 1 0 30 0
>                     , duration = 60
>                     , peId = 450
>                     , pScore = -1.0 -- *will* be recalced
>                     , pMoc = Just True -- won't be recalced
>                     }
>   s' = makeSession s [] [p]
>   proj = defaultProject
>   projs = [makeProject proj 100 100 [s']]
>   exp2 = [(450, 0.37846184, Just 0.37846184, Nothing)]

> test_runFactors = TestCase $ do
>   let dt = fromGregorian 2006 9 2 14 30 0
>   let s' = findPSessionByName "CV"
>   let s = s' { sId = 555}
>   let dur = 15::Minutes
>   let proj = defaultProject
>   let projs = [makeProject proj 1000 1000 ([s] ++ pSessions')]
>   -- Here we try to get the same results as ScoreTests.test_scoreFactors & test_scoreElements
>   -- and we can, except for pressures and some project info
>   (sess, factors) <- runFactors 555 dt dur projs True
>   assertEqual "test_runFactors 0" 1 (length factors)
>   assertEqual "test_runFactors 1" 33 (length . head $ factors)
>   assertEqual "test_runFactors 2" sess s
>   mapM_ (assertFactor factors) exp 
>   -- now make sure wband doesn't blow this up
>   let sw = s { sId = 556, receivers = [[Rcvr68_92]] }
>   let proj = defaultProject
>   let projs = [makeProject proj 1000 1000 ([sw] ++ pSessions')]
>   (sess, factors) <- runFactors 556 dt dur projs True
>   assertEqual "test_runFactors 3" 1 (length factors)
>   assertEqual "test_runFactors 4" 33 (length . head $ factors)
>   assertEqual "test_runFactors 5" sess sw
>       where
>     lookup' factors name = fromJust . fromJust . lookup name . head $ factors
>     assertFactor factors (key, value) = assertEqual ("test_scoreFactors " ++ key) value (lookup' factors key)
>     exp = [("stringency",1.0954108)
>           ,("atmosphericEfficiency",0.9439829)
>           ,("surfaceObservingEfficiency",0.9982148)
>           ,("trackingEfficiency",0.99917835)
>           -- Pressures differ due to diff in session pool
>           --,("rightAscensionPressure",1.0) 
>           --,("frequencyPressure",1.9724026)
>           ,("observingEfficiencyLimit",1.0)
>           ,("hourAngleLimit",1.0)
>           ,("zenithAngleLimit",1.0)
>           ,("trackingErrorLimit",1.0)
>           ,("atmosphericStabilityLimit",1.0)
>           ,("scienceGrade",1.0)
>           ,("thesisProject",1.0)
>           -- this tests' project isn't identical 
>           --,("projectCompletion",1.0024)
>           ,("observerOnSite",1.0)
>           ,("receiver",1.0)
>           ,("correctTimeOfDay",1.0)
>           ,("lstExcepted",1.0)
>           ,("observerAvailable",1.0)
>           ,("projectBlackout",1.0)
>           ,("inWindows",1.0)
>           -- scoreElements
>           , ("opacity", 8.786574e-3)
>           , ("elevation", 36.60029)
>           , ("sysNoiseTemp", 23.294113)
>           ]

> test_runNominees = TestCase $ do
>     let dt = fromGregorian 2006 10 1 17 0 0
>     bestDurs <- runNominees dt Nothing Nothing params pTestProjects True
>     --print $ map (\(n, v, d) -> (sName n, v, d)) bestDurs
>     assertEqual "test_bestDurations 1" 12 (length bestDurs)
>     let (s, v, d) = bestDurs !! 1
>     assertEqual "test_runNominees 2 n" "CV" (sName s)
>     assertEqual "test_runNominees 2 v" 3.9800308 v
>     assertEqual "test_runNominees 2 d" 360 d
>     let (s, v, d) = bestDurs !! 6
>     assertEqual "test_runNominees 3 n" "AS" (sName s)
>     assertEqual "test_runNominees 3 v" 3.3165581 v
>     assertEqual "test_runNominees 3 d" 450 d
>     -- what the hell, test them all
>     assertEqual "test_runNominees_1" exp $ map (\(n, v, d) -> (sName n, v, d)) bestDurs
>     -- change the params
>     bestDurs <- runNominees dt Nothing Nothing params2 pTestProjects True
>     assertEqual "test_runNominees_2" exp $ map (\(n, v, d) -> (sName n, v, d)) bestDurs
>     -- change the params again
>     bestDurs <- runNominees dt Nothing Nothing params3 pTestProjects True
>     -- there's no backup sessions!
>     assertEqual "test_runNominees_3" [] $ map (\(n, v, d) -> (sName n, v, d)) bestDurs
>     -- alter the min/max
>     bestDurs <- runNominees dt (Just 360) (Just 360) params pTestProjects True
>     assertEqual "test_runNominees_4" exp2 $ map (\(n, v, d) -> (sName n, v, d)) bestDurs
>   where
>     -- all false
>     params = [("timeBetween",Just "false"),("minimum",Just "false"),("blackout",Just "false"),("backup",Just "false"),("completed",Just "false"),("rfi",Just "false")]
>     exp = [("GB",0.0,0),("CV",3.9800308,360),("LP",5.345904,270),("TX",0.0,0),("VA",0.0,0),("WV",0.0,0),("AS",3.3165581,450),("MH",0.0,0),("TestWindowed1",0.0,0),("TestWindowed2",0.0,0),("GB_thesis",0.0,0),("WV_thesis",0.0,0)]
>     -- now all true, except backup
>     params2 = [("timeBetween",Just "true"),("minimum",Just "true"),("blackout",Just "true"),("backup",Just "false"),("completed",Just "true"),("rfi",Just "true")]
>     -- differs by exp: AS 450 -> 360
>     exp2 = [("GB",0.0,0),("CV",3.9800308,360),("LP",5.345904,270),("TX",0.0,0),("VA",0.0,0),("WV",0.0,0),("AS",3.2961845,360),("MH",0.0,0),("TestWindowed1",0.0,0),("TestWindowed2",0.0,0),("GB_thesis",0.0,0),("WV_thesis",0.0,0)]
>     -- all false, except backup
>     params3 = [("timeBetween",Just "false"),("minimum",Just "false"),("blackout",Just "false"),("backup",Just "true"),("completed",Just "false"),("rfi",Just "false")]

> test_runMOC = TestCase $ do
>     -- this test is identical to ScoreTests.test_minObsCond's
>     -- second assert, since in test mode, we set the weather
>     -- origin to be an hour earlier then the passed in time.
>     let dt = fromGregorian 2006 10 13 16 0 0
>     mocs <- mapM (runMOC' dt) sess
>     assertEqual "test_runMOC" expected mocs
>   where
>     runMOC' dt s = do
>       Just moc <- runMOC dt 30 s True -- test param == True!
>       return moc
>     names = ["GB","CV","LP","TX","VA","WV","AS"]
>     sess = concatMap (\name -> findPSessionsByName name) names
>     expected = [False,True,True,True,True,False,True]

> test_runPeriodMOC = TestCase $ do
>     mocs <- mapM runPeriodMOC' periods
>     assertEqual "test_runPeriodMOC" expected mocs
>   where
>     dt = fromGregorian 2006 10 13 16 0 0
>     periods = [defaultPeriod {startTime = dt
>                             , duration  = 30
>                             , session   = s} | s <- sess]
>     runPeriodMOC' p = do
>       Just moc <- runPeriodMOC p True -- test param == True!
>       return moc
>     names = ["GB","CV","LP","TX","VA","WV","AS"]
>     sess = concatMap (\name -> findPSessionsByName name) names
>     expected = [False,True,True,True,True,False,True]

Utilities:

Note these are *almost* identical to ScoreTests's pSessions.
Add id's to the test sessions from ScoreTests.lhs:

> pSessions' = map addId $ zip [340,341,342,343] pSessions
>   where
>     addId (id, s) = s {sId = id }
