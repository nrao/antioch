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

> module Antioch.SimulationTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather    (getWeatherTest)
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.Simulate
> import Antioch.Debug
> import Antioch.TimeAccounting
> import Antioch.Statistics (scheduleHonorsFixed)
> import Antioch.Generators (internalConflicts)
> import Data.List (sort, find, nub)
> import Data.Maybe
> import Control.OldException
> import Test.HUnit
> import System.Random

> tests = TestList [ 
>     test_simulateDailySchedule
>   , test_simulateDailyScheduleWithFixed
>   , test_simulateDailyScheduleWithWindows
>   , test_simulateWithWindows_2
>   , test_exhaustive_history
>   , test_honor_history
>   --, test_cancellations
>   , test_updateHistory
>   , test_updateSessions
>   , test_periodInWindow'
>   , test_filterDupUnpubPeriods
>   , test_getNewlyScheduledWindowInfo
>   , test_findNotCompleteWindows
>   , test_getWindowInfo
>   , test_getWindows
>   , test_getWindow
>   , test_newlyPublished
>   , test_updateSessionPeriods
>   , test_updatePublishedPeriods
>                  ]


> test_reconcileTimeRemaining = TestCase $ do
>   let minD = minDuration s
>   let s' = reconcileTimeRemaining s
>   assertEqual "test_reconcileTimeRemaining 1" False (sClosed s')
>   assertEqual "test_reconcileTimeRemaining 2" minD (minDuration s')
>   -- lower the remaining time by adding a big period
>   let p = mkPeriod 4600
>   let s2 = makeSession s [] [p]
>   let s' = reconcileTimeRemaining s2
>   assertEqual "test_reconcileTimeRemaining 3" False (sClosed s')
>   assertEqual "test_reconcileTimeRemaining 4" minD (minDuration s')
>   -- now lower the remaining time to below the min duration
>   -- but above the threshold!
>   let p = mkPeriod 4700
>   let s3 = makeSession s [] [p]
>   let s' = reconcileTimeRemaining s3
>   assertEqual "test_reconcileTimeRemaining 5" False (sClosed s')
>   assertEqual "test_reconcileTimeRemaining 6" 100 (minDuration s')
>   -- finally lower the time remaining below the threshold
>   let p = mkPeriod 4775
>   let s4 = makeSession s [] [p]
>   let s' = reconcileTimeRemaining s4
>   assertEqual "test_reconcileTimeRemaining 7" True (sClosed s')
>   assertEqual "test_reconcileTimeRemaining 8" minD (minDuration s')
>     where
>       s = gb
>       mkPeriod t = defaultPeriod { session = gb
>                                  , duration = t
>                                  , pDuration = t
>                                  , pState = Scheduled }

> test_updatePublishedPeriods = TestCase $ do
>     -- no-op
>     let result = updatePublishedPeriods [] history
>     -- remember that identity between periods is just on:
>     -- session id, start time, duration
>     assertEqual "test_updatePublishedPeriods 1" history result
>     let states = take 5 $ repeat Pending
>     assertEqual "test_updatePublishedPeriods 2" states (map pState result)
>     -- update one of the periods
>     let result = updatePublishedPeriods [published] history
>     -- this still passes because it doesn't check the pState
>     assertEqual "test_updatePublishedPeriods 3" history result
>     -- make sure the last one gets published!
>     let states = [Pending,Pending,Pending,Pending,Scheduled]
>     assertEqual "test_updatePublishedPeriods 4" states (map pState result)
>   where
>     history = concatMap periods $ getWindowedPSessions ++ [testWindow3]
>     w3period = (head . periods $ testWindow3) 
>     published = w3period {pState = Scheduled, pDuration = duration w3period}
>     
>

> test_updateSessionPeriods = TestCase $ do
>     -- no-op: nothing gets published, canceled, or created
>     let result = updateSessionPeriods s [] [] []
>     assertEqual "test_updateSessionPeriods 1" ps result
>     assertEqual "test_updateSessionPeriods 2" Pending (pState . head $ result)
>     -- publish the sessions' one default period
>     let result = updateSessionPeriods s [published] [] []
>     assertEqual "test_updateSessionPeriods 3" ps result
>     assertEqual "test_updateSessionPeriods 4" Scheduled (pState . head $ result)
>     -- cancel the default period
>     let result = updateSessionPeriods s [] [p] []
>     assertEqual "test_updateSessionPeriods 5" [] result
>     -- cancel the default period and add a chosen period
>     let result = updateSessionPeriods s [] [p] [newP]
>     assertEqual "test_updateSessionPeriods 5" [newP] result
>   where
>     s = testWindow3
>     ps = periods s
>     p = head . periods $ s
>     dur = duration p
>     published = p { pState = Scheduled, pDuration = dur }
>     newP = defaultPeriod { peId = 124 , startTime = fromGregorian 2006 10 20 17 30 0 , duration = 4*60    }

> test_newlyPublished = TestCase $ do
>   assertEqual "test_newlyPublished 1" True (newlyPublished [pendingPeriod] scheduledPeriod)
>   assertEqual "test_newlyPublished 2" False (newlyPublished [pendingPeriod] pendingPeriod)
>   assertEqual "test_newlyPublished 1" True (newlyPublished history scheduledPeriod)
>   assertEqual "test_newlyPublished 2" False (newlyPublished history pendingPeriod)
>     where
>   pendingPeriod = head $ periods testWindow3
>   scheduledPeriod = pendingPeriod {pState = Scheduled}
>   history = concatMap periods $ getOpenPSessions ++ getWindowedPSessions ++ [testWindow3]


The input to this test must be setup to be like the input found where
it is called in Simulate.lhs: that is, we have just created a new 
schedule with 'pack' and are trying to see how that affects the
unaltered list of input sessions.

> test_getNewlyScheduledWindowInfo = TestCase $ do
>     let winInfo = getNewlyScheduledWindowInfo ss [p]
>     assertEqual "getNewlyScheduledWindowInfo" 1 (length winInfo)
>     let result = map (\(w, c, d) -> (wId w, peId . fromJust $ c, peId d)) winInfo 
>     assertEqual "getNewlyScheduledWindowInfo" [(123, 300, 121)] result 
>   where
>     -- the input to pack; assuming no pre-scheduled periods
>     ss = getOpenPSessions ++ getWindowedPSessions ++ [testWindow3]
>     -- the output from pack: a chosen period for testWindow3
>     p = defaultPeriod { session = testWindow3
>                       , peId = 300
>                       , startTime = fromGregorian 2006 10 13 17 30 0 
>                       , duration = 4*60 }
>     

> test_findNotCompleteWindows = TestCase $ do
>     let psWs = findNotCompleteWindows ss ps
>     let result = map (\(p, w) -> (peId p, wId w)) psWs
>     assertEqual "findNotCompleteWindows" exp result 
>   where
>     -- contains just one complete window (wId = 132)
>     ss = getOpenPSessions ++ getWindowedPSessions ++ [testWindow3]
>     ps = concatMap periods ss
>     exp = [(100,121),(101,122),(200,131),(121,123)]

> test_getDefaultPeriods = TestCase $ do
>     let pIds = map peId $ getDefaultPeriods ss ws
>     assertEqual "getDefaultPeriods" expPids pIds 
>   where
>     ss = getOpenPSessions ++ getWindowedPSessions ++ [testWindow3]
>     ws = concatMap windows ss
>     expPids = [100,101,200,201,121]

The input to this test must be setup to be like the input found where
it is called in Simulate.lhs: that is, we have just created a new 
schedule with 'pack' and are trying to see how that affects the
unaltered list of input sessions.

> test_getWindowInfo_2 = TestCase $ do
>     let wInfo = getWindowInfo ss ps 
>     let wIds = map (\(w, c, d) -> wId w) $ wInfo 
>     assertEqual "test_getWindowInfo_2 1" [121,122,131,132,123] wIds
>     let defaultIds = map (\(w, c, d) -> peId d) $ wInfo 
>     assertEqual "test_getWindowInfo_2 3"  [100,101,200,201,121] defaultIds
>     let chosen = map (\(w, c, d) -> c) $ wInfo 
>     assertEqual "test_getWindowInfo_2 2"  (take 4 $ repeat Nothing) (take 4 chosen)
>     assertEqual "test_getWindowInfo_2 2"  300 (peId . fromJust . last $ chosen)
>   where
>     -- input to pack
>     ss = getOpenPSessions ++ getWindowedPSessions ++ [testWindow3]
>     -- output from pack: all the default periods, except for the one
>     -- from testWindow3, but with a chosen for that session
>     p = defaultPeriod { session = testWindow3
>                       , peId = 300
>                       , startTime = fromGregorian 2006 10 13 17 30 0 
>                       , duration = 4*60 }
>     ps = (concatMap periods getWindowedPSessions) ++ [p]

> test_getWindowInfo = TestCase $ do
>     let wInfo = getWindowInfo ss ps
>     let wIds = map (\(w, c, d) -> wId w) $ wInfo 
>     assertEqual "test_getWindowInfo 1" expWids wIds
>     let chosen = nub . sort $ map (\(w, c, d) -> c) $ wInfo 
>     assertEqual "test_getWindowInfo 2"  [Nothing] chosen
>     let defaultIds = map (\(w, c, d) -> peId d) $ wInfo 
>     assertEqual "test_getWindowInfo 3"  expPids defaultIds
>   where
>     ss = getOpenPSessions ++ getWindowedPSessions ++ [testWindow3]
>     ps = sort $ concatMap periods ss 
>     pIds = map peId ps
>     expWids = [131,121,132,123,122] 
>     expPids = [200,100,201,121,101]

> test_getWindows = TestCase $ do
>     let wIds = map wId $ getWindows ss ps
>     assertEqual "test_getWindow" exp wIds
>   where
>     ss = getOpenPSessions ++ getWindowedPSessions ++ [testWindow3]
>     ps = sort $ concatMap periods ss
>     pIds = map peId ps
>     exp = [131,121,132,123,122]  

> test_getWindow = TestCase $ do
>     let wIds = map wId $ map (getWindow ss) ps
>     assertEqual "test_getWindow" exp wIds
>   where
>     ss = getWindowedPSessions ++ [testWindow3]
>     ps = sort $ concatMap periods ss
>     pIds = map peId ps
>     exp = [131,121,132,123,122]  
>     

Attempt to see if the old test_sim_pack still works:

> test_simulateDailySchedule = TestCase $ do
>     (result, t, _) <- simulateDailySchedule rs dt packDays simDays history ss True True [] []
>     --print $ take 4 $ map duration result
>     --print $ take 4 $ map (toSqlString . startTime) result
>     --print $ take 4 $ map (sName . session) result
>     assertEqual "SimulationTests_test_sim_pack" exp (take 4 result)
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     simDays = 2
>     packDays = 2
>     history = []
>     ss = getOpenPSessions
>     expSs = [cv, va, tx, tx]
>     dts = [ fromGregorian 2006 2 1  3  0 0
>           , fromGregorian 2006 2 1  8  0 0
>           , fromGregorian 2006 2 1 12  0 0
>           , fromGregorian 2006 2 1 16  0 0
>           ]
>     durs = [300, 240, 240, 255] 
>     scores = replicate 10 0.0
>     exp = zipWith9 Period (repeat 0) expSs dts durs scores (repeat Pending) dts (repeat False) durs
>     

> test_simulateDailyScheduleWithFixed = TestCase $ do
>     -- fisrt simulate with JUST the one fixed session & period
>     let history = concatMap periods [s1]
>     (result, t, _) <- simulateDailySchedule rs dt1 packDays simDays history [s1] True True [] []
>     assertEqual "test_simFixed 1" 1 (length result)
>     assertEqual "test_simFixed 2" (startTime . head $ history) (startTime . head $ result)
>     -- make sure they are all getting published properly
>     assertEqual "test_simFixed 3" True (all (==True) $ map published result)
>     -- now make sure the fixed period is still there when it 
>     -- gets scheduled around
>     let ss = getOpenPSessions
>     (result, t, _) <- simulateDailySchedule rs dt1 packDays simDays history ss True True [] []
>     assertEqual "test_simFixed 4" 18 (length result)
>     assertEqual "test_simFixed 5" (head history) (result !! 2)
>     assertEqual "test_simFixed 6" 1 (length $ filter (\p -> (sId . session $ p) == 101) result)
>     -- make sure they are all getting published properly
>     assertEqual "test_simFixed 7" True (all (==True) $ map published result)
>   where
>     rs  = []
>     dt1 = fromGregorian 2006  9 20 0 0 0
>     dt2 = fromGregorian 2006  9 21 0 0 0
>     dt3 = fromGregorian 2006  9 26 0 0 0
>     dt4 = fromGregorian 2006 10 15 0 0 0
>     dt5 = fromGregorian 2006  9 25 0 0 0
>     simDays = 3
>     packDays = 2
>     --history = concat . map periods $ ss
>     --ss = getWindowedPSessions
>     published p = ((pState p) == Scheduled) && ((duration p) == pDuration p)
>     s1' = defaultSession { sId = 101
>                         , sName = "101"
>                         , sType = Fixed
>                         , frequency = 1.1
>                         , band = L
>                         }
>     p1 = defaultPeriod { startTime = fromGregorian 2006 9 20 16 0 0
>                        , duration = 2*60
>                        , pState = Pending
>                        , pDuration = 0
>                        , session = s1'
>                        }
>     s1 = makeSession s1' [] [p1]


> test_simulateDailyScheduleWithWindows = TestCase $ do
>     -- default windowed periods
>     let dwps = sort . concat . map periods $ ss
>     (result, t, _) <- simulateDailySchedule rs dt1 packDays simDays history ss True True [] []
>     --  ***    No competition, expect an earlier period to be scheduled
>     -- Four scheduled periods, first is new on first day of window and
>     -- rest are defaults
>     assertEqual "test_simulateDailyScheduleWithWindows 1" 4 (length result)
>     assertEqual "test_simulateDailyScheduleWithWindows 2" (fromGregorian 2006 9 22 3 45 0) (startTime . head $ result)
>     assertEqual "test_simulateDailyScheduleWithWindows 3" (tail dwps) (tail result)
>     let (win, chosen, def) = head . getWindowPeriodsFromTrace $ t
>     -- New period causes the chosen flag in the window to become true
>     -- Same new period in the result and in the trace
>     assertEqual "test_simulateDailyScheduleWithWindows 4" (Just . head $ result) chosen
>     -- The returned default period should be the same one as attached
>     -- to the session
>     assertEqual "test_simulateDailyScheduleWithWindows 5" def (head dwps)
>     -- make sure they are all getting published properly
>     assertEqual "test_simDSWin" [True,False,False,False] (map published result)
>
>     --  ***    No competition, but better weather, opportunity to
>     -- schedule multiple chosen periods, but still get just one.
>     (result, t, _) <- simulateDailySchedule rs dt2 packDays simDays history ss True True [] []
>     -- Four scheduled periods, all the default periods
>     assertEqual "test_simulateDailyScheduleWithWindows 6" 4 (length result)
>     -- The first one being the first period in session TestWindowed2
>     assertEqual "test_simulateDailyScheduleWithWindows 7" (fromGregorian 2006 9 22 3 45 0) (startTime . head $ result)
>     assertEqual "test_simulateDailyScheduleWithWindows 8" (tail dwps) (tail result)
>     let (win, chosen, def) = head . getWindowPeriodsFromTrace $ t
>     -- Same new period in the result and in the trace
>     assertEqual "test_simulateDailyScheduleWithWindows 9" (Just . head $ result) chosen
>     -- The returned default period should be the same one as attached
>     -- to the session
>     assertEqual "test_simulateDailyScheduleWithWindows 10" def (head dwps)
>     assertEqual "test_simDSWin 2" [True,False,False,False] (map published result)
>
>     --  ***    No competition, but the scheduling range encompasses
>     --         a  default window, so no new periods should be
>     --         scheduled.
>     (result, t, _) <- simulateDailySchedule rs dt3 packDays simDays history ss True True [] []
>     -- Four scheduled periods, all the default periods
>     assertEqual "test_simulateDailyScheduleWithWindows 11" 4 (length result)
>     -- Results should be all default periods
>     assertEqual "test_simulateDailyScheduleWithWindows 12" dwps result
>     let (win, chosen, def) = head . getWindowPeriodsFromTrace $ t
>     -- No chosen period
>     assertEqual "test_simulateDailyScheduleWithWindows 13" Nothing chosen
>     -- The returned default period should be the same one as attached
>     -- to the session
>     assertEqual "test_simulateDailyScheduleWithWindows 14" def (head dwps)
>     assertEqual "test_simDSWin 3" [True,False,False,False] (map published result)
> 
>     --  ***    No competition, but the scheduling range does not 
>     --         encompass a defaul period, and no chosen period is 
>     --         scheduled
>     (result, t, _) <- simulateDailySchedule rs dt4 packDays simDays history ss True True [] []
>     -- Four periods, all the default periods, but NOT scheduled
>     assertEqual "test_simulateDailyScheduleWithWindows 14" 4 (length result)
>     -- Results should be all default periods
>     assertEqual "test_simulateDailyScheduleWithWindows 15" dwps result
>     -- No windowed periods in trace
>     assertEqual "test_simulateDailyScheduleWithWindows 16" [] (getWindowPeriodsFromTrace t)
>     assertEqual "test_simDSWin 4" True (all (==False) $ map published result)
>
>     --  ***    No competition, scheduling across two windows
>     --         resulting in a chosen and a default window.
>     (result, t, _) <- simulateDailySchedule rs dt5 packDays 10 history ss True True [] []
>     -- Four scheduled periods, all the default periods
>     assertEqual "test_simulateDailyScheduleWithWindows 17" 4 (length result)
>     -- Results should be one chosen followed by three defaults.
>     assertEqual "test_simulateDailyScheduleWithWindows 18" (fromGregorian 2006 9 25 3 30 0) (startTime . head $ result)
>     assertEqual "test_simulateDailyScheduleWithWindows 19" (tail dwps) (tail result)
>     let (win, chosen, def) = head . getWindowPeriodsFromTrace $ t
>     -- Same new period in the result and in the trace
>     assertEqual "test_simulateDailyScheduleWithWindows 20" (Just . head $ result) chosen
>     -- The returned default period should be the same one as attached
>     -- to the session
>     assertEqual "test_simulateDailyScheduleWithWindows 21" def (head dwps)
>     assertEqual "test_simDSWin 5" [True, True, False, False] (map published result)
> 
>   where
>     rs  = []
>     dt1 = fromGregorian 2006  9 20 0 0 0
>     dt2 = fromGregorian 2006  9 21 0 0 0
>     dt3 = fromGregorian 2006  9 26 0 0 0
>     dt4 = fromGregorian 2006 10 15 0 0 0
>     dt5 = fromGregorian 2006  9 25 0 0 0
>     simDays = 2
>     packDays = 2
>     history = concat . map periods $ ss
>     ss = getWindowedPSessions
>     published p = ((pState p) == Scheduled) && ((duration p) == pDuration p)


> test_simulateWithWindows_2 = TestCase $ do
>   (result, t, _) <- simulateDailySchedule rs start packDays 2 [] [winS1] True True [] []
>   assertEqual "test_simulateWithWindows_2_1" [expP1] result
>   (result, t, _) <- simulateDailySchedule rs start packDays 2 [] [winS2] True True [] []
>   assertEqual "test_simulateWithWindows_2_2" [expP2] result
>   (result, t, _) <- simulateDailySchedule rs start packDays 2 [] [winS1, winS2] True True [] []
>   -- make sure they are both being scheduled!!!
>   assertEqual "test_simulateWithWindows_2_2" 2 (length result)
>   (result, t, _) <- simulateDailySchedule rs start packDays 2 [] [s1, s2] True True [] []
>   -- make sure they are both being scheduled!!!
>   assertEqual "test_simulateWithWindows_2_2" 2 (length result)
>   
>     where
>   rs = []
>   packDays = 2
>   dur = 2*60
>   start = fromGregorian 2006 2 2 0 0 0
>   wend = fromGregorian 2006 2 9 0 0 0
>   -- first windowed session
>   w1pId = 231
>   proj = defaultProject { pId = 1, pAllottedT = dur, pAllottedS = dur }
>   winS1' = defaultSession { sName = "one"
>                           , sId = 1
>                           , receivers = [[Rcvr1_2]]
>                           , frequency = 1.1
>                           , ra = 0.1
>                           , dec = 1.5 -- always up
>                           , band = L
>                           , sAllottedT = dur
>                           , sAllottedS = dur
>                           , minDuration = dur
>                           , maxDuration = dur
>                           , sType = Windowed
>                           , project = proj
>                           }
>   w1 = defaultWindow { wRanges = [(start, wend)]
>                        , wPeriodId = Just w1pId
>                        , wTotalTime = dur
>                        } 
>   p1 = defaultPeriod { peId = w1pId
>                       , startTime = addMinutes (-(12*60)) wend
>                       , duration = dur
>                       , pDuration = 0
>                       , pState = Pending
>                       , session = winS1'
>                       }
>   winS1 = makeSession winS1' [w1] [p1] 
>   expP1 = p1 { startTime = start }
>   --ss1 = [winS1]
>   -- second windowed session
>   w2pId = 232
>   proj2 = proj { pId = 2 }
>   winS2' = winS1' { project = proj2, sName = "two", sId = 2 }
>   w2 = w1 { wPeriodId = Just w2pId }
>   p2 = p1 { startTime = addMinutes (-(15*60)) wend
>           , peId = w2pId
>           , session = winS2'
>           }
>   winS2 = makeSession winS2' [w2] [p2]
>   expP2 = p2 { startTime = start }
>   -- open sessions
>   s1 = makeSession winS1 { sType = Open } [] []
>   s2 = makeSession winS2 { sType = Open } [] []


Attempt to see if old test still works:
Test to make sure that our time accounting isn't screwed up by the precence 
of pre-scheduled periods (history)

> test_exhaustive_history = TestCase $ do
>     w <- getWeatherTest $ Just dt
>     -- first, a test where the history uses up all the time
>     (result, t, _) <- simulateDailySchedule rs dt packDays simDays h1 ss1 True True [] []
>     assertEqual "test_sim_schd_pack_ex_hist_1" True (scheduleHonorsFixed h1 result)
>     assertEqual "test_sim_schd_pack_ex_hist_2" h1 result
>     -- now, if history only takes some of the time, make sure 
>     -- that the session's time still gets used up
>     (result, t, _) <- simulateDailySchedule rs dt packDays simDays h2 ss2 True True [] []
>     assertEqual "test_sim_schd_pack_ex_hist_3" True (scheduleHonorsFixed h2 result)
>     let observedTime = sum $ map duration result
>     -- This will fail until we use 'updateSession' in simulate
>     assertEqual "test_sim_schd_pack_ex_hist_4" True (abs (observedTime - (sAllottedT s2)) <= (minDuration s2))
>   where
>     rs  = []
>     dt = fromGregorian 2006 2 1 0 0 0
>     simDays = 7
>     packDays = 2
>     ds = defaultSession { frequency = 2.0, receivers = [[Rcvr1_2]], sType = Fixed }
>     -- a period that uses up all the sessions' time (480)
>     f1 = Period 0 ds {sId = sId cv} (fromGregorian 2006 2 4 3 0 0) 480 0.0 Pending dt False 480
>     h1 = [f1]
>     -- make sure that this session knows it's used up it's time
>     s1 = makeSession (cv { sAllottedT = 480, sAllottedS = 480}) [] h1
>     ss1 = [s1]
>
>     -- a period that uses MOST of the sessions' time (375)
>     f2 = Period 0 ds {sId = sId cv} (fromGregorian 2006 2 4 3 0 0) 375 0.0 Pending dt False 375
>     h2 = [f2]
>     -- make sure that this session knows it's used up MOST of it's time
>     s2 = makeSession (cv { sAllottedT = 480, sAllottedS = 480}) [] h1
>     ss2 = [s2]

Here we see if a long simulation honors pre-scheduled periods

> test_honor_history = TestCase $ do
>     -- first, a test where the history uses up all the time
>     (result, t, _) <- simulateDailySchedule rs dt packDays simDays h1 ss1 True True [] []
>     assertEqual "test_honor_history_1" True (scheduleHonorsFixed h1 result)
>     assertEqual "test_honor_history_2" False (internalConflicts result)
>   where
>     rs  = []
>     -- set it up to be like production 08B beta test scheduling
>     dt = fromGregorian 2006 2 1 0 0 0
>     simDays = 7
>     packDays = 2
>     -- the history is made up of a bunch of regularly spaced periods
>     ds = defaultSession {sId = sId cv, receivers = [[Rcvr1_2]], sType = Fixed}
>     mkFixed start = Period 0 ds start (4*60) 0.0 Pending dt False (sAllottedT cv)
>     fixedDts = [ addMinutes (day*24*60) dt | day <- [0 .. simDays]]
>     h1 = map mkFixed fixedDts
>     ss1 = [gb, va, tx, wv, mh, lp]


Here we attempt to schedule only a single high-frequency session - if it does
get on, it has a high chance of being canceled.

> test_cancellations = TestCase $ do
>     (result, tr, _) <- simulateDailySchedule [] start 2 15 [] ss True True [] []
>     let cs = getCanceledPeriods $ tr
>     assertEqual "test_cancellations_1" exp result
>     assertEqual "test_cancellations_2" 15 (length cs)
>   where
>     ss = [va]
>     start = fromGregorian 2006 6 1 0 0 0 -- summer time
>     p1 = defaultPeriod { session = va
>                        , startTime = fromGregorian 2006 6 15 0 30 0
>                        , duration = 255 }
>     p2 = defaultPeriod { session = va
>                        , startTime = fromGregorian 2006 6 15 22 30 0
>                        , duration = 360 }
>     exp = [p1, p2]
>     

> test_updateHistory = TestCase $ do
>     -- no time overlap of periods: it doesn't really matter how they
>     -- are setup, the result should just be == ++ them up
>     assertEqual "test_updateHistory_1" r1 (updateHistory h1 s1 []) 
>     -- this time, the new schedule (s2) includes all the same periods
>     -- in the history, except now they've been published, so the history
>     -- should get replaced
>     assertEqual "test_updateHistory_2" r1 (updateHistory h1 s2 [])
>     -- now cancel the last period in the history (h1), and make sure
>     -- it is really removed from the history
>     assertEqual "test_updateHistory_3" r3 (updateHistory h1 s3 c3) 
>   where
>     mkDts start num = map (\i->(i*dur) `addMinutes` start) [0 .. (num-1)] 
>     mkPeriod dt = defaultPeriod { startTime = dt, duration = dur }
>     dur = 120 -- two hours
>     -- first test 
>     h1_start = fromGregorian 2006 2 1 0 0 0
>     h1 = map mkPeriod $ mkDts h1_start 5
>     s1_start = fromGregorian 2006 2 1 10 0 0
>     s1 = map mkPeriod $ mkDts s1_start 3
>     dt1 = fromGregorian 2006 2 1 10 0 0
>     r1 = h1 ++ s1
>     -- second test
>     ph1 = (map publish h1)
>     s2 = ph1 ++ s1
>     -- third test
>     s3 = (take 4 ph1) ++ s1
>     c3 = [last ph1]
>     r3 = s3
>     publish p = p {pState = Scheduled, pDuration = duration p}

> test_updateSessions = TestCase $ do
>     -- test initial conditions
>     let psIds = getPeriodIds ss 
>     assertEqual "test_updateSessions_1" [1] psIds
>     -- test an update w/ out canceled periods
>     let updatedSess = updateSessions ss new_ps [] [] []
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_2" [1,2,3] newPsIds
>     -- test an update *with* canceled periods
>     let updatedSess = updateSessions ss new_ps [] canceled [] 
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_3" [2,3] newPsIds
>     -- test an update *with* canceled periods, but no new periods
>     let updatedSess = updateSessions ss [] [] canceled []
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_4" [] newPsIds
>     -- try a non-empty windows argument
>     let updatedSess = updateSessions (tw1:ss) [chosen] [] [condemned] [w1]
>     -- get the windowed session from the results
>     let tw1' = head $ filter (==tw1) updatedSess
>     --    session's first period has changed
>     assertEqual "test_updateSessions_5" tw1_newPs (periods tw1')
>     --    session's first chosen flag has changed
>     assertEqual "test_updateSessions_6" [True, False] (map wComplete $ windows tw1')
>     --    session's windows have not changed
>     assertEqual "test_updateSessions_7" (windows tw1) (windows tw1')
>     --    session's default periods have not changed
>     assertEqual "test_updateSessions_8" [Just 100, Just 101] (map wPeriodId $ windows tw1')
>     --    session's first period's is defaulted
>     assertEqual "test_updateSessions_9" [0, 101] (map peId $ periods tw1')
>     --    session's periods are referencing the session
>     assertEqual "test_updateSessions_10" [tw1', tw1'] (map session $ periods tw1')
>     --    session's windows are referencing the session
>     assertEqual "test_updateSessions_11" [tw1', tw1'] (map wSession $ windows tw1')
>     -- test an update w/ out canceled periods but with some published
>     assertEqual "test_updateSessions_12" [Pending] (map pState (concatMap periods ss))
>     let updatedSess = updateSessions ss new_ps [lp_p_pub] [] []
>     let newPsIds = getPeriodIds updatedSess 
>     assertEqual "test_updateSessions_13" [1,2,3] newPsIds
>     assertEqual "test_updateSessions_14" [Scheduled,Scheduled,Scheduled] (map pState (concatMap periods updatedSess))
>   where
>     lp_p = defaultPeriod { peId = 1, session = lp }
>     lp_ps = [lp_p]
>     lp_p_pub = lp_p {pState = Scheduled, pDuration = duration lp_p}
>     canceled = lp_ps
>     lp' = makeSession lp [] lp_ps
>     ss = [lp', cv]
>     dp = defaultPeriod {duration  = 15
>                       , pState = Scheduled
>                       , pDuration = 15
>                        }
>     new_lp_period = dp { peId = 2, session = lp }
>     new_cv_period = dp { peId = 3, session = cv }
>     new_ps = [new_lp_period, new_cv_period]
>     getPeriodIds sess = sort $ map peId $ concatMap periods sess
>     w1 = (head . windows $ tw1) {wComplete = True}
>     condemned = head . periods $ tw1
>     chosen = defaultPeriod {
>                 session = tw1
>               , startTime = fromGregorian 2006 10 2 12 15 0
>               , duration = 4*60
>               , pState = Scheduled
>               , pDuration = 4*60
>                }
>     tw1_newPs = [chosen, last . periods $ tw1]

> test_filterDupUnpubPeriods = TestCase $ do
>     -- periods do not have to be that different to be different
>     let p0 = defaultPeriod {startTime = 10000}
>     let p1 = defaultPeriod {startTime = 20000}
>     let p2 = defaultPeriod {startTime = 30000}
>     let p2' = defaultPeriod {startTime = 30000, pState = Scheduled}
>     let p3 = defaultPeriod {startTime = 40000}
>     let p4 = defaultPeriod {startTime = 50000}
>     let p5 = defaultPeriod {startTime = 60000}
>
>     -- should make p2 go away as long as it is next to p2'
>     let result = filterDupUnpubPeriods [p0, p1, p2, p2', p3, p4, p5]
>     assertEqual "test_filterDupUnpubPeriods_1" 6 (length result)
>     assertEqual "test_filterDupUnpubPeriods_2" Scheduled (pState . (!!) result $ 2)
>     let result = filterDupUnpubPeriods [p0, p1, p2', p2, p3, p4, p5]
>     assertEqual "test_filterDupUnpubPeriods_3" 6 (length result)
>     assertEqual "test_filterDupUnpubPeriods_4" Scheduled (pState . (!!) result $ 2) 

> test_periodInWindow' = TestCase $ do
>     assertEqual "test_periodInWindow' 1" True (periodInWindow' p1 w)
>     assertEqual "test_periodInWindow' 2" True (periodInWindow' p2 w)
>     assertEqual "test_periodInWindow' 3" True (periodInWindow' p3 w)
>     assertEqual "test_periodInWindow' 4" False (periodInWindow' p4 w)
>   where
>     start = fromGregorian' 2009 2 8
>     end   = fromGregorian' 2009 2 14
>     w = defaultWindow { wRanges = [(start, end)] }
>     pStart1 = fromGregorian 2009 2 8 12 0 0 
>     p1 = defaultPeriod { startTime = pStart1
>                       , duration = 60 }
>     pStart2 = fromGregorian 2009 2 8 0 0 0 
>     p2 = p1 { startTime = pStart2 }
>     pStart3 = fromGregorian 2009 2 7 23 45 0 
>     p3 = p2 { startTime = pStart3 }
>     pStart4 = fromGregorian 2009 2 7 23 30 0 
>     p4 = p3 { startTime = pStart4 }

Test Utilities:

> lp  = findPSessionByName "LP"
> cv  = findPSessionByName "CV"
> as  = findPSessionByName "AS"
> gb  = findPSessionByName "GB"
> mh  = findPSessionByName "MH"
> va  = findPSessionByName "VA"
> tx  = findPSessionByName "TX"
> wv  = findPSessionByName "WV"
> tw1 = findPSessionByName "TestWindowed1"
> tw2 = findPSessionByName "TestWindowed2"

> testWindow3 = makeSession s' [w] [p]
>     where
>   p = defaultPeriod { peId = 121 , startTime = fromGregorian 2006 10 23 17 30 0 , duration = 4*60    }
>   w = defaultWindow {
>           wId = 123
>         , wRanges = [(fromGregorian' 2006 10 10, fromGregorian' 2006 10 25)]
>         , wPeriodId = Just 121
>         , wTotalTime = 4*60
>                     }
>   s' = defaultSession { sId = 123
>       , sName = "TestWindowed3"
>       , windows = [w]
>       , periods = [p]
>       , sAllottedT = 2*4*60
>       , sAllottedS = 2*4*60
>       , minDuration = 4*60
>       , maxDuration = 4*60
>       , frequency = 1.1
>       , ra = hrs2rad 14.3
>       , dec = deg2rad 13.3
>       , receivers = [[Rcvr1_2]]
>       , band = L
>       , sType = Windowed
>       }

> isSessionKnotted :: Session -> Bool
> isSessionKnotted s = all (\f -> f s) [pknots]
>   where
>     pknots s = all (pknot s) [0 .. ((length . periods $ s) - 1)]
>     pknot s i = s == (session ((periods s) !! i))
>     -- wknots, etc

