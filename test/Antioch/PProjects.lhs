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

> module Antioch.PProjects
>     (
>       pTestProjects
>     , findPSessionByName
>     , findPSessionsByName
>     , getPPeriods
>     , getPSessionId
>     , getOpenPSessions
>     , getWindowedPSessions
>     ) where

> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Utilities

> pTestProjects :: [Project]
> pTestProjects = [project1, project2]

> project1' = defaultProject {
>     pId = 1
>   , pName = "TestDB"
>   , semester = "06A"
>   -- , timeLeft = 28740
>   , pAllottedT = 33812
> }

> p1sessions' = [
>     defaultSession {
>         sId = 1
>       , sName = "GB"
>       , periods = []
>       , sAllottedT = 80*60
>       , sAllottedS = 80*60
>       , minDuration = 2*60
>       , maxDuration = 8*60
>       , frequency = 27.5
>       , ra = hrs2rad 6.4
>       , dec = deg2rad 10.4
>       , receivers = [[Rcvr26_40]]
>       , band = A
>       }
>
>   , defaultSession {
>         sId = 2
>       , sName = "CV"
>       , periods = []
>       , sAllottedT = 60*60
>       , sAllottedS = 60*60
>       , minDuration = 2*60
>       , maxDuration = 8*60
>       , frequency = 4.3
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 30.2
>       , receivers = [[Rcvr4_6]]
>       , band = C
>       }
>
>   , defaultSession {
>         sId = 3
>       , sName = "LP"
>       , periods = [
>             defaultPeriod {
>                 startTime = fromGregorian 2006 10 15 9 0 0
>               , pState = Scheduled
>               , duration = 6*60
>               , pDuration = 6*60
>                }
>           ]
>       , sAllottedT = 40*60
>       , sAllottedS = 40*60
>       , minDuration = 4*60
>       , maxDuration = 6*60
>       , frequency = 5.4
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 5.4
>       , receivers = [[Rcvr4_6]]
>       , band = A
>       }
>
>   , defaultSession {
>         sId = 4
>       , sName = "TX"
>       , sAllottedT = 40*60
>       , sAllottedS = 40*60
>       , minDuration = 4*60
>       , maxDuration = 6*60
>       , frequency = 17.8
>       , ra = hrs2rad 19.9
>       , dec = deg2rad 15.5
>       , receivers = [[Rcvr18_26]]
>       , band = K
>       }
>
>   , defaultSession {
>         sId = 5
>       , sName = "VA"
>       , sAllottedT = 30*60
>       , sAllottedS = 30*60
>       , minDuration = 4*60
>       , maxDuration = 6*60
>       , frequency = 22.7
>       , ra = hrs2rad 12.9
>       , dec = deg2rad 29.2
>       , receivers = [[Rcvr18_26]]
>       , band = K
>       }
>
>   , defaultSession {
>         sId = 6
>       , sName = "WV"
>       , sAllottedT = 120*60
>       , sAllottedS = 120*60
>       , minDuration = 4*60
>       , maxDuration = 6*60
>       , frequency = 34.9
>       , ra = hrs2rad 4.2
>       , dec = deg2rad 17.4
>       , receivers = [[Rcvr26_40]]
>       , band = A
>       }
>
>   , defaultSession {
>         sId = 7
>       , sName = "AS"
>       , sAllottedT = 40*60
>       , sAllottedS = 40*60
>       , minDuration = 6*60
>       , maxDuration = 8*60
>       , frequency = 0.5
>       , ra = hrs2rad 14.3
>       , dec = deg2rad 18.4
>       , receivers = [[Rcvr_450]]
>       , band = L
>       }
>
>   , defaultSession {
>         sId = 8
>       , sName = "MH"
>       , periods = [
>             defaultPeriod {
>                 startTime = fromGregorian 2006 10 16 20 0 0
>               , pState = Scheduled
>               , duration = 4*60
>               , pDuration = 4*60
>                }
>           , defaultPeriod {
>                 startTime = fromGregorian 2006 11 1 20 0 0
>               , pState = Scheduled
>               , duration = 2*60
>               , pDuration = 2*60
>                }
>           ]
>       , sAllottedT = 40*60
>       , sAllottedS = 40*60
>       , minDuration = 2*60
>       , maxDuration = 6*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [[Rcvr_PAR]]
>       , band = W
>       , trkErrThreshold = trkErrThresholdFilledArrays
>       }
>
>   , defaultSession {
>         sId = 12
>       , sName = "TestWindowed1"
>       , windows = [
>             defaultWindow {
>                 wId = 121
>               , wRanges = [(fromGregorian' 2006 10 1
>                           , fromGregorian' 2006 10 8)]
>               , wPeriodId = Just 100
>               , wTotalTime = 4*60
>                }
>           , defaultWindow {
>                 wId = 122
>               , wRanges = [(fromGregorian' 2006 10 22
>                           , fromGregorian' 2006 10 28)]
>               , wPeriodId = Just 101
>               , wTotalTime = 4*60
>                }
>           ]
>       , periods = [
>             defaultPeriod {
>                 peId = 100
>               , startTime = fromGregorian 2006 10 4 17 15 0
>               , duration = 4*60
>                }
>           , defaultPeriod {
>                 peId = 101
>               , startTime = fromGregorian 2006 10 27 17 45 0
>               , duration = 4*60
>                }
>           ]
>       , sAllottedT = 2*4*60
>       , sAllottedS = 2*4*60
>       , minDuration = 4*60
>       , maxDuration = 4*60
>       , frequency = 67.8
>       , ra = hrs2rad 14.3
>       , dec = deg2rad 13.3
>       , receivers = [[Rcvr_PAR]]
>       , band = W
>       , sType = Windowed
>       , trkErrThreshold = trkErrThresholdFilledArrays
>       }
>
>   , defaultSession {
>         sId = 13
>       , sName = "TestWindowed2"
>       , windows = [
>             defaultWindow {
>                 wId = 131
>               , wRanges = [(fromGregorian' 2006 9 22
>                           , fromGregorian' 2006 9 29)]
>               , wPeriodId = Just 200
>               , wTotalTime = 3*60
>                }
>           , defaultWindow {
>                 wId = 132
>               , wRanges = [(fromGregorian' 2006 10 15
>                           , fromGregorian' 2006 10 22)]
>               , wPeriodId = Just 201
>               , wTotalTime = 15 -- complete, but not all time consumed
>               , wComplete = True
>                }
>           ]
>       , periods = [
>             defaultPeriod {
>                 peId = 200
>               , startTime = fromGregorian 2006 9 28 2 0 0
>               , duration = 3*60
>                }
>           , defaultPeriod {
>                 peId = 201
>               , startTime = fromGregorian 2006 10 20 6 30 0
>               , duration = 3*60
>                }
>           ]
>       , sAllottedT = 2*3*60
>       , sAllottedS = 2*3*60
>       , minDuration = 3*60
>       , maxDuration = 3*60
>       , timeBetween = 14*60
>       , frequency = 2.4
>       , ra = hrs2rad 0.9
>       , dec = deg2rad 16.1
>       , receivers = [[Rcvr2_3]]
>       , band = S
>       , sType = Windowed
>       }
>   ]

> p1sessions = [ makeSession s (windows s) (periods s) | s <- p1sessions' ]
> project1 = makeProject project1' (500*60) (500*60) p1sessions

> project2' = defaultProject {
>     pId = 2
>   , pName = "TestThesis"
>   , semester = "06A"
>   , thesis = True
>   -- , timeLeft = 14*60
>   -- , pAllottedT = 14*60
> }

> p2sessions' = [
>     defaultSession {
>         sId = 15
>       , sName = "GB_thesis"
>       , periods = []
>       , sAllottedT = 80*60
>       , sAllottedS = 80*60
>       , minDuration = 2*60
>       , maxDuration = 8*60
>       , frequency = 27.5
>       , ra = hrs2rad 6.4
>       , dec = deg2rad 10.4
>       , receivers = [[Rcvr26_40]]
>       , grade = 3.0
>       , band = A
>       }
>
>   , defaultSession {
>         sId = 16
>       , sName = "WV_thesis"
>       , periods = []
>       , sAllottedT = 120*60
>       , sAllottedS = 120*60
>       , minDuration = 4*60
>       , maxDuration = 6*60
>       , frequency = 34.9
>       , ra = hrs2rad 4.2
>       , dec = deg2rad 17.4
>       , receivers = [[Rcvr26_40]]
>       , grade = 3.0
>       , band = A
>       }
>   ]

> p2sessions = [ makeSession s (windows s) (periods s) | s <- p2sessions' ]
> project2 = makeProject project2' (14*60) (14*60) p2sessions

Utilities:

> findPSessionByName :: String -> Session
> --findPSessionByName name = head $ filter (\s' -> (sName s')==name) (concatMap sessions pTestProjects) 
> findPSessionByName name = head $ findPSessionsByName name

> findPSessionsByName :: String -> [Session]
> findPSessionsByName name = filter (\s' -> (sName s')==name) (concatMap sessions pTestProjects) 

> getPPeriods :: [Period]
> getPPeriods = concat . map periods . concat . map sessions $ pTestProjects

Assumes we have unique names.  

> getPSessionId :: String -> Int
> getPSessionId name = sId $ findPSessionByName name

> getOpenPSessions :: [Session]
> getOpenPSessions = (take 8 ss) ++ (drop ((length ss) - 2) ss)
>     where ss = concatMap sessions pTestProjects

> getWindowedPSessions :: [Session]
> getWindowedPSessions = take 2 . drop 8 $ ss
>     where ss = concatMap sessions pTestProjects
