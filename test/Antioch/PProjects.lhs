> module Antioch.PProjects
>     (
>       pTestProjects
>     , findPSessionByName
>     , findPSessionsByName
>     , getPSessionId
>     , getOpenPSessions
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
>   , timeTotal = 33812
> }

> p1sessions' = [
>     defaultSession {
>         sId = 1
>       , sName = "GB"
>       , periods = []
>       , totalTime = 80*60
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
>       , totalTime = 60*60
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
>               , duration = 6*60 }
>           ]
>       , totalTime = 40*60
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
>       , totalTime = 40*60
>       , minDuration = 4*60
>       , maxDuration = 6*60
>       , frequency = 17.8
>       , ra = hrs2rad 19.9
>       , dec = deg2rad 15.5
>       , receivers = [[Rcvr18_22]]
>       , band = K
>       }
>
>   , defaultSession {
>         sId = 5
>       , sName = "VA"
>       , totalTime = 30*60
>       , minDuration = 4*60
>       , maxDuration = 6*60
>       , frequency = 22.7
>       , ra = hrs2rad 12.9
>       , dec = deg2rad 29.2
>       , receivers = [[Rcvr22_26]]
>       , band = K
>       }
>
>   , defaultSession {
>         sId = 6
>       , sName = "WV"
>       , totalTime = 120*60
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
>       , totalTime = 40*60
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
>               , duration = 4*60
>               }
>           , defaultPeriod {
>                 startTime = fromGregorian 2006 11 1 20 0 0
>               , duration = 2*60
>               }
>           ]
>       , totalTime = 40*60
>       , minDuration = 2*60
>       , maxDuration = 6*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [[Rcvr_PAR]]
>       , band = W
>       }
>
>   {- TBF: period for Fixed vs. periods for Open, Windowed 
>   , defaultFixed {
>         sId = 9
>       , sName = "TestFixed1"  -- no fixed yet
>       , period =
>             defaultPeriod {
>                 startTime = fromGregorian 2006 9 1 18 0 0
>               , duration = 6*60 }
>       , totalTime = 6*60
>       , minDuration = 6*60
>       , maxDuration = 6*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [Rcvr_PAR]
>       , band = W
>       }
>
>   , defaultFixed {
>         sId = 10
>       , sName = "TestFixed2"  -- no fixed yet
>       , period =
>             defaultPeriod {
>                 startTime = fromGregorian 2006 9 4 11 0 0
>               , duration = 4*60 }
>       , totalTime = 4*60
>       , minDuration = 4*60
>       , maxDuration = 4*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [Rcvr_PAR]
>       , band = W
>       }
>
>   , defaultFixed {
>         sId = 11
>       , sName = "TestFixed3"  -- no fixed yet
>       , period =
>             defaultPeriod {
>                 startTime = fromGregorian 2006 9 5 1 0 0
>               , duration = 5*60 }
>       , totalTime = 5*60
>       , minDuration = 5*60
>       , maxDuration = 5*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [Rcvr_PAR]
>       , band = W
>       }
>   -}
>
>   {-  TBF on hold until actually scoring/scheduling windowed sessions
>   , defaultWindowed {
>         sId = 12
>       , sName = "TestWindowed1"
>       , totalTime = 4*60
>       , minDuration = 4*60
>       , maxDuration = 4*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [Rcvr_PAR]
>       , band = W
>       }
>
>   , defaultWindowed {
>         sId = 13
>       , sName = "TestWindowed2"
>       , totalTime = 6*60
>       , minDuration = 6*60
>       , maxDuration = 6*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [Rcvr_PAR]
>       , band = W
>       }
>
>   , defaultWindowed {
>         sId = 14
>       , sName = "TestWindowed3"
>       , totalTime = 4*60
>       , minDuration = 4*60
>       , maxDuration = 4*60
>       , frequency = 2.0
>       , ra = hrs2rad 17.3
>       , dec = deg2rad 30.4
>       , receivers = [Rcvr2_3]
>       , band = S
>       }
>   -}
>   ]

> p1sessions = [ makeSession s (periods s) | s <- p1sessions' ]
> project1 = makeProject project1' (500*60) p1sessions

> project2' = defaultProject {
>     pId = 2
>   , pName = "TestThesis"
>   , semester = "06A"
>   , thesis = True
>   -- , timeLeft = 14*60
>   -- , timeTotal = 14*60
> }

> p2sessions' = [
>     defaultSession {
>         sId = 15
>       , sName = "GB_thesis"
>       , periods = []
>       , totalTime = 80*60
>       , minDuration = 2*60
>       , maxDuration = 8*60
>       , frequency = 27.5
>       , ra = hrs2rad 6.4
>       , dec = deg2rad 10.4
>       , receivers = [[Rcvr26_40]]
>       , grade = GradeB
>       , band = A
>       }
>
>   , defaultSession {
>         sId = 16
>       , sName = "WV_thesis"
>       , periods = []
>       , totalTime = 120*60
>       , minDuration = 4*60
>       , maxDuration = 6*60
>       , frequency = 34.9
>       , ra = hrs2rad 4.2
>       , dec = deg2rad 17.4
>       , receivers = [[Rcvr26_40]]
>       , grade = GradeB
>       , band = A
>       }
>   ]

> p2sessions = [ makeSession s (periods s) | s <- p2sessions' ]
> project2 = makeProject project2' (14*60) p2sessions

Utilities:

> findPSessionByName :: String -> Session
> --findPSessionByName name = head $ filter (\s' -> (sName s')==name) (concatMap sessions pTestProjects) 
> findPSessionByName name = head $ findPSessionsByName name

> findPSessionsByName :: String -> [Session]
> findPSessionsByName name = filter (\s' -> (sName s')==name) (concatMap sessions pTestProjects) 

Assumes we have unique names.  

> getPSessionId :: String -> Int
> getPSessionId name = sId $ findPSessionByName name

> getOpenPSessions :: [Session]
> getOpenPSessions = (take 8 ss) ++ (drop ((length ss) - 2) ss)
>     where ss = concatMap sessions pTestProjects
