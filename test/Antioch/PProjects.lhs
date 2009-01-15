> module Antioch.PProjects
>     (
>       pTestProjects
>     ) where

> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Utilities

> project1' = defaultProject {
>     pName = "TestDB"
>   , semester = "06C"
>   , timeLeft = 28740
>   , timeTotal = 33812
> }

> p1sessions'' = [
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
>       , receivers = [Rcvr26_40]
>       , band = A
>       }
>
>   , defaultSession {
>         sId = 2
>       , sName = "CV"
>       , periods = []
>       , totalTime = 60*60
>       , minDuration = 12*60
>       , maxDuration = 8*60
>       , frequency = 4.3
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 30.2
>       , receivers = [Rcvr4_6]
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
>       , receivers = [Rcvr4_6]
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
>       , receivers = [Rcvr18_22]
>       , band = K
>       }
>
>   , defaultSession {
>         sId = 5
>       , sName = "VA"
>       , totalTime = 30*60
>       , minDuration = 4*60
>       , maxDuration = 6*60
>       , frequency = 17.8
>       , ra = hrs2rad 19.9
>       , dec = deg2rad 15.5
>       , receivers = [Rcvr18_22]
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
>       , receivers = [Rcvr26_40]
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
>       , receivers = [Rcvr_450]
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
>       , totalUsed = 6*60
>       , minDuration = 2*60
>       , maxDuration = 6*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [Rcvr_PAR]
>       , band = W
>       }
>
>   , defaultSession {
>         sId = 9
>       , sName = "TestFixed1"  -- no fixed yet
>       , periods = [
>             defaultPeriod {
>                 startTime = fromGregorian 2006 9 1 18 0 0
>               , duration = 6*60 }
>           ]
>       , totalTime = 6*60
>       , totalUsed = 6*60
>       , minDuration = 6*60
>       , maxDuration = 6*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [Rcvr_PAR]
>       , band = W
>       }
>
>   , defaultSession {
>         sId = 10
>       , sName = "TestFixed2"  -- no fixed yet
>       , periods = [
>             defaultPeriod {
>                 startTime = fromGregorian 2006 9 4 11 0 0
>               , duration = 6*60 }
>           ]
>       , totalTime = 4*60
>       , totalUsed = 4*60
>       , minDuration = 4*60
>       , maxDuration = 4*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [Rcvr_PAR]
>       , band = W
>       }
>
>   , defaultSession {
>         sId = 11
>       , sName = "TestFixed3"  -- no fixed yet
>       , periods = [
>             defaultPeriod {
>                 startTime = fromGregorian 2006 9 5 1 0 0
>               , duration = 5*60 }
>           ]
>       , totalTime = 5*60
>       , totalUsed = 5*60
>       , minDuration = 5*60
>       , maxDuration = 5*60
>       , frequency = 67.8
>       , ra = hrs2rad 12.3
>       , dec = deg2rad 10.4
>       , receivers = [Rcvr_PAR]
>       , band = W
>       }
>   ]

> p1sessions' = [ makeSession s (periods s) | s <- p1sessions'' ]
> project1 = makeProject project1' p1sessions'

> pTestProjects = [project1]
