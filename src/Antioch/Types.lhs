> module Antioch.Types where

> import Data.Time.Clock

> type DateTime = UTCTime

> type Minutes = Int
> type Score   = Float

> data Receiver = NoiseSource
>               | Rcvr_RRI
>               | Rcvr_342
>               | Rcvr_450
>               | Rcvr_600
>               | Rcvr_800
>               | Rcvr_1070
>               | Rcvr1_2
>               | Rcvr2_3
>               | Rcvr4_6
>               | Rcvr8_10
>               | Rcvr12_18
>               | Rcvr18_22
>               | Rcvr22_26
>               | Rcvr26_40
>               | Rcvr40_52
>               | Rcvr_PAR
>               | Zpectrometer
>               | Holography


> data Grade = GradeA | GradeB | GradeC

> data Session = Session {
>       sName          :: String
>     , project        :: Project
>     , totalTime      :: Minutes
>     , totalUsed      :: Minutes
>     , minDuration    :: Minutes
>     , maxDuration    :: Minutes
>     , timeBetween    :: Minutes
>     , frequency      :: Float
>     , ra             :: Float
>     , dec            :: Float
>     , backup         :: Bool
>     , receivers      :: [Receiver]
>     , enabled        :: Bool
>     , authorized     :: Bool
>     , grade          :: Grade
>   }

> data Project = Project {
>       pName          :: String
>     , semester       :: String
>     , sessions       :: [Session]
>     , thesis         :: Bool
>     , timeLeft       :: Minutes
>     , timeTotal      :: Minutes
>   }

> data Period  = Period  {
>       session       :: Session
>     , startTime     :: UTCTime
>     , duration      :: Minutes
>     , score         :: Score
>   }
