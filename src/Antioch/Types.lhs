> module Antioch.Types where

> import Data.Time.Clock

> type Minutes = Int

> data Receiver = Rcvr12_18 | Rcvr18_20

> data Grade = GradeA | GradeB | GradeC

> data Session = Session {
>       sName          :: String
>     , project        :: Project
>     , totalTime      :: Minutes
>     , totalUsed      :: Minutes
>     , minDuration    :: Minutes
>     , maxDuration    :: Minutes
>     , timeBetween    :: Minutes
>     , frequency      :: Double
>     , ra             :: Double
>     , dec            :: Double
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
>     , score         :: Double
>   }
