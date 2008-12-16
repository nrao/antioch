> module Antioch.Types where

> import Data.Time.Calendar  (fromGregorian)
> import Data.Time.Clock     (UTCTime(..), secondsToDiffTime)

> type DateTime = UTCTime

> instance Show UTCTime where
>     show _ = "datetime"

> gimmeTime :: Integer -> Int -> Int -> Integer -> DateTime
> gimmeTime year month day secs = UTCTime (fromGregorian year month day) (secondsToDiffTime secs)

> type Minutes = Int

> data Receiver = Rcvr12_18 | Rcvr18_20 deriving Show

> data Grade = GradeA | GradeB | GradeC deriving Show

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
>   } deriving Show

> data Project = Project {
>       pName          :: String
>     , semester       :: String
>     , sessions       :: [Session]
>     , thesis         :: Bool
>     , timeLeft       :: Minutes
>     , timeTotal      :: Minutes
>   } deriving Show

> data Period  = Period  {
>       session       :: Session
>     , startTime     :: UTCTime
>     , duration      :: Minutes
>     , score         :: Double
>   } deriving Show

> defaultSession = Session {
>       sName   = ""
>     , project        = defaultProject 
>     , totalTime      = 0
>     , totalUsed      = 0
>     , minDuration    = 0
>     , maxDuration    = 0
>     , timeBetween    = 0
>     , frequency      = 0.0
>     , ra             = 0.0
>     , dec            = 0.0
>     , backup         = False
>     , receivers      = [Rcvr12_18]
>     , enabled        = False
>     , authorized     = False
>     , grade          = GradeA
>   }

> defaultProject = Project {
>       pName          = ""
>     , semester       = ""
>     , sessions       = [defaultSession]
>     , thesis         = False
>     , timeLeft       = 0
>     , timeTotal      = 0
>   }

> defaultPeriod = Period {
>       session       = defaultSession
>     , startTime     = UTCTime (fromGregorian 2008 1 1) (secondsToDiffTime 0)
>     , duration      = 0
>     , score         = 0.0
>   }
