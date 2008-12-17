> module Antioch.Types where

> import Antioch.DateTime
> import Data.Time.Calendar  (fromGregorian)
> import Data.Time.Clock     (UTCTime(..), secondsToDiffTime)

> gimmeTime :: Integer -> Int -> Int -> Integer -> DateTime
> gimmeTime year month day secs = UTCTime (fromGregorian year month day) (secondsToDiffTime secs)

> type Minutes = Int
> type Score   = Float
> type Radians = Float

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
>               deriving Show

> data Grade = GradeA | GradeB | GradeC deriving Show

> data Session = Session {
>       sName          :: String
>     , project        :: Project
>     , totalTime      :: Minutes
>     , totalUsed      :: Minutes
>     , minDuration    :: Minutes
>     , maxDuration    :: Minutes
>     , timeBetween    :: Minutes
>     , frequency      :: Float
>     , ra             :: Radians
>     , dec            :: Radians
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
>   } 

> instance Show Project where
>     show p = pName p

> data Period  = Period  {
>       session       :: Session
>     , startTime     :: UTCTime
>     , duration      :: Minutes
>     , score         :: Score
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
