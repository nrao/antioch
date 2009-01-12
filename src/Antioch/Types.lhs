> module Antioch.Types where

> import Antioch.DateTime
> import Data.Function (on)

> type Frequency = Float
> type Minutes   = Int
> type Score     = Float
> type Radians   = Float

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
>               deriving (Eq, Show)

> data Grade = GradeA | GradeB | GradeC deriving (Eq, Show)
> data Band = L | S | C | X | U | K | A | Q
>           deriving (Enum, Eq, Read, Show)

> data Session = Session {
>     sName       :: String
>   , project     :: Project
>   , periods     :: [Period]
>   , totalTime   :: Minutes
>   , totalUsed   :: Minutes
>   , minDuration :: Minutes
>   , maxDuration :: Minutes
>   , timeBetween :: Minutes
>   , frequency   :: Float
>   , ra          :: Radians
>   , dec         :: Radians
>   , backup      :: Bool
>   , receivers   :: [Receiver]
>   , enabled     :: Bool
>   , authorized  :: Bool
>   , grade       :: Grade
>   , band        :: Band
>   } deriving (Eq, Show)

> data Project = Project {
>     pName     :: String
>   , semester  :: String
>   , sessions  :: [Session]
>   , thesis    :: Bool
>   , timeLeft  :: Minutes
>   , timeTotal :: Minutes
>   } deriving Eq

> instance Show Project where
>     show p = "Project: " ++ pName p ++ ", " ++ semester p ++ " Time: ("++ (show . timeTotal $ p) ++ ", " ++ (show . timeLeft $ p) ++ ") Sessions: " ++ show [ totalTime s | s <- sessions p] ++ ", " ++  show [ totalUsed s | s <- sessions p]

> data Period  = Period  {
>     session   :: Session
>   , startTime :: DateTime
>   , duration  :: Minutes
>   , pScore    :: Score
>   } deriving (Eq, Show)

> instance Ord Period where
>     (<) = (<) `on` startTime

> defaultSession = Session {
>     sName       = ""
>   , project     = defaultProject 
>   , periods     = [defaultPeriod]
>   , totalTime   = 0
>   , totalUsed   = 0
>   , minDuration = 0
>   , maxDuration = 0
>   , timeBetween = 0
>   , frequency   = 0.0
>   , ra          = 0.0
>   , dec         = 0.0
>   , backup      = False
>   , receivers   = [Rcvr12_18]
>   , enabled     = False
>   , authorized  = False
>   , grade       = GradeA
>   , band        = L
>   }

> defaultProject = Project {
>     pName     = ""
>   , semester  = ""
>   , sessions  = [defaultSession]
>   , thesis    = False
>   , timeLeft  = 0
>   , timeTotal = 0
>   }

> defaultPeriod = Period {
>     session   = defaultSession
>   , startTime = fromGregorian' 2008 1 1
>   , duration  = 0
>   , pScore    = 0.0
>   }
