> module Antioch.Types where

> import Antioch.DateTime
> import Data.Function (on)
> import Data.Ix

> type Frequency = Float
> type Minutes   = Int
> type Score     = Float
> type Radians   = Float

> quarter = 15 :: Minutes

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
>               -- | Rcvr18_22
>               -- | Rcvr22_26 -- these 2 rcvrs wrapped in one like in DB 
>               | Rcvr18_26
>               | Rcvr26_40
>               | Rcvr40_52
>               | Rcvr_PAR
>               | Zpectrometer
>               | Holography
>               deriving (Eq, Show, Read)

Sessions store their desired Rcvrs in Conjugate Normal Form (CNF).
Ex: [K or L] and [K or S], or [[Receiver]].  In this form, all
'or' receiver groups must evaluate as True.

> type ReceiverGroup = [Receiver]

> data Grade = GradeC | GradeB | GradeA deriving (Eq, Ord, Show, Read)
> data Band = L | S | C | X | U | K | A | Q | W
>           deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)
> data SessionType = Open | Fixed | Windowed deriving (Eq, Show, Read)
> data TransitType = Optional | Partial | Center deriving (Eq, Show, Read)

TBF: Initially, Open, Fixed, and Windowed all share the same contents.
Ideally, we need to evolve these as we go and add new items and remove
useless items. Until the need arises to use different types, we will
use a single data structure for all sessions.

> data Session = Session {
>     sId         :: Int
>   , sName       :: String
>   , sAlloted    :: Minutes
>   , sClosed     :: Bool
>   , project     :: Project
>   , periods     :: [Period]
>   , minDuration :: Minutes
>   , maxDuration :: Minutes
>   , timeBetween :: Minutes
>   , frequency   :: Float
>   , ra          :: Radians
>   , dec         :: Radians
>   , backup      :: Bool
>   , receivers   :: [ReceiverGroup]
>   , enabled     :: Bool
>   , authorized  :: Bool
>   , grade       :: Grade
>   , band        :: Band
>   , lowRFI      :: Bool
>   , lstExclude  :: [(Float, Float)]
>   , sType       :: SessionType
>   , transit     :: TransitType
>   } deriving Show


> instance Eq Session where
>     (==) = (==) `on` sId

> instance Ord Session where
>     compare = compare `on` sId

> --periods' s@(Fixed { }) = [period s]
> periods' s             = periods s

Need to calculate a windowed session's opportunities from its observation details.

> opportunities s@(Windowed { }) = []
> opportunities _                = []

Tying the knot.

> makeSession      :: Session -> [Period] -> Session
> --makeSession s@(Fixed { }) [p] = s'
> --  where
> --    s' = s { period = p { session = s' } }
> --    t  = duration p
> makeSession s ps = s'
>   where
>     s' = s { periods = map (\p -> p { session = s' }) ps }

> updateSession      :: Session -> [Period] -> Session
> updateSession s ps = makeSession s $ periods' s ++ ps

> data Project = Project {
>     pId             :: !Int
>   , pName           :: !String
>   , pAlloted        :: !Minutes
>   , pClosed         :: !Bool
>   , semester        :: !String
>   , sessions        :: [Session]
>   , thesis          :: !Bool
>   , maxSemesterTime :: !Minutes
>   , observers       :: [Observer]
>   } deriving Eq

> makeProject :: Project -> Minutes -> [Session] -> Project
> makeProject p tt ss = p'
>   where
>     p' = p { pAlloted = tt, sessions = map (\s -> s { project = p' }) ss }
>     t  = sum . map sAlloted $ ss

> instance Show Project where
>     show p = "Project: " ++ pName p ++ ", " ++ semester p ++ " Time: ("++ (show . pAlloted $ p) ++ ") Sessions: " ++ show [ sAlloted s | s <- sessions p] 

> type DateRange = (DateTime, DateTime)

> inDateRange :: DateTime -> DateRange -> Bool
> inDateRange dt r = start < dt && dt < end
>   where
>     start = fst r
>     end   = snd r

> data Observer = Observer {
>     sanctioned   :: Bool
>   , reservations :: [DateRange]
>   , blackouts    :: [DateRange]
> } deriving (Eq, Show, Read)

> data Period  = Period  {
>     session   :: Session
>   , startTime :: DateTime
>   , duration  :: Minutes
>   , pScore    :: Score  -- Average forecasted score
>   , pForecast :: DateTime
>   , pBackup   :: Bool
>   } 

> instance Show Period where
>     show p = "Period: " ++ printName p ++ " at " ++ toSqlString (startTime p) ++ " for " ++ show (duration p) ++ " with " ++ show (pScore p)
>       where 
>         n = sName . session $ p
>         printName p = if n == "" then show . sId . session $ p else n

> instance Ord Period where
>     (<) = (<) `on` startTime
>     (>) = (>) `on` startTime
>     (<=) = (<=) `on` startTime
>     (>=) = (>=) `on` startTime

TBF: Until scoring settles down, we want an equality operator for periods that
ignores their numerical scores.

> instance Eq Period where
>     (==) = periodsEqual

> periodsEqual :: Period -> Period -> Bool
> periodsEqual p1 p2 = eqIds p1 p2 &&
>                      eqStarts p1 p2 &&
>                      eqDurs p1 p2
>   where
>     eqIds    = (==) `on` session
>     eqStarts = (==) `on` startTime
>     eqDurs   = (==) `on` duration

Simple Functions for Periods:

> periodHalfTime :: Period -> DateTime
> periodHalfTime p = addMinutes' (duration p `div` 2) $ startTime p

> periodEndTime :: Period -> DateTime
> periodEndTime p = addMinutes' (duration p) $ startTime p

> defaultSession = Session {
>     sId         = 0
>   , sName       = ""
>   , project     = defaultProject 
>   , periods     = [defaultPeriod]
>   , sAlloted   = 0
>   , minDuration = 0
>   , maxDuration = 0
>   , timeBetween = 0
>   , frequency   = 0.0
>   , ra          = 0.0
>   , dec         = 0.0
>   , backup      = False
>   , receivers   = [[Rcvr12_18]]
>   , enabled     = False
>   , authorized  = False
>   , grade       = GradeA
>   , band        = L
>   , lowRFI      = False
>   , lstExclude  = []
>   , sClosed     = False
>   , sType       = Open
>   , transit     = Optional
>   }

> defaultObserver = Observer {
>     sanctioned   = True
>   , reservations = []
>   , blackouts    = []
> }

> defaultProject = Project {
>     pId       = 0
>   , pName     = ""
>   , semester  = ""
>   , sessions  = [defaultSession]
>   , thesis    = False
>   , pAlloted = 0
>   , maxSemesterTime = 10000000 -- more then enough time
>   , observers = [defaultObserver]
>   , pClosed  = False
>   }

> defaultPeriod = Period {
>     session   = defaultSession
>   , startTime = fromGregorian' 2008 1 1
>   , duration  = 0
>   , pScore    = 0.0
>   , pForecast = undefined
>   , pBackup   = False
>   }

