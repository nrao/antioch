> module Antioch.Types where

> import Antioch.DateTime
> import Data.Function (on)
> import Data.List     (find)
> import Data.Ix

> type Frequency = Float   -- GHz
> type Minutes   = Int
> type Score     = Float
> type Radians   = Float
> type Grade     = Float
> type SemesterName  = String

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
>               | RcvrArray18_26
>               deriving (Ord, Enum, Eq, Show, Read)

Sessions store their desired Rcvrs in Conjugate Normal Form (CNF).
Ex: [K or L] and [K or S], or [[Receiver]].  In this form, all
'or' receiver groups must evaluate as True.

> type ReceiverGroup = [Receiver]

Note: some of the bands specified below are simply for our own purposes,
such as: P

> data Band = P | L | S | C | X | U | K | A | Q | W
>           deriving (Bounded, Enum, Eq, Ix, Ord, Read, Show)
> data SessionType = Open | Fixed | Windowed | Elective deriving (Eq, Show, Read)
> data TransitType = Optional | Partial | Center deriving (Eq, Show, Read)
> data StateType = Pending | Scheduled | Deleted | Complete deriving (Eq, Show, Read)
> data ObservingType = Radar | Vlbi | Pulsar | Continuum | SpectralLine | Maintenance | Calibration | Testing | Commissioning deriving (Ord, Eq, Show, Read)

TBF: Initially, Open, Fixed, and Windowed all share the same contents.
Ideally, we need to evolve these as we go and add new items and remove
useless items. Until the need arises to use different types, we will
use a single data structure for all sessions.

> data Session = Session {
>     sId         :: Int
>   , sName       :: String
>   , sAllottedT  :: Minutes
>   , sAllottedS  :: Minutes
>   , sClosed     :: Bool
>   , project     :: Project
>   , windows     :: [Window]
>   , electives   :: [Electives]
>   , periods     :: [Period]
>   , minDuration :: Minutes
>   , maxDuration :: Minutes
>   , timeBetween :: Minutes
>   , frequency   :: Frequency
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
>   , oType       :: ObservingType
>   , transit     :: TransitType
>   , xi          :: Float
>   , elLimit     :: Maybe Radians 
>   , guaranteed  :: Bool
>   } deriving Show

> instance Eq Session where
>     (==) = (==) `on` sId

> instance Ord Session where
>     compare = compare `on` sId

> data Window  = Window {
>     wId          :: Int
>   , wSession     :: Session    -- assigned for simulations only
>   , wRanges      :: [DateRange]
>   , wPeriodId    :: Maybe Int  -- default period id  
>   , wComplete    :: Bool       -- requires more observing or not
>   , wTotalTime   :: Minutes    -- time allotted
>    }

> instance Show Window where
>     show w = "Window for: " ++ printName w ++ " from " ++ toSqlString (wStart w) ++ " to " ++ toSqlString (wEnd w) ++ " (for " ++ show (flip div (24*60) . wDuration $ w) ++ " days) Cmp: " ++ (show . wComplete $ w) ++ " Time: " ++ (show . wTotalTime $ w) -- ++ "; wPeriods: " ++ show (wPeriods (wSession w) w)
>       where 
>         n = sName . wSession $ w
>         printName w = if n == "" then show . sId . wSession $ w else n

> wStart :: Window -> DateTime
> wStart w = minimum $ map fst $ wRanges w

> wEnd :: Window -> DateTime
> wEnd w = maximum $ map snd $ wRanges w

> wDuration :: Window -> Minutes
> wDuration w = diffMinutes (wEnd w) (wStart w)

> instance Ord Window where
>     (<) = (<) `on` wStart
>     (>) = (>) `on` wStart
>     (<=) = (<=) `on` wStart
>     (>=) = (>=) `on` wStart

> instance Eq Window where
>     (==) = windowsEqual

> isVlbi :: Session -> Bool
> isVlbi s = (oType s) == Vlbi

> getOverhead :: Session -> Int
> getOverhead s = if isVlbi s
>                 then 2
>                 else 1

Relies on wPeriodId, and therefore only works for real data (simulated
data does not have periods with unique ids).

> wPeriod :: Window -> Maybe Period
> wPeriod w = find (\p -> (wPeriodId w) == Just (peId p)) (periods . wSession $ w)

> hasWindows :: Session -> Bool
> hasWindows s = (sType s) == Windowed && (windows s) /= []

> windowsEqual :: Window -> Window -> Bool
> windowsEqual w1 w2 = eqIds w1 w2 &&
>                      eqStarts w1 w2 &&
>                      eqDurs w1 w2 
>   where
>     eqIds    = (==) `on` wSession
>     eqStarts = (==) `on` wStart
>     eqDurs   = (==) `on` wDuration

Electives are just a means of grouping some periods.  But we do need
an easy way of determining if a period is the last period in an elective,
in case the Sesshun does not have gauranteed time.

> data Electives = Electives {
>     eId        :: Int -- DB PK
>   , eComplete  :: Bool
>   , ePeriodIds :: [Int] -- PK's of periods, sorted by ASC startTime
> } deriving Eq

> instance Show Electives where
>     show e = "Elective (" ++ (show . eId $ e) ++ ")"

Need to calculate a windowed session's opportunities from its observation details.

Tying the knot.

> makeSession      :: Session -> [Window] -> [Period] -> Session
> makeSession s ws ps = s'
>   where
>     s' = s { windows = map (\w -> w { wSession = s'}) ws
>            , periods = map (\p -> p { session = s' }) ps
>            }

> updateSession      :: Session -> [Period] -> Session
> updateSession s ps = makeSession s (windows s) $ periods s ++ ps

> data Project = Project {
>     pId             :: !Int
>   , pName           :: !String
>   , pAllottedT      :: !Minutes
>   , pAllottedS      :: !Minutes
>   , pClosed         :: !Bool
>   , semester        :: !String
>   , sessions        :: [Session]
>   , thesis          :: !Bool
>   , observers       :: [Observer]
>   , requiredFriends :: [Observer]
>   , pBlackouts      :: [DateRange]
>   } deriving Eq

> makeProject :: Project -> Minutes -> Minutes -> [Session] -> Project
> makeProject p tt st ss = p'
>   where
>     p' = p { pAllottedT = tt, pAllottedS = st, sessions = map (\s -> s { project = p' }) ss }

> instance Show Project where
>     show p = "Project: " ++ pName p ++ ", " ++ semester p ++ " Time: ("++ (show . pAllottedT $ p) ++ ") Sessions: " ++ show [ sAllottedT s | s <- sessions p] 

> type DateRange = (DateTime, DateTime)

> inDateRange :: DateTime -> DateRange -> Bool
> inDateRange dt r = start < dt && dt < end
>   where
>     start = fst r
>     end   = snd r

> data Observer = Observer {
>     oId          :: Int
>   , firstName    :: String -- for debugging
>   , lastName     :: String -- for debugging
>   , pstId        :: Int
>   , sanctioned   :: Bool
>   , reservations :: [DateRange]
>   , blackouts    :: [DateRange]
> } deriving (Eq, Show, Read)

> data Period  = Period  {
>     peId        :: Int
>   , session     :: Session
>   , startTime   :: DateTime
>   , duration    :: Minutes    -- assigned time
>   , pScore      :: Score      -- Average forecasted score
>   , pState      :: StateType
>   , pForecast   :: DateTime
>   , pBackup     :: Bool
>   , pDuration   :: Minutes    -- billed time
>   } 

> instance Show Period where
>     show p = "Period: " ++ printName p ++ " (" ++ show (peId p) ++ ") " ++
>              " at " ++ toSqlString (startTime p) ++
>              " for " ++ show (duration p) ++
>              " (" ++ show (pDuration p) ++
>              ") with score of " ++ show (pScore p) ++
>              " from " ++ (toSqlString . pForecast $ p) ++
>              " " ++ show (pState p) ++
>              "  band: " ++ (show . band . session $ p) ++
>              "  RA: " ++ (show . (\x -> 12*x/pi) . ra . session $ p) ++
>              "  grade: " ++ (show . grade . session $ p)
>       where 
>         n = sName . session $ p
>         printName p = if n == "" then show . sId . session $ p else n

> instance Ord Period where
>     (<) = (<) `on` startTime
>     (>) = (>) `on` startTime
>     (<=) = (<=) `on` startTime
>     (>=) = (>=) `on` startTime

TBF: Until scoring settles down, we want an equality operator for periods that
ignores their numerical scores.  Note that equality between different periods
is slightly arbitrary.

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
> periodHalfTime p = addMinutes (duration p `div` 2) $ startTime p

> periodEndTime :: Period -> DateTime
> periodEndTime p = addMinutes (duration p) $ startTime p

> defaultSession = Session {
>     sId         = 0
>   , sName       = ""
>   , project     = defaultProject 
>   , windows     = []
>   , electives   = []
>   , periods     = [defaultPeriod]
>   , sAllottedT  = 0
>   , sAllottedS  = 0
>   , minDuration = 0
>   , maxDuration = 0
>   , timeBetween = 0
>   , frequency   = 0.0
>   , ra          = 0.0
>   , dec         = 0.0
>   , backup      = False
>   , receivers   = [] -- e.g., [[Rcvr12_18]]
>   , enabled     = True
>   , authorized  = True
>   , grade       = 4.0
>   , band        = L
>   , lowRFI      = False
>   , lstExclude  = []
>   , sClosed     = False
>   , sType       = Open
>   , oType       = SpectralLine
>   , transit     = Optional
>   , xi          = 1.0
>   , elLimit     = Nothing
>   , guaranteed  = True 
>   }

> defaultObserver = Observer {
>     oId          = 0 
>   , firstName    = "" -- for debugging
>   , lastName     = "" -- for debugging
>   , pstId        = 0 
>   , sanctioned   = True
>   , reservations = []
>   , blackouts    = []
> }

> defaultProject = Project {
>     pId             = 0
>   , pName           = ""
>   , semester        = ""
>   , sessions        = [defaultSession]
>   , thesis          = False
>   , pAllottedT      = 0
>   , pAllottedS      = 0
>   , observers       = [defaultObserver]
>   , requiredFriends = []
>   , pBlackouts      = []
>   , pClosed         = False
>   }

> defaultStartTime = fromGregorian' 2008 1 1

> defaultPeriod = Period {
>     peId        = 0
>   , session     = defaultSession
>   , startTime   = defaultStartTime
>   , duration    = 0
>   , pScore      = 0.0
>   , pState      = Pending
>   , pForecast   = fromGregorian' 2008 1 1
>   , pBackup     = False
>   , pDuration   = 0
>   }

> defaultWindow  = Window {
>     wId          = 0
>   , wSession     = defaultSession
>   , wRanges      = []
>   , wPeriodId    = Nothing
>   , wComplete    = False
>   , wTotalTime   = 0
>    }
