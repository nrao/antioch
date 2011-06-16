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

> module Antioch.Generators where

> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.SLALib  (slaGaleq)
> import Antioch.Utilities
> import Antioch.DateTime
> --import Antioch.Filters (truncateHistory)
> import Data.Char
> import Data.List 
> import Data.Maybe      (isJust, maybeToList, fromJust)
> import System.Random   (getStdGen, setStdGen, mkStdGen)
> import Test.QuickCheck hiding (frequency)
> import qualified Test.QuickCheck as T
> import Control.Monad.RWS.Strict
> import System.IO.Unsafe  (unsafePerformIO)
> import Debug.Trace

> instance Arbitrary Project where
>     arbitrary       = genProject
>     coarbitrary _ b = b

> instance Arbitrary Session where
>     arbitrary       = genSession
>     coarbitrary _ b = b

> instance Arbitrary Period where
>     arbitrary       = genPeriod
>     coarbitrary _ b = b

Generate a random project name: 'A' .. 'Z'

> genProjectName :: Gen Char 
> genProjectName = elements (map chr [65 .. 90])

> genSemesterName :: Int -> Gen String
> genSemesterName year = elements $ semesterChoices year

> semesterChoices :: Int -> [String]
> semesterChoices year = [ s (year-1) "C"] ++ sA ++ sB ++ sC
>   where
>     s y a = (drop 2 $ show y) ++ a
>     sA = take 4 $ repeat (s year "A") --"06A", "06A", "06A", "06A"
>     sB = take 4 $ repeat (s year "B") --"06B", "06B", "06B", "06B"
>     sC = take 3 $ repeat (s year "C") --"06C", "06C", "06C"


trimesterMonth = [C,A,A,A,A,B,B,B,B,3,3,3] 

> genThesis :: Gen Bool
> genThesis = T.frequency [(20, return True), (80, return False)]

> genMaxSemesterTime :: Minutes -> Gen Minutes
> genMaxSemesterTime time = T.frequency [(20, return $ div time 2), (80, return time)]

Generation of arbitrary project, note the pId is set to 0,
unique ids are produced in GenerateSchedule where needed.

> genProject :: Gen Project
> genProject = do
>     genProjectForYear 2006

Generation of not so arbitrary project: specify the year to 
determine semester distrubition.

> genProjectForYear :: Int -> Gen Project
> genProjectForYear year = do
>     name     <- genProjectName
>     semester <- genSemesterName year
>     thesis   <- genThesis
>     sessions <- genProjectSessions
>     let pAllottedT = sum [ sAllottedT s | s <- sessions ]
>     maxST    <- genMaxSemesterTime pAllottedT
>     let project = defaultProject {
>           pName = str name
>         , semester = semester
>         , thesis = thesis
>         , pAllottedS = maxST
>         }
>     return $ makeProject project pAllottedT pAllottedT sessions


Generate n projects.

> genProjects         :: Int -> Gen [Project]
> genProjects 0       = return []
> genProjects (n + 1) = do
>     p  <- genProject 
>     pp <- genProjects n
>     return $ p : pp

> genScheduleProjects :: Gen [Project]
> genScheduleProjects =
>     choose (10, 30) >>= genProjects

Now lets make sure we are properly generating Projects: test each attribute
at a time:

> prop_pName p = "A" <= pName p && pName p <= "Z"
> prop_semester p = any (==(semester p)) ["05C", "06A", "06B", "06C"]

Each Project's Sessions can have a sAllottedT between 2 & 30 hrs.  Currently
a project has between 1 and 5 Sessions.

> prop_sessions p = 1 <= (length . sessions $ p) && (length . sessions $ p) <= 5
> prop_pAllottedT p = (1*2*60) <= pAllottedT p && pAllottedT p <= (5*30*60)

> prop_pAllottedTQuarter p = pAllottedT p `mod` quarter == 0

Each Session can have 0-3 Periods, each with a max of 10 hours:

> prop_projectPeriods p = let n = sum [length . periods $ s | s <- sessions p] in 0 <= n && n <= 5*3

> prop_pCommittedT p = 0 <= pCommittedT p && pCommittedT p <= pAllottedT p

choose LST range and declination
s - single sources or few sources in one area of the sky
    g - galactic plane (some near GC)
    e - extra galactic
a - all sky or a large region of the sky

> skyType = elements "geegeegeeaaa"  -- sssa, s <- gee

Generates RA and Dec based on skyType:
    RA in radians
    Dec in degrees

> genRaDec 'g' = T.frequency [(20, galacticCenter), (80, galactic)]
>   where
>     galacticCenter = do
>         dec <- choose (-27.0, -29.0)
>         return (hrs2rad 18.0, deg2rad dec)
>     galactic = do
>         longitude <- choose (0.0, 250.0)
>         return $ slaGaleq (deg2rad longitude) 0.0
> genRaDec _   = do
>     ra  <- choose (0.0, 2*pi)
>     dec <- fmap asin . choose $ (sin . deg2rad $ -35.0, sin . deg2rad $ 90.0)
>     return (ra, dec)

> round2quarter :: Minutes -> Minutes
> round2quarter m = m - (m `mod` quarter)

Only 20 percent of the low freq. sessions are backups

> genBackupFlag :: Float -> Gen Bool
> genBackupFlag freq =
>   if freq > 10.0 then T.frequency [(100, return False)] 
>            else T.frequency [(25, return True), (75, return False)]

> genMinTP freq = 
>   if freq > 18.0 then choose (3*60, 3*60)
>            else choose (3*60, 6*60)

> genMaxTP freq = 
>   if freq > 18.0 then choose (12*60, 12*60)
>            else choose (11*60, 12*60)

Backup sessions should not use a time between

> genTimeBetween :: Bool -> Gen Minutes
> genTimeBetween backup = if backup then return 0 else T.frequency [(98, return 0)
>                             , (1, return (12*60))
>                             , (1, return (48*60))]

> genLowRFIFlag :: Gen Bool
> genLowRFIFlag = T.frequency [(96, return False), (4, return True)]

Backup sessions should not use a transit flag 

> genTransitFlag :: Bool -> Gen TransitType
> genTransitFlag backup = if backup then return Optional else T.frequency [(98, return Optional)
>                             , (1, return Partial)
>                             , (1, return Center)]

> genLSTExclusion :: Gen [(Float, Float)]
> genLSTExclusion = T.frequency [(100, return []), (0, lsts)]
>   where
>     lsts = do 
>       low  <- choose (0.0, 5.0)
>       high <- choose (6.0, 12.0)
>       return $ [(low, high)]

Method for producing a generic Open Session, note the sid is set
to 0, unique ids are produced in GenerateSchedule where needed.

> genSession :: Gen Session
> genSession = do
>     project    <- genProject
>     t          <- genSemester
>     -- first generatre rcvr, then have everything else follow.
>     r          <- genRcvr Open t
>     let b      = receiver2Band r
>     f          <- genFreq' r
>     g          <- genGrade [4.0, 4.0, 3.0, 3.0, 3.0]
>     bk         <- genBackupFlag f
>     s          <- skyType
>     (ra, dec)  <- genRaDec s
>     sAllottedT <- choose (6*60, 30*60)
>     minD       <- genMinTP f
>     maxD       <- genMaxTP f
>     --minD       <- choose (2*60, 6*60)
>     --maxD       <- choose (11*60, 12*60)
>     otype      <- genOType r Open
>     tb         <- genTimeBetween bk
>     lstEx      <- genLSTExclusion
>     lowRFIFlag <- genLowRFIFlag
>     trans      <- genTransitFlag bk
>     return $ defaultSession {
>                  project        = project
>                , band           = b
>                , frequency      = f
>                , ra             = ra
>                , dec            = dec
>                , minDuration    = round2quarter minD
>                , maxDuration    = round2quarter maxD
>                -- This code is used only for strategy scheduleMinDuration.
>                --, sAllottedT     = matchAvTime sAllottedT(round2quarter minD)
>                , sAllottedT      = round2quarter sAllottedT
>                , sAllottedS      = round2quarter sAllottedT
>                , timeBetween    = round2quarter tb
>                , lstExclude     = lstEx
>                , lowRFI         = lowRFIFlag
>                , sType          = Open
>                , oType          = otype
>                , transit        = trans
>                , grade          = g
>                , receivers      = [[r]]
>                , backup         = bk
>                -- default Open Session have one period, want none here
>                , periods        = []
>                }

Method for producing a generic, initial Fixed Session.  Various
fields are modified in GenerateSchedule where more information
is available.

> genSessionFixed :: Gen Session
> genSessionFixed = do
>     project    <- genProject
>     t          <- genSemester
>     -- first generatre rcvr, then have everything else follow.
>     r          <- genRcvr Fixed t
>     let b      = receiver2Band r
>     f          <- genFreq' r
>     g          <- genGrade [4.0, 4.0, 3.0, 3.0, 3.0]
>     let bk     = False
>     s          <- skyType
>     (ra, dec)  <- genRaDec s
>     sAllottedT <- choose (6*60, 30*60)
>     minD       <- genMinTP f
>     maxD       <- genMaxTP f
>     --minD       <- choose (2*60, 6*60)
>     --maxD       <- choose (11*60, 12*60)
>     otype      <- genOType r Fixed
>     tb         <- genTimeBetween bk
>     --lstEx      <- genLSTExclusion
>     lowRFIFlag <- genLowRFIFlag
>     --trans      <- genTransitFlag bk
>     return $ defaultSession {
>                  project        = project
>                , periods        = []
>                , band           = b
>                , frequency      = f
>                , ra             = ra
>                , dec            = dec
>                , minDuration    = round2quarter minD
>                , maxDuration    = round2quarter maxD
>                -- This code is used only for strategy scheduleMinDuration.
>                --, sAllottedT     = matchAvTime sAllottedT(round2quarter minD)
>                , sAllottedT      = round2quarter sAllottedT
>                , sAllottedS      = round2quarter sAllottedT
>                , timeBetween    = 0
>                , lstExclude     = []
>                , lowRFI         = False
>                , transit        = Optional
>                , grade          = g
>                , receivers      = [[r]]
>                , backup         = bk
>                , sType          = Fixed
>                , oType          = otype
>                }

Method for producing a generic, initial Windowed Session.  Various
fields are modified in GenerateSchedule where more information
is available.

> genSessionWindowed :: Gen Session
> genSessionWindowed = do
>     project    <- genProject
>     t          <- genSemester
>     -- first generatre rcvr, then have everything else follow.
>     r          <- genRcvr Windowed t
>     let b      = receiver2Band r
>     f          <- genFreq' r
>     g          <- genGrade [4.0, 4.0, 3.0, 3.0, 3.0]
>     let bk     = False
>     s          <- skyType
>     (ra, dec)  <- genRaDec s
>     sAllottedT <- choose (6*60, 30*60)
>     minD       <- genMinTP f
>     maxD       <- genMaxTP f
>     --minD       <- choose (2*60, 6*60)
>     --maxD       <- choose (11*60, 12*60)
>     otype      <- genOType r Windowed
>     tb         <- genTimeBetween bk
>     --lstEx      <- genLSTExclusion
>     lowRFIFlag <- genLowRFIFlag
>     --trans      <- genTransitFlag bk
>     return $ defaultSession {
>                  project        = project
>                , periods        = []
>                , band           = b
>                , frequency      = f
>                , ra             = ra
>                , dec            = dec
>                , minDuration    = round2quarter minD
>                , maxDuration    = round2quarter maxD
>                -- This code is used only for strategy scheduleMinDuration.
>                --, sAllottedT     = matchAvTime sAllottedT(round2quarter minD)
>                , sAllottedT      = round2quarter sAllottedT
>                , sAllottedS      = round2quarter sAllottedT
>                , timeBetween    = 0
>                , lstExclude     = []
>                , lowRFI         = False
>                , transit        = Optional
>                , grade          = g
>                , receivers      = [[r]]
>                , backup         = bk
>                , sType          = Windowed
>                , oType          = otype
>                }

This is only for use with the scheduleMinDuration strategy.  We want
to use this so that the sAllottedT of a session can be completely scheduled
without leaving behind 'loose change'.  This can happen because this strategy
only schedule Periods of length minDuration.

> matchAvTime :: Int -> Int -> Int
> matchAvTime sAllottedT minDuration = (sAllottedT `div` minDuration) * minDuration

> genProjectSessions :: Gen [Session]
> genProjectSessions = 
>     T.frequency [(30, return 1), (30, return 2), (30, return 3), (5, return 4), (5, return 5)] >>= vector

> genSessions         :: Int -> Gen [Session]
> genSessions 0       = return []
> genSessions (n + 1) = do
>     s  <- genSession
>     ss <- genSessions n
>     return $ s : ss

Assumes a single scalar rcvr group - avoid PF receivers

> prop_Receiver s = if (elem rcvr [NoiseSource .. Rcvr_1070]) then True else (rcvr == band2Receiver (band s))
>   where
>     rcvr = head . head . receivers $ s

> prop_Ra s = 0.0 <= ra s && ra s <= 2 * pi

> prop_Ra2 s = validRA s

> validRA :: Session -> Bool
> validRA s = 0.0 <= ra' && ra' <= 24.0
>   where 
>     ra' = rad2hrs . ra $ s

Make sure that the total time used up by the periods is correct:

> prop_sCommittedT s        = 0 <= sCommittedT s && sCommittedT s <= (3*10*60)
> prop_sAllottedT s         = (2*60) <= sAllottedT s && sAllottedT s <= (30*60)
> prop_sAllottedTQuarter s  = sAllottedT s `mod` quarter == 0
> prop_minDuration s        = (2*60) <= minDuration s && minDuration s <= (6*60)
> prop_minDurationQuarter s = minDuration s `mod` quarter == 0
> prop_maxDuration s        = (11*60) <= maxDuration s && maxDuration s <= (12*60)
> prop_maxDurationQuarter s = maxDuration s `mod` quarter == 0

> prop_Dec s = (-pi) / 2 <= dec s && dec s <= pi / 2

> prop_Dec2 s = validDec s

Lower limit is based on Carl's data where some decs were at -44.0.

> validDec :: Session -> Bool
> validDec s = -45.0 <= dec' && dec' <= 90.0
>   where
>     dec' = rad2deg . dec $ s

> prop_DecDegree s = (-180) <= dec s && dec s <= 180 

> genStartTime :: Gen DateTime
> genStartTime = elements [fromGregorian' 2008 1 1, fromGregorian' 2008 1 2]

Durations for Periods come in 15 minute intervals, and probably aren't smaller
then an hour.  TBD: use T.frequency

> genDuration :: Gen Minutes
> genDuration = do
>     quarters <- choose (1*4, 10*4)
>     return $ quarters * quarter

> genPeriod :: Gen Period
> genPeriod = do
>      session   <- genSession  
>      startTime <- genStartTime
>      duration  <- genDuration
>      let period = defaultPeriod {
>          session   = session
>        , startTime = startTime
>        , duration  = duration
>        , pScore    = 0.0
>        , pState    = Pending
>        , pForecast = startTime
>        , pBackup   = False
>        }
>      return period

> genSessionPeriods :: Gen [Period]
> genSessionPeriods = 
>     T.frequency [(50, return 0), (25, return 1), (20, return 2), (5, return 3)] >>= vector

> genPeriods         :: Int -> Gen [Period]
> genPeriods 0       = return []
> genPeriods (n + 1) = do
>     p  <- genPeriod
>     ps <- genPeriods n
>     return $ p : ps

Make sure Durations are made of 15-minute intervals

> prop_duration p = duration p `mod` quarter == 0

We need to (partially) fill a chunk of time with valid periods.  It would
be good to parameterize this such that we can control the degree to which
the schedule is filled, or randomly select which sessions to generate
periods for, i.e., we can make this more sophisticated.

> genSchedulePeriods :: DateTime -> Minutes -> [Session] -> Gen [Maybe Period]
> genSchedulePeriods starttime schedDur sessions = do
>     -- duration probably between 8 - 24 hours.
>     -- how many periods to stick in there?
>     n <- choose (1, 3)
>     -- try to stick each period in the schedule, unless there's no room;
>     -- make sure that max & min session durations are obeyed, and that
>     -- we don't create1 any dead space from holes < smalles min. session
>     let schedDurs = round2quarter $ schedDur `div` n 
>     let dts = [ (schedDurs * i) `addMinutes` starttime | i <- [0 .. (n-1)]]
>     mapM (genSchedulePeriod (head sessions) schedDurs) dts 

> genSchedulePeriod :: Session -> Minutes -> DateTime -> Gen (Maybe Period)
> genSchedulePeriod sess dur dt = do
>     -- keep it simple: start period at start of time slot
>     -- see if the period will fit in the allotted time
>     -- Note: to avoid 'dead time', allow at least min. sess. time at the end
>     let maxL = dur - (2*60)
>     pDur' <- choose (min maxL (minDuration sess), min maxL (maxDuration sess))
>     let pDur = round2quarter pDur' --quarter * (pDur' `div` quarter)
>     let fits = minDuration sess <= maxL
>     return $ if not fits then Nothing else Just $ Period 0 sess dt pDur 0.0 Pending dt False pDur

Check this generator itself: make sure the periods adhere to expected properties

> prop_schedulePeriods = forAll (genSessions 100) $ \ss ->
>                       forAll genStartDate $ \starttime ->
>                       forAll genScheduleDuration $ \dur ->
>                       forAll (genSchedulePeriods starttime dur ss) $ \ps ->
>   let ps' = concatMap maybeToList ps in length ps' < 4 && 
>                                         not (internalConflicts ps') &&
>                                         obeyMinSeparation ps' (2*60)

instance Ord t => Span (Interval t) where
    (Interval s1 e1) `conflict` (Interval s2 e2) = s1 < e2 && s2 < e15

> endTime p = duration p `addMinutes` startTime p

> overlap p1 p2 = s1 < e2 && s2 < e1
>   where
>     [s1, s2] = map startTime [p1, p2]
>     [e1, e2] = map endTime [p1, p2]

> overlaps y = isJust . find (overlap y)

> internalConflicts xs = or [x `overlaps` (xs \\ [x]) | x <- xs]

> internalConflicts' xs = [x | x <- xs, x `overlaps` (xs \\ [x])]

> windowConflicts xs = or [x `winOverlaps` (xs \\ [x]) | x <- xs]

> windowConflicts' xs = [x | x <- xs, x `winOverlaps` (xs \\ [x])]

> winOverlap w1 w2 = s1 < e2 && s2 < e1
>   where
>     [s1, s2] = map wStart [w1, w2]
>     [e1, e2] = map wEnd   [w1, w2]

> winOverlaps y = isJust . find (winOverlap y)

> conflicts :: [Period] -> [Period] -> Bool
> conflicts [] ys = False
> conflicts (x:xs) ys | overlaps x (delete x ys) = True
>                     | otherwise = conflicts xs ys

> obeyMinSeparation :: [Period] -> Minutes -> Bool
> obeyMinSeparation ps minSep = dropWhile (>=minSep) (minutesBetween ps) == []

> minutesBetween :: [Period] -> [Minutes]
> minutesBetween []     = []
> minutesBetween (p:[]) = []
> minutesBetween (p:ps) = diffMinutes (startTime (head ps)) (endTime p) : minutesBetween ps

> type Semester = Char
  
> genSemester :: Gen Semester
> -- b = backup, A = Feb - May, B = Jun - Sep, C = Oct - Jan
> genSemester = elements "bAAAABBBBCCCC"

> prop_Semester = forAll genSemester $ \s -> s `elem` ['b', 'A', 'B', 'C']

> str :: a -> [a]
> str = (: [])


choose observing band distribution
average up to trimester 7C

Band  Hours  Percent  Year  alloc
L     700    46.0%    27.6  26
S     130     8.6%     5.2   4
C     110     7.2%     4.3   5
X     120     7.9%     4.7   4
U      90     5.9%     3.5   3
K     230    15.1%     9.1   6
A      60     3.9%     2.3   6
Q      80     5.3%     3.2   6

> genGrade :: [Grade] -> Gen Grade
> genGrade = elements

Deprecated: now we specify the band from the receiver.

> band2Receiver :: Band -> Receiver
> band2Receiver P = Rcvr_1070 -- represents all prime focus receivers

> band2Receiver L = Rcvr1_2
> band2Receiver S = Rcvr2_3
> band2Receiver C = Rcvr4_6
> band2Receiver X = Rcvr8_10
> band2Receiver U = Rcvr12_18
> band2Receiver K = Rcvr18_26 -- Rcvr18_22 and Rcvr18_22
> band2Receiver A = Rcvr26_40
> band2Receiver Q = Rcvr40_52
> band2Receiver W = Rcvr_PAR

> receiver2Band :: Receiver -> Band
> receiver2Band Rcvr_RRI = P
> receiver2Band Rcvr_342 = P
> receiver2Band Rcvr_450 = P
> receiver2Band Rcvr_600 = P
> receiver2Band Rcvr_800 = P
> receiver2Band Rcvr_1070 = L
> receiver2Band Rcvr1_2 = L
> receiver2Band Rcvr2_3 = S
> receiver2Band Rcvr4_6 = C
> receiver2Band Rcvr8_10 = X
> receiver2Band Rcvr12_18 = U
> receiver2Band Rcvr18_26 = K 
> receiver2Band Rcvr26_40 = A
> receiver2Band Rcvr40_52 = Q
> receiver2Band Rcvr_PAR = W
> receiver2Band Holography = U
> receiver2Band RcvrArray18_26 = K 

This 'code' is only of use in the 'genRcvr' method, where we simply
need a one character code to identity each receiver

> code2Receiver :: String -> Receiver
> code2Receiver "R" = Rcvr_RRI 
> code2Receiver "3" = Rcvr_342 
> code2Receiver "4" = Rcvr_450 
> code2Receiver "6" = Rcvr_600 
> code2Receiver "8" = Rcvr_800
> code2Receiver "1" = Rcvr_1070
> code2Receiver "L" = Rcvr1_2
> code2Receiver "S" = Rcvr2_3
> code2Receiver "C" = Rcvr4_6
> code2Receiver "X" = Rcvr8_10
> code2Receiver "U" = Rcvr12_18
> code2Receiver "K" = Rcvr18_26 
> code2Receiver "A" = Rcvr26_40
> code2Receiver "Q" = Rcvr40_52
> code2Receiver "W" = Rcvr_PAR
> code2Receiver "H" = Holography
> code2Receiver "F" = RcvrArray18_26

Deprecated: now we specifiy the band from the receiver

> genBand     :: Int -> Gen Band
> genBand sem = fmap (read . str) . elements $ bands !! sem
>   where
>     bands = [ "KKQQAAXUCCSLLLLLLLLL"  -- 0 => backup
>             , "KKKQQAAXUCCSSLLLLLLL"  -- 1
>             , "KQQAXUCSLLLLLLLLLLLL"  -- 2
>             , "KKQQAAAXXUCCSLLLLLLL"  -- 3
>             ]

> genRcvr :: SessionType -> Char -> Gen Receiver
> genRcvr sType sem = fmap (code2Receiver . str) . elements $ band
>   where
>     band = fromJust . lookup (sType, sem) $ bands
>     bands = [ ((Open,'b'),      "38WWKKKQAAAXUUCCSLLL")
>             , ((Open,'A'),      "38WWKKKQAAAXXUCCSSLL")
>             , ((Open,'B'),      "38WKQQAAXUUCSLLLLLLL")
>             , ((Open,'C'),      "38WWKKKQQQAAAXUCCSLL")
>             , ((Fixed,'b'),     "333388XXUCCSSSLLLLLL")
>             , ((Fixed,'A'),     "333388XXUCCSSSLLLLLL")
>             , ((Fixed,'B'),     "333388XXUCCSSSSLLLLL")
>             , ((Fixed,'C'),     "333388XXUCCSSSLLLLLL")
>             , ((Windowed,'b'),  "333388KXXXUCSSSLLLLL")
>             , ((Windowed,'A'),  "333388KXXXUCSSSLLLLL")
>             , ((Windowed,'B'),  "333388XXXUCSSSSLLLLL")
>             , ((Windowed,'C'),  "333388KXXXUCSSSLLLLL")
>             ]

> genOType :: Receiver -> SessionType -> Gen ObservingType
> genOType rcvr sType = T.frequency [(p,       return SpectralLine)
>                                  , (100 - p, return Continuum)]
>   where
>    o = Open
>    f = Fixed
>    w = Windowed
>    -- returns the percentage of observing type SpectralLine
>    p = fromJust . lookup (rcvr, sType) $ types
>    types = [
>        --            Open                   Fixed                  Windowed
>        ((Rcvr_RRI,   o),100), ((Rcvr_RRI,   f),100), ((Rcvr_RRI,   w),100)
>      , ((Rcvr_342,   o),100), ((Rcvr_342,   f),100), ((Rcvr_342,   w),100)
>      , ((Rcvr_450,   o),100), ((Rcvr_450,   f),100), ((Rcvr_450,   w),100)
>      , ((Rcvr_600,   o),100), ((Rcvr_600,   f),100), ((Rcvr_600,   w),100)
>      , ((Rcvr_800,   o),100), ((Rcvr_800,   f),100), ((Rcvr_800,   w),100)
>      , ((Rcvr_1070,  o),100), ((Rcvr_1070,  f),100), ((Rcvr_1070,  w),100)
>      , ((Rcvr1_2,    o),100), ((Rcvr1_2,    f),100), ((Rcvr1_2,    w),100)
>      , ((Rcvr2_3,    o),100), ((Rcvr2_3,    f),100), ((Rcvr2_3,    w),100)
>      , ((Rcvr4_6,    o),100), ((Rcvr4_6,    f),100), ((Rcvr4_6,    w),100)
>      , ((Rcvr8_10,   o),100), ((Rcvr8_10,   f),100), ((Rcvr8_10,   w),100)
>      , ((Rcvr12_18,  o),100), ((Rcvr12_18,  f),100), ((Rcvr12_18,  w),100)
>      , ((Rcvr18_26,  o),100), ((Rcvr18_26,  f),100), ((Rcvr18_26,  w),100)
>      , ((Rcvr26_40,  o), 75), ((Rcvr26_40,  f), 75), ((Rcvr26_40,  w), 75)
>      , ((Rcvr40_52,  o),100), ((Rcvr40_52,  f),100), ((Rcvr40_52,  w),100)
>      , ((Rcvr_PAR,   o),  0), ((Rcvr_PAR,   f),  0), ((Rcvr_PAR,   w),  0)
>      , ((Holography, o),100), ((Holography, f),100), ((Holography, w),100)
>      , ((RcvrArray18_26,o),100), ((RcvrArray18_26,f),100), ((RcvrArray18_26,w),100)
>     ]

Generate frequency by Band.
Assume we are observing the water line 40% of the time.

> genFreq   :: Band -> Gen Float
> genFreq K = T.frequency [(40, return 22.2), (60, choose (18.0, 26.0))]
> genFreq P = choose ( 0.35, 1.0) 
> genFreq L = choose ( 1.0,  2.0)
> genFreq S = choose ( 2.0,  3.95)
> genFreq C = choose ( 3.95, 5.85)
> genFreq X = choose ( 8.0, 10.0)
> genFreq U = choose (12.0, 15.4)
> genFreq A = choose (26.0, 40.0)
> genFreq Q = choose (40.0, 50.0)
> genFreq W = choose (80.0, 100.0)

Generate frequency by Receiver.

> genFreq' :: Receiver -> Gen Float
> genFreq' Rcvr18_26 = T.frequency [(40, return 22.2), (60, choose (18.0, 26.0))]
> genFreq' RcvrArray18_26 = choose (18.0 , 26.0) 
> genFreq' Rcvr_RRI  = choose (0.1 , 1.6) 
> genFreq' Rcvr_342  = choose (0.29 ,0.395) 
> genFreq' Rcvr_450  = choose (0.385, 0.52) 
> genFreq' Rcvr_600  = choose (0.51 , 0.69) 
> genFreq' Rcvr_800  = choose (0.68 , 0.92) 
> genFreq' Rcvr_1070 = choose (0.91, 1.23) 
> genFreq' Rcvr1_2   = choose ( 1.0,  2.0)
> genFreq' Rcvr2_3   = choose ( 2.0,  3.95)
> genFreq' Rcvr4_6   = choose ( 3.95, 5.85)
> genFreq' Rcvr8_10  = choose ( 8.0, 10.0)
> genFreq' Rcvr12_18 = choose (12.0, 15.4)
> genFreq' Rcvr26_40 = choose (26.0, 40.0)
> genFreq' Rcvr40_52 = choose (40.0, 50.0)
> genFreq' Rcvr_PAR  = choose (90.0, 90.0)
> genFreq' Holography = choose (11.7, 12.2)



> generate' f = do
>     g <- getStdGen
>     return $ generate 0 g f

> generateVec :: Arbitrary a => Int -> IO [a]
> generateVec = generate' . vector

> generateTestData :: Int -> ([Session], [Period])
> generateTestData n = (sessions, periods)
>   where
>     g = mkStdGen 1
>     sessions = generate 0 g $ genSessions n
>     periods  = generate 0 g $ genPeriods n

Consistenly reproduces the *same* randomly generated set of Projects

> generateTestProjects :: Int -> [Project]
> generateTestProjects n = generate 0 g $ genProjects n
>   where
>     g = mkStdGen 1

This essentially returns the same data as generateTestProjects, but as a list
of just the Sessions, w/ unique id's and names.

> generateTestSessions :: Int -> [Session]
> generateTestSessions numProjs = sess
>   where
>     g = mkStdGen 1
>     ps = generate 0 g $ genProjects numProjs
>     sess = zipWith (\s n -> s { sId = n, sName = show n }) (concatMap sessions ps) [0..]

Anytime in 2006 (to the hour).  Oh yeah, avoiding the last 3 days of each month

> gen2006Date :: Gen DateTime
> gen2006Date = do
>     month <- choose (1, 12)
>     day <- choose (1, 28) 
>     hr <- choose (0, 23)
>     return $ fromGregorian 2006 month day hr 0 0

Sometime in Oct. 2006

> genStartDate :: Gen DateTime
> genStartDate = do
>     day   <- choose (1, 30) 
>     hr    <- choose (0, 23)
>     return $ fromGregorian 2006 10 day hr 0 0

> genScheduleDuration :: Gen Minutes
> genScheduleDuration = do
>   dur <- choose (8*60, 24*60)
>   return $ round2quarter dur

Tsys values are looked up from a database using integer values (really 
rounded off floats) for frequency and elevation.

In GHz

> genLookupFrequency :: Gen Float
> genLookupFrequency = choose (2.0, 50.0)

In Radians

> genLookupElevation :: Gen Float
> genLookupElevation = choose (deg2rad 5, deg2rad 90)
