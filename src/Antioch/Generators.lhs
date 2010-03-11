> module Antioch.Generators where

> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.SLALib  (slaGaleq)
> import Antioch.Utilities
> import Antioch.DateTime
> import Data.Char
> import Data.List 
> import Data.Maybe      (isJust, maybeToList)
> import System.Random   (getStdGen, setStdGen, mkStdGen)
> import Test.QuickCheck hiding (frequency)
> import qualified Test.QuickCheck as T

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

TBF: Currently, the idea of semester is very limited.

> genSemesterName :: Gen String
> genSemesterName = elements ["05C"
>                           , "06A", "06A", "06A", "06A"
>                           , "06B", "06B", "06B", "06B"
>                           , "06C", "06C", "06C"
>                            ]

trimesterMonth = [3,1,1,1,1,2,2,2,2,3,3,3] 

> genThesis :: Gen Bool
> genThesis = T.frequency [(20, return True), (80, return False)]

> genMaxSemesterTime :: Minutes -> Gen Minutes
> genMaxSemesterTime time = T.frequency [(20, return $ div time 2), (80, return time)]


> genProject :: Gen Project
> genProject = do
>     name     <- genProjectName
>     semester <- genSemesterName
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
>     return $ makeProject project pAllottedT sessions

> genProjects         :: Int -> Gen [Project]
> genProjects 0       = return []
> genProjects (n + 1) = do
>     p  <- genProject
>     pp <- genProjects n
>     return $ p : pp

> genScheduleProjects :: Gen [Project]
> genScheduleProjects =
>     -- TBF: generate unique ids for projects & sessions
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

TBF: this does not pass because generated periods aren't limited by their
sessions' sAllottedT.

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
>   if freq > 18.0 then choose (2*60, 2*60)
>            else choose (2*60, 6*60)

> genMaxTP freq = 
>   if freq > 18.0 then choose (12*60, 12*60)
>            else choose (11*60, 12*60)

> genTimeBetween :: Gen Minutes
> genTimeBetween = T.frequency [(50, return 0)
>                             , (25, return (8 *60))
>                             , (25, return (12*60))]

> genLowRFIFlag :: Gen Bool
> genLowRFIFlag = T.frequency [(85, return False), (15, return True)]

> genTransitFlag :: Gen TransitType
> genTransitFlag = T.frequency [(70, return Optional)
>                             , (15, return Partial)
>                             , (15, return Center)]

> genLSTExclusion :: Gen [(Float, Float)]
> genLSTExclusion = T.frequency [(80, return []), (20, lsts)]
>   where
>     lsts = do 
>       low  <- choose (0.0, 5.0)
>       high <- choose (6.0, 12.0)
>       return $ [(low, high)]

> genSession :: Gen Session
> genSession = do
>     project    <- genProject
>     t          <- genSemester
>     b          <- genBand t
>     let r      = band2Receiver b
>     g          <- genGrade [4.0, 4.0, 3.0, 3.0, 3.0]
>     f          <- genFreq b
>     bk         <- genBackupFlag f
>     s          <- skyType
>     (ra, dec)  <- genRaDec s
>     sAllottedT <- choose (6*60, 30*60)
>     minD       <- genMinTP f
>     maxD       <- genMaxTP f
>     --minD       <- choose (2*60, 6*60)
>     --maxD       <- choose (11*60, 12*60)
>     tb         <- genTimeBetween
>     lstEx      <- genLSTExclusion
>     lowRFIFlag <- genLowRFIFlag
>     trans      <- genTransitFlag
>     return $ defaultSession {
>                  project        = project
>                , periods        = []
>                , band           = b
>                , frequency      = f
>                , ra             = ra
>                , dec            = dec
>                , minDuration    = round2quarter minD
>                , maxDuration    = round2quarter maxD
>                -- TBF: only for scheduleMinDuration; then go back
>                --, sAllottedT     = matchAvTime sAllottedT(round2quarter minD)
>                , sAllottedT      = round2quarter sAllottedT
>                , timeBetween    = round2quarter tb
>                , lstExclude     = lstEx
>                , lowRFI         = lowRFIFlag
>                , transit        = trans
>                , grade          = g
>                , receivers      = [[r]]
>                , backup         = bk
>                }

TBF: this is only for use with the scheduleMinDuration strategy.  We want
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

Assumes a single scalar rcvr group

> prop_Receiver s = (head . head . receivers $ s) == band2Receiver (band s)

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

TBF: originally, Dana had us set the lower limit to -40.0, but Carl's data
has some decs at -44.0.

> validDec :: Session -> Bool
> validDec s = -45.0 <= dec' && dec' <= 90.0
>   where
>     dec' = rad2deg . dec $ s

TBF: thing is, this is in degrees, and it doesn't pass either!

> prop_DecDegree s = (-180) <= dec s && dec s <= 180 

TBF: start on 15 min. boundries in a given time range. But how to make them
mutually exclusive?  Need for varied and interesting start times!!!! :|

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

We need to (partially) fill a chunk of time with valid periods.  
It would be good to
parameterize this such that we can control the degree to which the schedule
is filled.
TBF: we can make this more sophisticated.

> genSchedulePeriods :: DateTime -> Minutes -> [Session] -> Gen [Maybe Period]
> genSchedulePeriods starttime schedDur sessions = do
>     -- duration probably between 8 - 24 hours.
>     -- how many periods to stick in there?
>     n <- choose (1, 3)
>     -- try to stick each period in the schedule, unless there's no room;
>     -- make sure that max & min session durations are obeyed, and that
>     -- we don't create1 any dead space from holes < smalles min. session
>     let schedDurs = round2quarter $ schedDur `div` n 
>     let dts = [ (schedDurs * i) `addMinutes'` starttime | i <- [0 .. (n-1)]]
>     -- TBF: randomly select which sessions to generate periods for
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

TBF: this code needs to go somewhere where it can be shared better - duplicate!
TBF: the sudoku.lhs from the python beta test has lots of tested code for 
finding conflicts.  we should probably tap into that eventually.  For now
we'll reproduce some of the basic conflict detection stuff:

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

> conflicts :: [Period] -> [Period] -> Bool
> conflicts [] ys = False
> conflicts (x:xs) ys | overlaps x (delete x ys) = True
>                     | otherwise = conflicts xs ys

> obeyMinSeparation :: [Period] -> Minutes -> Bool
> obeyMinSeparation ps minSep = dropWhile (>=minSep) (minutesBetween ps) == []

> minutesBetween :: [Period] -> [Minutes]
> minutesBetween []     = []
> minutesBetween (p:[]) = []
> minutesBetween (p:ps) = diffMinutes' (startTime (head ps)) (endTime p) : minutesBetween ps


> type Semester = Int
  
> genSemester :: Gen Semester
> genSemester = fmap (read . str) . elements $ "0111122223333"

> prop_Semester = forAll genSemester $ \s -> s `elem` [0..3]

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

> band2Receiver :: Band -> Receiver
> band2Receiver L = Rcvr1_2
> band2Receiver S = Rcvr2_3
> band2Receiver C = Rcvr4_6
> band2Receiver X = Rcvr8_10
> band2Receiver U = Rcvr12_18
> band2Receiver K = Rcvr18_26 -- Rcvr18_22 -- Need Rcvr22_26
> band2Receiver A = Rcvr26_40
> band2Receiver Q = Rcvr40_52




> genBand     :: Int -> Gen Band
> genBand sem = fmap (read . str) . elements $ bands !! sem
>   where
>     bands = [ "KKQQAAXUCCSLLLLLLLLL"  -- 0 => backup
>             , "KKKQQAAXUCCSSLLLLLLL"  -- 1
>             , "KQQAXUCSLLLLLLLLLLLL"  -- 2
>             , "KKQQAAAXXUCCSLLLLLLL"  -- 3
>             ]

Assume we are observing the water line 40% of the time.

> genFreq   :: Band -> Gen Float
> genFreq K = T.frequency [(40, return 22.2), (60, choose (18.0, 26.0))]
> genFreq L = return 2.0
> genFreq S = choose ( 2.0,  3.95)
> genFreq C = choose ( 3.95, 5.85)
> genFreq X = choose ( 8.0, 10.0)
> genFreq U = choose (12.0, 15.4)
> genFreq A = choose (26.0, 40.0)
> genFreq Q = choose (40.0, 50.0)

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
