> module Antioch.Generators where

> import Antioch.Types
> import Antioch.SLALib (slaGaleq)
> import Antioch.Utilities
> import Antioch.DateTime
> import Data.Char
> import System.Random  (getStdGen)
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
> genSemesterName = elements ["05C", "06A", "06B", "06C"]

> genThesis :: Gen Bool
> genThesis = T.frequency [(20, return True), (80, return False)]

TBF: how to link to Sessions that have already been generated?  and then use
those to calculate timeLeft and timeTotal?

> --assignProject p ss = [ s {project = p} | s <- sessions p]

> genProject :: Gen Project
> genProject = do
>     name     <- genProjectName
>     semester <- genSemesterName
>     thesis   <- genThesis
>     sessions <- genProjectSessions
>     let timeTotal = sum [ totalTime s | s <- sessions ]
>     let timeUsed  = sum [ totalUsed s | s <- sessions ]
>     let project = defaultProject {
>           pName = str name
>         , semester = semester
>         , thesis = thesis
>         , timeTotal = timeTotal
>         , timeLeft = timeTotal - timeUsed
>         }
>     return $ makeProject project sessions

> genProjects         :: Int -> Gen [Project]
> genProjects 0       = do {return $ []}
> genProjects (n + 1) = do
>     p  <- genProject
>     pp <- genProjects n
>     return $ [p] ++ pp

> genScheduleProjects :: Gen [Project]
> genScheduleProjects = do
>     n <- choose (10, 30)
>     ps <- genProjects n
>     return $ ps 

Now lets make sure we are properly generating Projects: test each attribute
at a time:

> prop_pName p = "A" <= pName p && pName p <= "Z"
> prop_semester p = any (==(semester p)) ["07C", "08A", "08B", "08C"]
> prop_thesis p = thesis p == True || thesis p == False

Each Project's Sessions can have a totalTime between 2 & 30 hrs.  Currently
a project has between 1 and 5 Sessions.

> prop_sessions p = 1 <= (length . sessions $ p) && (length . sessions $ p) <= 5
> prop_timeTotal p = (1*2*60) <= timeTotal p && timeTotal p <= (5*30*60)

> prop_timeTotalQuarter p = timeTotal p `mod` quarter == 0

Each Session can have 0-3 Periods, each with a max of 10 hours:

> prop_projectPeriods p = let n = sum [ (length . periods $ s) | s <- sessions p] in 0 <= n && n <= 5*3

TBF: this does not pass because generated periods aren't limited by their
sessions' totalTime.

> prop_timeLeft p = 0 <= timeLeft p && timeLeft p <= timeTotal p

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
>         return (deg2rad 18.0, deg2rad dec)
>     galactic = do
>         longitude <- choose (0.0, 250.0)
>         return $ slaGaleq (deg2rad longitude) 0.0
> genRaDec _   = do
>     ra  <- choose (0.0, 2*pi)
>     dec <- fmap asin . choose $ (sin . deg2rad $ -35.0, sin . deg2rad $ 90.0)
>     return (ra, dec)

> round2quarter :: Minutes -> Minutes
> round2quarter m = m - (m `mod` quarter)

TBF: how to link these to generated Projects? 

> genSession :: Gen Session
> genSession = do
>     project    <- genProject
>     t          <- genSemester
>     b          <- genBand t
>     let r      = band2Receiver b
>     g          <- genGrade [GradeA, GradeA, GradeB, GradeC, GradeC]
>     f          <- genFreq b
>     s          <- skyType
>     (ra, dec)  <- genRaDec s
>     totalTime  <- choose (2*60, 30*60)
>     minD       <- choose (2*60, 4*60)
>     maxD       <- choose (6*60, 8*60)
>     return $ defaultSession {
>                  project        = project
>                , periods        = []
>                , band           = b
>                , frequency      = f
>                , ra             = ra
>                , dec            = dec
>                , minDuration    = round2quarter minD
>                , maxDuration    = round2quarter maxD
>                , totalTime      = round2quarter totalTime
>                , totalUsed      = 0
>                , grade          = g
>                , receivers      = [r]
>                }


> genProjectSessions :: Gen [Session]
> genProjectSessions = 
>     T.frequency [(25, return 1), (25, return 2), (20, return 3), (20, return 4), (10, return 5)] >>= vector


> genSessions         :: Int -> Gen [Session]
> genSessions 0       = do {return $ []}
> genSessions (n + 1) = do
>     s  <- genSession
>     ss <- genSessions n
>     return $ [s] ++ ss

> prop_Grade s = grade s `elem` [GradeA, GradeB, GradeC]
> prop_Receiver s = head (receivers s) == band2Receiver (band s)

> prop_Ra s = 0.0 <= ra s && ra s <= 2 * pi

Make sure that the total time used up by the periods is correct:

> prop_totalUsed s          = 0 <= totalUsed s && totalUsed s <= (3*10*60)
> prop_totalTime s          = (2*60) <= totalTime s && totalTime s <= (30*60)
> prop_totalTimeQuarter s   = totalTime s `mod` quarter == 0
> prop_minDuration s        = (2*60) <= minDuration s && minDuration s <= (4*60)
> prop_minDurationQuarter s = minDuration s `mod` quarter == 0
> prop_maxDuration s        = (6*60) <= maxDuration s && maxDuration s <= (8*60)
> prop_maxDurationQuarter s = maxDuration s `mod` quarter == 0

> prop_Dec s = (-pi) / 2 <= dec s && dec s <= pi / 2

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
>      let period = Period {
>          session   = session
>        , startTime = startTime
>        , duration  = duration
>        , pScore    = 0.0
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
>     return $ [p] ++ ps

Make sure Durations are made of 15-minute intervals

> prop_duration p = duration p `mod` quarter == 0

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

> genGrade    :: [Grade] -> Gen Grade
> genGrade gs = elements gs

> band2Receiver :: Band -> Receiver
> band2Receiver L = Rcvr1_2
> band2Receiver S = Rcvr2_3
> band2Receiver C = Rcvr4_6
> band2Receiver X = Rcvr8_10
> band2Receiver U = Rcvr12_18
> band2Receiver K = Rcvr18_22 -- Need Rcvr22_26
> band2Receiver A = Rcvr26_40
> band2Receiver Q = Rcvr40_52




> genBand     :: Int -> Gen Band
> genBand sem = fmap (read . str) . elements $ bands !! sem
>   where
>     bands = [ "KKQQAAXUCCSLLLLLLLLL"  -- 0 => backup
>             , "KKKQQQAXUCCSSLLLLLLL"  -- 1
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

Sometime in Oct. 2006

> genStartDate :: Gen DateTime
> genStartDate = do
>     day <- choose (1, 30) 
>     hr <- choose (0, 23)
>     return $ fromGregorian 2006 10 day hr 0 0

> genScheduleDuration :: Gen Minutes
> genScheduleDuration = choose (8, 24)
