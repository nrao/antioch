> module Antioch.ReportsTests where

> --import Antioch.Reports
> import Antioch.RunSimulation
> import Antioch.Reports
> import Antioch.Schedule
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score
> import Antioch.PProjects
> import Antioch.GenerateSchedule
> import Data.List
> import Data.Maybe

Main value of these unit tests is to check for compilation and run time errors:
If it doesn't blow up, it passes

> tests = TestList [ test_runSim
>                  , test_textReports
>                  , test_reportPeriodWindow
>                  --, test_reportWindowTimes
>                  ]

> test_runSim = TestCase $ do
>   -- TBF: remove old plots
>   let start = fromGregorian 2006 2 2 0 0 0
>   --runSim start 3 "."
>   runSimulation Pack "." (statsPlotsToFile "." "") start 3 "" True True
>   -- TBF: make sure new plots and text report are there
>   assertEqual "test_runSim" True True

> test_textReports = TestCase $ do
>     textReports name outdir now execTime dt days strategyName ss schedule canceled winfo gaps scores scoreDetails simInput rs history quiet 
>     textReports name outdir now execTime dt days strategyName ss2 schedule2 canceled winfo gaps scores scoreDetails simInput rs history quiet 
>   where
>     name = "unit_test"
>     outdir = "."
>     now = fromGregorian 2008 1 1 0 0 0
>     execTime = 100
>     dt = fromGregorian 2006 9 1 0 0 0
>     days = 70 
>     strategyName = "Pack"
>     ss = []
>     schedule = []
>     canceled = []
>     gaps = []
>     scores = [("obsEff", [])
>             , ("atmEff", [])
>             , ("trkEff", [])
>             , ("srfEff", [])]
>     scoreDetails = [("obsEff", [])
>             , ("atmEff", [])
>             , ("trkEff", [])
>             , ("srfEff", [])]
>     simInput = True
>     rs = []
>     winfo = []
>     history = []
>     quiet = True
>     -- for the second report, use the test sessions
>     ss2 = getOpenPSessions
>     --s = head ss2'
>     --p = defaultPeriod { session = s
>     --                  , startTime = fromGregorian 2006 2 3 0 0 0
>     --                  , duration = 60
>     --                  }
>     --s' = makeSession s [] [p]
>     --ss2 = tail ss2' ++ [s']
>     schedule2 = concatMap periods ss2

> test_reportPeriodWindow = TestCase $ do
>     assertEqual "test_reportPeriodWindow_1" True (validSimulatedWindows s)
>     assertEqual "test_reportPeriodWindow_2" exp (reportPeriodWindow p)
>   where
>     {-
>     winStart = fromGregorian 2006 2 1 0 0 0
>     winDur   = 10*24*60
>     pStart   = fromGregorian 2006 2 8 5 30 0
>     s' = defaultSession { sType = Windowed }
>     p' = defaultPeriod { startTime = pStart
>                        , duration = 60*2 
>                        , session = s' }
>     w' = defaultWindow { wSession = s' 
>                        , wStart = winStart
>                        , wDuration = winDur }
>     s = makeSession s' [w'] [p']
>     -}
>     s = getTestWindowSession
>     p = head . periods $ s
>     exp = "2006-02-01 00:00:00                                   [2006-02-08 05:30:00 for 120 mins.] 2006-02-11 00:00:00\n"

> test_reportWindowedTimes = TestCase $ do
>     assertEqual "test_reportWindowedTimes_1" True (validSimulatedWindows s)
>     assertEqual "test_reportWindowedTimes_2" exp (reportWindowedTimes wInfo)
>     assertEqual "test_reportWindowedTimes_3" exp2 (reportWindowedTimes wInfo2)
>     assertEqual "test_reportWindowedTimes_4" exp3 (reportWindowedTimes (wInfo ++wInfo2))
>   where
>     s = getTestWindowSession
>     wInfo = [(head . windows $ s, Nothing, head . periods $ s)]
>     s2' = getTestWindowSession2
>     cp = defaultPeriod { session = s2
>                        , startTime = fromGregorian 2006 3 2 12 0 0
>                        , duration = 60*2 }
>     dp = head . periods $ s2'
>     s2 = makeSession s2' (windows s2') [cp]
>     wInfo2 = [(head . windows $ s2, Just cp, dp)]
>     exp = "Window Times:\n              Periods   Hours    \n    default   1         2.00     \n    chosen    0         0.00     \n    total     1         2.00     \n"
>     exp2 = "Window Times:\n              Periods   Hours    \n    default   0         0.00     \n    chosen    1         2.00     \n    total     1         2.00     \n"
>     exp3 = "Window Times:\n              Periods   Hours    \n    default   1         2.00     \n    chosen    1         2.00     \n    total     2         4.00     \n"

Utilities:

> getTestWindowSession :: Session
> getTestWindowSession = makeSession s' [w'] [p']
>   where
>     winStart = fromGregorian 2006 2 1 0 0 0
>     winDur   = 10*24*60
>     pStart   = fromGregorian 2006 2 8 5 30 0
>     s' = defaultSession { sType = Windowed }
>     p' = defaultPeriod { startTime = pStart
>                        , duration = 60*2 
>                        , session = s' }
>     w' = defaultWindow { wSession = s' 
>                        , wStart = winStart
>                        , wDuration = winDur }

> getTestWindowSession2 :: Session
> getTestWindowSession2 = makeSession s' [w'] [p']
>   where
>     winStart = fromGregorian 2006 3 1 0 0 0
>     winDur   = 10*24*60
>     pStart   = fromGregorian 2006 3 8 5 30 0
>     s' = defaultSession { sType = Windowed }
>     p' = defaultPeriod { startTime = pStart
>                        , duration = 60*2 
>                        , session = s' }
>     w' = defaultWindow { wSession = s' 
>                        , wStart = winStart
>                        , wDuration = winDur }
