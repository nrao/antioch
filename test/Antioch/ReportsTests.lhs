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

Main value of these unit tests is to check for compilation and run time errors:
If it doesn't blow up, it passes

> tests = TestList [ test_runSim
>                  , test_textReports
>                  ]

> test_runSim = TestCase $ do
>   -- TBF: remove old plots
>   let start = fromGregorian 2006 2 2 0 0 0
>   --runSim start 3 "."
>   runSimulation Pack "." (statsPlotsToFile "." "") start 3 "" True True
>   -- TBF: make sure new plots and text report are there
>   assertEqual "test_runSim" True True

> test_textReports = TestCase $ do
>     textReports name outdir now execTime dt days strategyName ss schedule canceled gaps scores scoreDetails simInput rs history quiet 
>     textReports name outdir now execTime dt days strategyName ss2 schedule2 canceled gaps scores scoreDetails simInput rs history quiet 
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
