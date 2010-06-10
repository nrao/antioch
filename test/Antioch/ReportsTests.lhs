> module Antioch.ReportsTests where

> --import Antioch.Reports
> import Antioch.RunSimulation
> import Antioch.Reports
> import Antioch.Schedule
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score

Main value of these unit tests is to check for compilation and run time errors:
If it doesn't blow up, it passes

> tests = TestList [test_runSim]

> test_runSim = TestCase $ do
>   -- TBF: remove old plots
>   let start = fromGregorian 2006 2 2 0 0 0
>   --runSim start 3 "."
>   runSimulation Pack "." (statsPlotsToFile "." "") start 3 "" True True
>   -- TBF: make sure new plots and text report are there
>   assertEqual "test_runSim" True True
