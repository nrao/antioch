> module Antioch.ReportsTests where

> import Antioch.Reports
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score

Main value of these unit tests is to check for compilation and run time errors:
If it doesn't blow up, it passes

> tests = TestList [test_runSim]

> test_runSim = TestCase $ do
>   -- TBF: remove old plots
>   runSim 3 "."
>   -- TBF: make sure new plots and text report are there
>   assertEqual "test_runSim" True True
