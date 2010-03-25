> module Antioch.SimulateObservingTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.PProjects
> import Antioch.SimulateObserving
> import Data.List (sort, find)
> import Data.Maybe
> import Test.HUnit
> import System.Random

> tests = TestList [
>      test_findCanceledPeriods
>                  ]


> test_findCanceledPeriods = TestCase $ do
>   assertEqual "SimulationTests_test_findCanceledPeriods1" [] $ findCanceledPeriods [] []
>   assertEqual "SimulationTests_test_findCanceledPeriods2" [] $ findCanceledPeriods [p1] [p1]
>   assertEqual "SimulationTests_test_findCanceledPeriods3" [] $ findCanceledPeriods [p1,p2] [p1,p2]
>   assertEqual "SimulationTests_test_findCanceledPeriods4" [p2] $ findCanceledPeriods [p1,p2] [p1]
>   assertEqual "SimulationTests_test_findCanceledPeriods5" [p1] $ findCanceledPeriods [p1,p2] [p2]
>   assertEqual "SimulationTests_test_findCanceledPeriods6" [p2] $ findCanceledPeriods [p1,p2] [p1,p3]
>     where
>   dt1 = fromGregorian 2006 2 1 0 0 0
>   dt2 = fromGregorian 2006 2 1 1 0 0
>   dt3 = fromGregorian 2006 2 1 2 0 0
>   p1 = Period 0 defaultSession dt1 1 0.0 Pending dt1 False 1
>   p2 = Period 0 defaultSession dt2 1 0.0 Pending dt2 False 1
>   p3 = Period 0 defaultSession dt3 1 0.0 Pending dt3 False 1


