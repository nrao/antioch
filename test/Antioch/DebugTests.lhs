> module Antioch.DebugTests where

> import Antioch.Debug
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score

> tests = TestList [test_getCanceledPeriods]

> test_getCanceledPeriods = TestCase $ do
>   assertEqual "test_getCanceledPeriods" ps canceled
>     where
>   start = fromGregorian 2006 2 1 0 0 0
>   dts = [(i*60) `addMinutes'` start | i <- [0..10]]
>   ps = map mkPeriod dts
>   mkPeriod dt = Period defaultSession dt 60 0.0 undefined False 60
>   trace' = map (Cancellation) ps
>   trace'' = map (Timestamp) dts
>   trace = trace' ++ trace''
>   canceled = getCanceledPeriods trace     

