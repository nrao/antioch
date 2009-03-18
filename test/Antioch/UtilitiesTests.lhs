> module Antioch.UtilitiesTests where

> import Antioch.DateTime
> import Antioch.Utilities
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score

> tests = TestList [test_utc2lst]

BETA: compare against TimeAgent.Absolute2RelativeLST
from antioch.util import TimeAgent
from datetime import datetime
dt = datetime(2006, 10, 15, 11)
TimeAgent.hr2rad(TimeAgent.Absolute2RelativeLST(dt))

> test_utc2lst = TestCase $ do
>   -- BETA: beta gives 1.90240073092 due to Float vs. Double
>   assertEqual "test_utc2lst" 1.8941972  lst1
>   assertEqual "test_utc2lst_2" expLsts lsts
>     where
>   dt = fromGregorian 2006 10 15 11 0 0
>   lst1 = hrs2rad $ utc2lstHours dt
>   start = fromGregorian 2006 10 16 0 0 0
>   dts = [(i*60) `addMinutes'` start | i <- [0..23]]
>   lsts = map (hrs2rad . utc2lstHours) dts
>   expLsts = [5.3151116,5.5612197,5.8319397,6.102659,9.019408e-2,0.36091372,0.6070226,0.87774235,1.1238515,1.3945711,1.64068,1.9113998,2.1821194,2.4528391,2.7235596,2.9696681,3.240388,3.4864967,3.7572165,4.003326,4.298656,4.544765,4.815485,5.0862055]

