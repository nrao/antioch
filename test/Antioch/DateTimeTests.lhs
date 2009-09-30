> module Antioch.DateTimeTests where

> import Antioch.DateTime
> import Antioch.SLALib
> import Antioch.Utilities
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score

> tests = TestList [test_secondsToMJD
>                 , test_addMonth
>                  ]

BETA: results compared to using 3rd party libraries used in beta's TimeAgent

> test_secondsToMJD = TestCase $ do
>   let dt = fromGregorian 2006 10 15 11 0 0 
>   let mjd = secondsToMJD dt
>   -- BETA: difference due to Float vs. Double
>   -- printing mjd reveals it to really be  54023.457
>   assertEqual "test_secondsToMJD" 54023.4583333 mjd

> test_addMonth = TestCase $ do
>   let dt1 = fromGregorian 2006 11 15 11 0 0 
>   let dt2 = fromGregorian 2006 12 15 11 0 0 
>   let dt3 = fromGregorian 2007  1 15 11 0 0 
>   assertEqual "test_addMonth_1" dt2 (addMonth dt1)
>   assertEqual "test_addMonth_2" dt3 (addMonth dt2)

