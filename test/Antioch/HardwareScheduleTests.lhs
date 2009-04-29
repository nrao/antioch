> module Antioch.HardwareScheduleTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.HardwareSchedule
> import Maybe
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)

> tests = TestList [
>     test_getReceiverSchedule
>      ]

> test_getReceiverSchedule = TestCase $ do
>     schd <- getReceiverSchedule dt
>     assertEqual "test_getReceiverSchedule" exp schd  
>       where
>         dt  = fromGregorian 2006 6 1 4 0 0
>         dt1 = fromGregorian 2006 6 1 4 0 0
>         dt2 = fromGregorian 2006 6 7 4 0 0
>         exp = [(dt1,[Rcvr1_2,Rcvr2_3]),(dt2,[Rcvr1_2,Rcvr4_6])]


