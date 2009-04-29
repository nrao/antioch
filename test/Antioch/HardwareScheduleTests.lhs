> module Antioch.HardwareScheduleTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.HardwareSchedule
> import Maybe
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)

> tests = TestList [
>     test_getReceiverSchedule
>   , test_getReceiverSchedule2
>      ]

> test_getReceiverSchedule = TestCase $ do
>     schd <- getReceiverSchedule $ Just dt
>     assertEqual "test_getReceiverSchedule" exp schd  
>       where
>         dt  = fromGregorian 2006 6 1 4 0 0
>         dt1 = fromGregorian 2006 6 1 4 0 0
>         dt2 = fromGregorian 2006 6 7 4 0 0
>         exp = [(dt1,[Rcvr1_2,Rcvr2_3]),(dt2,[Rcvr1_2,Rcvr4_6])]

> test_getReceiverSchedule2 = TestCase $ do
>     schd <- getReceiverSchedule $ Just dt
>     assertEqual "test_getReceiverSchedule2_1" exp schd  
>     schd <- getReceiverSchedule $ Just dt3
>     assertEqual "test_getReceiverSchedule2_2" ([exp!!1]) schd  
>     schd <- getReceiverSchedule Nothing 
>     assertEqual "test_getReceiverSchedule2_2" exp schd  
>       where
>         dt  = fromGregorian 2006 6 2 4 0 0
>         dt1 = fromGregorian 2006 6 1 4 0 0
>         dt2 = fromGregorian 2006 6 7 4 0 0
>         dt3 = fromGregorian 2006 6 8 4 0 0
>         exp = [(dt1,[Rcvr1_2,Rcvr2_3]),(dt2,[Rcvr1_2,Rcvr4_6])]


