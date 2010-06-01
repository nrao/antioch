> module Antioch.ReceiverTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.DSSData
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Maybe
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)

> tests = TestList [
>     test_getReceiverTemperature
>   , test_getPrimaryReceiver
>   , test_rcvrInFreqRange
>                  ]

Rcvr1_2 : 
(1.692,37.955)
(1.696,9.575)
(1.7,8.8525)
(1.704,8.6775)
(1.708,8.502501)

> test_getReceiverTemperature = TestCase $ do
>   temp <- getReceiverTemperature s1
>   assertEqual "test_getReceiverTemperatures_1" temp1 temp 
>     where
>       freq1 = 1.7
>       temp1 = 8.8525
>       s1 = defaultSession { receivers = [[Rcvr1_2]], frequency = freq1 }

> test_rcvrInFreqRange = TestCase $ do
>   assertEqual "test_rcvrInFreqRange_1" True (rcvrInFreqRange 1.6 Rcvr1_2) 
>   assertEqual "test_rcvrInFreqRange_2" False (rcvrInFreqRange 1.12 Rcvr1_2)

> test_getPrimaryReceiver = TestCase $ do
>   let r = getPrimaryReceiver s1
>   assertEqual "test_getPrimaryReciever_1" Rcvr1_2 r 
>   -- TBF, WTF: what to do in this case?
>   --let r = getPrimaryReceiver s2
>   --assertEqual "test_getPrimaryReciever_2" Rcvr1_2 r 
>     where
>       freq1 = 1.7
>       temp1 = 8.8525
>       s1 = defaultSession { receivers = [[Rcvr1_2]], frequency = freq1 }
>       -- TBF: the following freq is valid in the receiver temps, but is out of
>       -- the official range of the receiver according to the DSS ranges
>       freq2 = 1.12 
>       temp2 = 13.775
>       s2 = defaultSession { receivers = [[Rcvr1_2]], frequency = freq2 }

> test_data = TestCase $ do
>   projs <- getProjects
>   print . length $ projs
>   let ss = concatMap sessions projs
>   let rcvrCheck = map checkRcvrAndFreq ss
>   print "len rcvrCheck: "
>   print . length $ rcvrCheck
>   print "Number of sessions that do not have a single rcvrInFreqRange: "
>   print . length $ filter (==False) rcvrCheck
>     where
>       checkRcvrAndFreq s = any (rcvrInFreqRange (frequency s)) (rcvrs s)
>       rcvrs s = concat . receivers $ s 
