> module Antioch.ReceiverTemperaturesTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.ReceiverTemperatures
> import Maybe
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)

> tests = TestList [
>   test_receiverTemperatures
>                  ]

TBF: for now just make sure it doesn't blow up
Some Frequencies & Temperatures:
(1.116,12.8275)
(1.12,13.775)
(1.124,17.43)


> test_receiverTemperatures = TestCase $ do
>   rt <- getReceiverTemperatures
>   ts <- temperatures rt Rcvr1_2
>   assertEqual "test_receiverTemperatures_1" (1.1,22.125) (head ts) 
>   t <- temperature rt Rcvr1_2 1.12 ts
>   assertEqual "test_receiverTemperatures_2" 13.775 t 
>   t <- temperature rt Rcvr1_2 1.119 ts
>   assertEqual "test_receiverTemperatures_3" 12.8275 t 
>   t <- temperature rt Rcvr1_2 1.121 ts
>   assertEqual "test_receiverTemperatures_4" 13.775 t 
>   assertEqual "test_receiverTemperatures" True True
