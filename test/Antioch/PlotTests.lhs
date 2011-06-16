> module Antioch.PlotTests where

> import Antioch.Plots
> import Test.HUnit

> tests = TestList [test_adjustHistData
>                  ]

> test_adjustHistData = TestCase $ do
>     assertEqual "test_adjustHistData 1" xy1  (adjustHistData xy1) 
>     assertEqual "test_adjustHistData 2" xy2' (adjustHistData xy2) 
>     assertEqual "test_adjustHistData 3" xy3' (adjustHistData xy3) 
>     assertEqual "test_adjustHistData 4" xy4' (adjustHistData xy4) 
>   where
>     xy1  = [(0.0,1.0)]
>     xy2  = [(0.0,1.0),  (1.0,3.0)]
>     xy2' = [(-0.5,1.0), (0.5,3.0)]
>     xy3  = [(0.0,1.0),  (1.0,3.0), (2.0, 2.0)]
>     xy3' = [(-0.5,1.0), (0.5,3.0), (1.5, 2.0)]
>     xy4  = [(0.0,1.0),  (1.0,3.0), (2.0, 2.0), (3.0, 0.0)]
>     xy4' = [(-0.5,1.0), (0.5,3.0), (1.5, 2.0), (2.5, 0.0)]
