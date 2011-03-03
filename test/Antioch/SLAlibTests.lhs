> module Antioch.SLAlibTests where

> import Antioch.DateTime
> import Antioch.SLALib
> import Antioch.Utilities
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score

> tests = TestList [test_gmst
>                 , test_slaDrange]

BETA: matches 3rd party python library slalib.sla_gmst
import slalib
slalib.sla_gmst(54907.0)

> test_gmst = TestCase $ do
>   let mjd = 54907.0
>   let result = gmst mjd
>   assertEqual "test_utc2lst" 3.0490882973440634 result

> test_slaDrange = TestCase $ do
>     -- these tests nicely illustrange what slaDrange does
>     -- start increasing them from zero
>     assertEqual "test_slaDrange_1" 0.0  (slaDrange 0.0) 
>     assertEqual "test_slaDrange_2" 3.13 (slaDrange 3.13) 
>     assertAlmostEqual "test_slaDrange_3" 5 (-3.1331853071795863) (slaDrange 3.15) 
>     assertAlmostEqual "test_slaDrange_4" 5 (-5.3185307179585806e-2) (slaDrange 6.23) 
>     assertEqual "test_slaDrange_5" (0.0) (slaDrange d2pi) 
>     assertAlmostEqual "test_slaDrange_6" 2 3.13 (slaDrange (3.13+d2pi)) 
>     assertAlmostEqual "test_slaDrange_7" 5 (-3.1331853071795863) (slaDrange (3.15+(10*d2pi))) 
>     -- start decreasing them from zero
>     assertEqual "test_slaDrange_8" 0.0  (slaDrange 0.0) 
>     assertEqual "test_slaDrange_9" (-3.13) (slaDrange (-3.13)) 
>     assertAlmostEqual "test_slaDrange_10" 5 3.1331853071795863 (slaDrange (-3.15)) 
>     assertAlmostEqual "test_slaDrange_11" 5 5.3185307179585806e-2 (slaDrange (-6.23)) 
>     assertEqual "test_slaDrange_12" (0.0) (slaDrange d2pi) 
>     assertAlmostEqual "test_slaDrange_13" 2 (-3.13) (slaDrange (-(3.13+d2pi))) 
>     assertAlmostEqual "test_slaDrange_14" 5 3.1331853071795863 (slaDrange (-(3.15+(10*d2pi)))) 

Test utilities

> assertAlmostEqual :: String -> Int -> Double -> Double -> IO ()
> assertAlmostEqual name places expected value =
>     assertBool name $ abs (value - expected) < epsilon
>   where
>     epsilon = 1.0 / 10.0 ** fromIntegral places   
>     
