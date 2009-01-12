> module Antioch.ScoreTests where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities
> import Test.HUnit

> tests = TestList [
>     test_zenithOpticalDepth
>   , test_receiverTemperature
>   , test_kineticTemperature
>   ]

> test_zenithOpticalDepth = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 0
>     let Just result = runScoring w (zenithOpticalDepth dtLP sessLP)
>     assertAlmostEqual "test_zenithOpticalDepth" 5 0.00799 result

> test_receiverTemperature = TestCase $ do
>     assertEqual "test_receiverTemperature" 5.0 $ receiverTemperature dtLP sessLP

> test_kineticTemperature = TestCase $ do
>     w <- getWeather . Just $ fromGregorian 2006 10 14 9 15 0
>     let Just result = runScoring w (kineticTemperature dtLP sessLP)
>     assertAlmostEqual "test_kineticTemperature" 1 257.49832 result

Test utilities

> assertAlmostEqual name places expected value =
>     assertBool name $ abs (value - expected) < epsilon
>   where
>     epsilon = 1.0 / 10.0 ** places

Test data generation

> sessLP = defaultSession {
>     sName     = "LP"
>   , ra        = hrs2rad 12.3
>   , dec       = deg2rad  5.4
>   , frequency = 5.4
>   , receivers = [Rcvr4_6]
>   }

> dtLP = fromGregorian 2006 10 15 12 0 0
