> module Antioch.WeatherTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Maybe
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)

> tests = TestList [
>     test_WeatherIsJust
>   , test_WindsArePositive
>   , test_WindsAreReasonable
>      ]

> test_WindsArePositive = TestCase $ do
>   let sql = "SELECT wind_speed FROM forecasts WHERE wind_speed < 0.0"
>   cnn <- connect
>   result <- getFloat cnn sql []
>   assertEqual "test_WindsArePositive" True (isNothing result)

> test_WindsAreReasonable = TestCase $ do
>   let sql = "SELECT wind_speed FROM forecasts WHERE wind_speed > 200.0"
>   cnn <- connect
>   result <- getFloat cnn sql []
>   assertEqual "test_WindsAreReasonable" True (isNothing result)

> test_WeatherIsJust = TestCase $ do
>   -- first success
>   assertEqual "test_IsJust1" True (notNothing dt1) 
>   -- TBF: then failure
>   assertEqual "test_IsJust2" True (notNothing dt2) 
>     where
>       dt1 = fromGregorian 2006 1 2 0 0 0
>       dt2 = fromGregorian 2006 1 1 0 0 0
>       notNothing dt = dropWhile (==True) (map isJust (map unsafePerformIO (getValues dt))) == []
>       getValues dt = unsafePerformIO $ do
>         let target = dt
>         let f = 2.0 :: Float
>         let el = pi / 4.0 :: Radians
>         w <- getWeather . Just $ dt
>         return $ [wind w target
>         -- TBF: this does not work & is not being used: , tatm w target
>             , opacity w target f
>             , tsys w target f
>             , totalStringency w f el
>         -- TBF: no table! but not being used: , minOpacity w f el
>             , minTSysPrime w f el
>             ]


