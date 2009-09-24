> module Antioch.WeatherTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Maybe
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)

> tests = TestList [
>     test_data0_11Day
>   , test_data0_11Night
>   , test_data36_47Day
>   , test_data36_47Night
>   , test_years
>   , test_WeatherIsJust
>   , test_dataFirstLine
>   , test_WindsArePositive
>   , test_WindsAreReasonable
>      ]

TBF: only 2006 in our Weather DB still!

> test_years = TestCase $ do 
>   test_year dt05 n
>   test_year dt06 val06
>   test_year dt07 n
>   test_year dt08 n
>   test_year dt09 n
>     where
>       test_year dt val = do
>          w <- getWeather $ Just dt
>          wind' <- wind w (60 `addMinutes'` dt)
>          assertEqual ("test_years_" ++ (toSqlString dt)) val (fromMaybe (-1.0) wind')
>       val06 = 5.8049664
>       n     = (-1.0) 
>       dt05  = fromGregorian 2005 2 1 0 0 0 
>       dt06  = fromGregorian 2006 2 1 0 0 0 
>       dt07  = fromGregorian 2007 2 1 0 0 0 
>       dt08  = fromGregorian 2008 2 1 0 0 0 
>       dt09  = fromGregorian 2009 2 1 0 0 0 

> test_WindsArePositive = TestCase $ do
>   cnn <- connect
>   let sql = "SELECT wind_speed FROM forecasts WHERE wind_speed < 0.0"
>   result <- getFloat cnn sql []
>   assertEqual "test_WindsArePositive" True (isNothing result)
>   let sql = "SELECT wind_speed FROM weather_station2 WHERE wind_speed < 0.0"
>   result <- getFloat cnn sql []
>   assertEqual "test_W2WindsArePositive" True (isNothing result)

> test_WindsAreReasonable = TestCase $ do
>   cnn <- connect
>   let sql = "SELECT wind_speed FROM forecasts WHERE wind_speed > 200.0"
>   result <- getFloat cnn sql []
>   assertEqual "test_WindsAreReasonable" True (isNothing result)
>   let sql = "SELECT wind_speed FROM weather_station2 WHERE wind_speed > 200.0"
>   result <- getFloat cnn sql []
>   assertEqual "test_W2WindsAreReasonable" True (isNothing result)

> test_WeatherIsJust = TestCase $ do
>   assertEqual "test_IsJust1" True (notNothing dt1) 
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
>         return $ [ wind w target
>                  , w2_wind w target
>         -- TBF: this does not work & is not being used: , tatm w target
>                  , opacity w target f
>                  , tsys w target f
>                  , totalStringency w f el
>         -- TBF: no table! but not being used: , minOpacity w f el
>                  , minTSysPrime w f el
>             ]

> test_dataFirstLine = TestCase $ do
>   let now = fromGregorian 2006 1 1 0 0 0  
>   let dt  = fromGregorian 2006 1 1 1 0 0 
>   w <- getWeather $ Just now
>   wind' <- wind w dt
>   assertEqual "test_dataFirstLine_wind" 6.13935418989 (fromMaybe 0.0 wind')
>   wind' <- w2_wind w dt
>   assertEqual "test_dataFirstLine_w2_wind" 6.57004 (fromMaybe 0.0 wind')

BETA: TestTWeather.testData0_11Night - uses 2007 in date, but server converts this
to 2006 date.  

> test_data0_11Night = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 1 9 0 0 
>   let freq = 11.2
>   w <- getWeather $ Just now
>   wind' <- wind w dt
>   w2_wind' <- w2_wind w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq
>   assertAlmostEqual "test_data0_11Night_opacity" 4 0.0149 (fromMaybe 0.0 opacity')
>   assertAlmostEqual "test_data0_11Night_tsys" 3 273.6758 (fromMaybe 0.0 tsys')
>   assertEqual "test_data0_11Night_wind"  1.3301781 (fromMaybe 0.0 wind')
>   assertEqual "test_data0_11Night_w2_wind"  1.564047 (fromMaybe 0.0 w2_wind')

BETA: TestTWeather.testData0_11Day - uses 2007 in date, but server converts this
to 2006 date.  

> test_data0_11Day = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 1 18 0 0 
>   let freq = 11.2
>   w <- getWeather $ Just now
>   wind' <- wind w dt
>   w2_wind' <- w2_wind w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq
>   assertAlmostEqual "test_data0_11Day_opacity" 4 0.01682 (fromMaybe 0.0 opacity')
>   assertAlmostEqual "test_data0_11Day_tsys" 3 275.3244 (fromMaybe 0.0 tsys')
>   assertEqual "test_data0_11Day_wind"  3.337157 (fromMaybe 0.0 wind')
>   assertEqual "test_data0_11Day_w2_wind"  3.146323 (fromMaybe 0.0 w2_wind')

BETA: TestTWeather.testData36_47Night - uses 2007 in date, but server converts this
to 2006 date.  

> test_data36_47Night = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 3 4 0 0 
>   let freq = 22.6
>   w <- getWeather $ Just now
>   wind' <- wind w dt
>   w2_wind' <- w2_wind w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq
>   assertAlmostEqual "test_data36_47Night_opacity" 4 0.2493 (fromMaybe 0.0 opacity')
>   assertAlmostEqual "test_data36_47Night_tsys" 3 277.3694 (fromMaybe 0.0 tsys')
>   assertAlmostEqual "test_data36_47Night_wind" 4 3.2750 (fromMaybe 0.0 wind')
>   assertAlmostEqual "test_data36_47Night_w2_wind" 4 3.296367 (fromMaybe 0.0 w2_wind')

BETA: TestTWeather.testData36_47Day - uses 2007 in date, but server converts this
to 2006 date.  

> test_data36_47Day = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 3 2 0 0 
>   let freq = 15.0 
>   w <- getWeather $ Just now
>   wind' <- wind w dt
>   w2_wind' <- w2_wind w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq
>   assertAlmostEqual "test_data36_47Day_opacity" 4 0.0269 (fromMaybe 0.0 opacity')
>   assertAlmostEqual "test_data36_47Day_tsys" 3 275.6069 (fromMaybe 0.0 tsys')
>   assertAlmostEqual "test_data36_47Day_wind" 4 4.2099 (fromMaybe 0.0 wind')
>   assertAlmostEqual "test_data36_47Day_w2_wind" 4 2.259703 (fromMaybe 0.0 w2_wind')

Test utilities

TBF: place this in utils ...

> assertAlmostEqual :: String -> Int -> Float -> Float -> IO ()
> assertAlmostEqual name places expected value =
>     assertBool name $ abs (value - expected) < epsilon
>   where
>     epsilon = 1.0 / 10.0 ** fromIntegral places

