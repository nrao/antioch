> module Antioch.WeatherTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Maybe
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)
> import qualified Control.Exception as E

> tests = TestList [
>     test_data0_11Day
>   , test_data0_11Night
>   , test_data36_47Day
>   , test_data36_47Night
>   -- , test_years
>   , test_getLastImportTime
>   , test_WeatherIsJust
>   , test_dataFirstLine
>   -- , test_fetchAnyWind
>   , test_WindsArePositive
>   , test_WindsAreReasonable
>   , test_forecastType
>   , test_correctWindSpeed
>   , test_gbt_wind
>   , test_wind
>   , test_bestOpacity
>      ]

> test_bestOpacity = TestCase $ do
>     let dt = fromGregorian 2006 6 10 8 0 0
>     w <- getWeatherTest $ Just dt
>     opacity <- bestOpacity w dt 1.1
>     print opacity
>     assertEqual "test_bestOpacity" (Just 7.219601e-3) opacity 

> test_forecastType = TestCase $ do
>   assertEqual "test_forecastType 1" 1 (forecastType dt1 dt1 dt1)
>   assertEqual "test_forecastType 2" 1 (forecastType dt2 dt1 dt1)
>   assertEqual "test_forecastType 3" 2 (forecastType dt3 dt1 dt1)
>   assertEqual "test_forecastType 4" 3 (forecastType dt4 dt1 dt1)
>   assertEqual "test_forecastType 5" 3 (forecastType dt5 dt1 dt1)
>   assertEqual "test_forecastType 6" 4 (forecastType dt6 dt1 dt1)
>   assertEqual "test_forecastType 7" 5 (forecastType dt7 dt1 dt1)
>   assertEqual "test_forecastType 8" 5 (forecastType dt8 dt1 dt1)
>     where
>   dt1 = fromGregorian 2006 2 2 12 0 0 -- diff = 0 hrs Type = I
>   dt2 = fromGregorian 2006 2 2 16 0 0 -- 4 hrs I
>   dt3 = fromGregorian 2006 2 2 20 0 0 -- 8 hrs II
>   dt4 = fromGregorian 2006 2 3  0 9 0 -- 12:09 III
>   dt5 = fromGregorian 2006 2 3  4 0 0 -- 16 hrs III 
>   dt6 = fromGregorian 2006 2 3  8 0 0 -- 20 hrs IV
>   dt7 = fromGregorian 2006 2 3 12 9 0 -- 24:09 V
>   dt8 = fromGregorian 2006 2 3 13 9 0 -- 25:09 V

> test_years = TestCase $ do 
>   test_year dt05 n
>   test_year dt06 val06
>   test_year dt07 n
>   test_year dt08 n
>   test_year dt09 n
>     where
>       test_year dt val = do
>          w <- getWeatherTest $ Just dt
>          wind' <- wind w (60 `addMinutes` dt)
>          assertEqual ("test_years_" ++ (toSqlString dt)) val (fromMaybe (-1.0) wind')
>       val06 = 5.8049664
>       n     = (-1.0) 
>       dt05  = fromGregorian 2005 2 1 0 0 0 
>       dt06  = fromGregorian 2006 2 1 0 0 0 
>       dt07  = fromGregorian 2007 2 1 0 0 0 
>       dt08  = fromGregorian 2008 2 1 0 0 0 
>       dt09  = fromGregorian 2009 2 1 0 0 0 

> test_getLastImportTime = TestCase $ do
>   cnn <- connectTest
>   let exp = fromGregorian 2010 7 20 19 11 0
>   res <- getLastImportTime cnn
>   assertEqual "test_getLastImportTime" exp res

> test_WindsArePositive = TestCase $ do
>   cnn <- connectTest
>   let sql = "SELECT wind_speed FROM forecasts WHERE wind_speed < 0.0"
>   result <- getFloat cnn sql []
>   assertEqual "test_WindsArePositive" True (isNothing result)
>   let sql = "SELECT wind_speed FROM gbt_weather WHERE wind_speed < 0.0"
>   result <- getFloat cnn sql []
>   assertEqual "test_W2WindsArePositive" True (isNothing result)

> test_WindsAreReasonable = TestCase $ do
>   cnn <- connectTest
>   let sql = "SELECT wind_speed FROM forecasts WHERE wind_speed > 200.0"
>   result <- getFloat cnn sql []
>   assertEqual "test_WindsAreReasonable" True (isNothing result)
>   let sql = "SELECT wind_speed FROM gbt_weather WHERE wind_speed > 200.0"
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
>         w <- getWeatherTest . Just $ dt
>         return $ [ wind w target
>                  , gbt_wind w target
>                  , opacity w target f
>                  , tsys w target f
>                  , totalStringency w f el Rcvr1_2 SpectralLine 
>                  , minTSysPrime w f el Rcvr1_2
>             ]

> test_dataFirstLine = TestCase $ do
>   let now = fromGregorian 2006 1 1 0 0 0  
>   let dt  = fromGregorian 2006 1 1 1 0 0 
>   w <- getWeatherTest $ Just now
>   wind' <- wind w dt
>   assertEqual "test_dataFirstLine_wind" 8.181183 (fromMaybe 0.0 wind')
>   wind' <- gbt_wind w dt
>   assertEqual "test_dataFirstLine_w2_wind" 6.57004 (fromMaybe 0.0 wind')
>   

This tests that indeed, if the appropriate forecast is found, an attempt at
finding any forecast can actually be successful.  To support this test, we
inserted the following into the 'weather' DB:
insert into weather_dates values (DEFAULT, '2007-01-01 01:00:00');
insert into forecasts values (DEFAULT, 3, 12554, 99.99);

> test_fetchAnyWind = TestCase $ do
>   let dt = fromGregorian 2007 1 1 1 0 0
>   w <- getWeatherTest $ Just dt
>   wind <- wind w dt
>   assertEqual "test_fetchAnyWind" 99.99 (fromMaybe 0.0 wind)

BETA: TestTWeather.testData0_11Night - uses 2007 in date, but server converts this
to 2006 date.  

> test_data0_11Night = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 1 9 0 0 
>   let freq = 11.2
>   w <- getWeatherTest $ Just now
>   wind' <- wind w dt
>   w2_wind' <- gbt_wind w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq
>   assertEqual "test_data0_11Night_opacity" 1.5096413e-2 (fromMaybe 0.0 opacity')
>   assertEqual "test_data0_11Night_tsys" 273.97748 (fromMaybe 0.0 tsys')
>   assertEqual "test_data0_11Night_wind"  2.3770752 (fromMaybe 0.0 wind')
>   assertEqual "test_data0_11Night_w2_wind" 1.5640465 (fromMaybe 0.0 w2_wind')

BETA: TestTWeather.testData0_11Day - uses 2007 in date, but server converts this
to 2006 date.  

> test_data0_11Day = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 1 18 0 0 
>   let freq = 11.2
>   w <- getWeatherTest $ Just now
>   wind' <- wind w dt
>   w2_wind' <- gbt_wind w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq
>   assertEqual "test_data0_11Day_opacity" 1.5868748e-2 (fromMaybe 0.0 opacity')
>   assertEqual "test_data0_11Day_tsys" 273.97607 (fromMaybe 0.0 tsys')
>   assertEqual "test_data0_11Day_wind" 3.784982 (fromMaybe 0.0 wind')
>   assertEqual "test_data0_11Day_w2_wind"  3.146323 (fromMaybe 0.0 w2_wind')

BETA: TestTWeather.testData36_47Night - uses 2007 in date, but server converts this
to 2006 date.  

> test_data36_47Night = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 3 4 0 0 
>   let freq = 22.6
>   w <- getWeatherTest $ Just now
>   wind' <- wind w dt
>   w2_wind' <- gbt_wind w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq
>   assertEqual "test_data36_47Night_opacity" 0.22306535 (fromMaybe 0.0 opacity')
>   assertEqual "test_data36_47Night_tsys" 277.59595 (fromMaybe 0.0 tsys')
>   assertEqual "test_data36_47Night_wind" 6.2714043 (fromMaybe 0.0 wind')
>   assertEqual "test_data36_47Night_w2_wind" 3.2963674 (fromMaybe 0.0 w2_wind')

BETA: TestTWeather.testData36_47Day - uses 2007 in date, but server converts this
to 2006 date.  

> test_data36_47Day = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 3 2 0 0 
>   let freq = 15.0 
>   w <- getWeatherTest $ Just now
>   wind' <- wind w dt
>   w2_wind' <- gbt_wind w dt
>   opacity' <- opacity w dt freq
>   tsys' <- tsys w dt freq
>   assertEqual "test_data36_47Day_opacity" 2.7477108e-2 (fromMaybe 0.0 opacity')
>   assertEqual "test_data36_47Day_tsys" 275.6452 (fromMaybe 0.0 tsys')
>   assertEqual "test_data36_47Day_wind" 6.359522 (fromMaybe 0.0 wind')
>   assertEqual "test_data36_47Day_w2_wind" 2.259703 (fromMaybe 0.0 w2_wind')

> test_correctWindSpeed = TestCase $ do
>   assertEqual "test_correctWindSpeed night" 8.774445 (correctWindSpeed night . mph2mps $ 17.66975)
>   assertEqual "test_correctWindSpeed day" 8.605942 (correctWindSpeed day . mph2mps $ 16.19775)
>     where
>       night = fromGregorian 2006 12 10 10 15 0
>       day = fromGregorian 2006 12 10 19 30 0

> test_gbt_wind = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 1 9 0 0 
>   w <- getWeatherTest $ Just now
>   w2_wind' <- gbt_wind w dt
>   assertEqual "test_gbt_wind_1"  1.5640465 (fromMaybe 0.0 w2_wind')
>   winds <- mapM (getGbtWind w) (dts dt)
>   assertEqual "test_gbt_wind_2" exp winds
>     where
>       dts start = map (\q -> (q*15) `addMinutes` start) [0..8]
>       getGbtWind w d = gbt_wind w d
>       exp = [Just 1.5640465,Just 1.5640465,Just 0.834291,Just 0.834291,Just 0.834291,Just 0.834291,Just 0.6365064,Just 0.6365064,Just 0.6365064]

> test_wind = TestCase $ do
>   let now = fromGregorian 2006 6 1 8 0 0 
>   let dt = fromGregorian 2006 6 1 9 0 0 
>   w <- getWeatherTest $ Just now
>   wind' <- wind w dt
>   assertEqual "test_wind_1"  2.3770752 (fromMaybe 0.0 wind')
>   winds <- mapM (getWind w) (dts dt)
>   assertEqual "test_wind_2"  exp winds
>     where
>       dts start = map (\q -> (q*15) `addMinutes` start) [0..8]
>       getWind w d = wind w d
>       exp = [Just 2.3770752,Just 2.3770752,Just 3.725532,Just 3.725532,Just 3.725532,Just 3.725532,Just 3.4385333,Just 3.4385333,Just 3.4385333]

> print_weather_values = TestCase $ do
>   mapM print_weather (dts start)
>   assertEqual "" True True 
>     where
>       start = fromGregorian 2004 6 10 0 0 0
>       dts  start = map (\days -> (days*24*60) `addMinutes` start) $ take 8 $ [0,30 ..]  
>       print_weather dt = do
>          w <- getWeatherTest $ Just dt
>          wind' <- wind w dt --(60 `addMinutes` dt)
>          wind_mph' <- wind_mph w dt
>          ir <- irradiance w dt
>          --gbt_wind' <- gbt_wind w dt --(60 `addMinutes` dt)
>          --gbt_ir <- gbt_irradiance w dt
>          op2 <- opacity w dt 2
>          tsys2 <- tsys w dt 2
>          op20 <- opacity w dt 20
>          tsys20 <- tsys w dt 20
>          op100 <- opacity w dt 100
>          tsys100 <- tsys w dt 100
>          print $ (toSqlString dt) ++ ":" ++ (sv wind') ++ (sv wind_mph') ++ (sv ir) ++ (sv op2) ++ (sv tsys2)  ++ (sv op20) ++ (sv tsys20) ++ (sv op100) ++ (sv tsys100)
>       sv v = " " ++ (show $ fromMaybe (-1.0) v)

