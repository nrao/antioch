> module Antioch.DateTimeTests where

> import Antioch.DateTime
> import Antioch.SLALib
> import Antioch.Utilities
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score
> --import Antioch.SunRiseSet
> import Data.Time (getCurrentTimeZone, getTimeZone, utcToLocalTime, localTimeToUTC)
> import Control.Monad.Trans             (liftIO)

> tests = TestList [test_secondsToMJD
>                 , test_addMonth
>                 , test_isHighRFITime
>                 , test_httpTzToDt
>                 , test_time_translations
>                 , test_setHour
>                 , test_isDayTime
>                 , test_isDayTime_2
>                 , test_roundToHour
>                 , test_roundToHalfPast
>                 , test_isPTCSDayTime
>                 , test_getRise
>                 , test_getSet
>                  ]

> test_setHour = TestCase $ do
>   let hour = 8
>   let dt = fromGregorian 2006 10 1 23 10 23
>   let exp = fromGregorian 2006 10 1 hour 0 0
>   assertEqual "test_setHour" exp (setHour hour dt)

BETA: results compared to using 3rd party libraries used in beta's TimeAgent

> test_secondsToMJD = TestCase $ do
>   let dt = fromGregorian 2006 10 15 11 0 0 
>   let mjd = secondsToMJD dt
>   -- BETA: difference due to Float vs. Double
>   -- printing mjd reveals it to really be  54023.457
>   assertEqual "test_secondsToMJD" 54023.4583333 mjd

> test_httpTzToDt = TestCase $ do
>     res <- httpTzToDt "2011-03-13 01A15A00" "ET"
>     assertEqual "test_httpTzToDt_1" (2011, 3, 13, 6, 15, 0) (toGregorian res)
>     res <- httpTzToDt "2011-03-13 02A00A00" "ET"
>     assertEqual "test_httpTzToDt_2" (2011, 3, 13, 7, 0, 0) (toGregorian res)
>     res <- httpTzToDt "2011-03-13 03A30A00" "ET"
>     assertEqual "test_httpTzToDt_3" (2011, 3, 13, 7, 30, 0) (toGregorian res)
>     res <- httpTzToDt "2011-03-13 06A00A00" "UTC"
>     assertEqual "test_httpTzToDt_4" (2011, 3, 13, 6, 0, 0) (toGregorian res)
>     res <- httpTzToDt "2011-03-13 07A45A00" "UTC"
>     assertEqual "test_httpTzToDt_5" (2011, 3, 13, 7, 45, 0) (toGregorian res)
>     res <- httpTzToDt "2011-03-13 07A30A00" "UTC"
>     assertEqual "test_httpTzToDt_6" (2011, 3, 13, 7, 30, 0) (toGregorian res)
>     res <- httpTzToDt "2011-11-06 01A00A00" "ET"
>     assertEqual "test_httpTzToDt_7" (2011, 11, 6, 5, 0, 0) (toGregorian res)
>     res <- httpTzToDt "2011-11-06 02A00A00" "ET"
>     assertEqual "test_httpTzToDt_8" (2011, 11, 6, 7, 0, 0) (toGregorian res)
>     res <- httpTzToDt "2011-11-06 05A00A00" "UTC"
>     assertEqual "test_httpTzToDt_9" (2011, 11, 6, 5, 0, 0) (toGregorian res)
>     res <- httpTzToDt "2011-11-06 06A00A00" "UTC"
>     assertEqual "test_httpTzToDt_10" (2011, 11, 6, 6, 0, 0) (toGregorian res)
>     res <- httpTzToDt "2011-11-06 07A00A00" "UTC"
>     assertEqual "test_httpTzToDt_11" (2011, 11, 6, 7, 0, 0) (toGregorian res)

Conversions verified at
http://www.timeanddate.com/worldclock/converter.html

> test_isHighRFITime = TestCase $ do
>   -- Spring forward
>   res <- isHighRFITime . fromGregorian 2011 3 12 12 45 $ 0
>   assertEqual "test_isHighRFITime_1" False res -- 07:45 EST
>   res <- isHighRFITime . fromGregorian 2011 3 12 13 0 $ 0
>   assertEqual "test_isHighRFITime_2" True res -- 08:00 EST
>   res <- isHighRFITime . fromGregorian 2011 3 13 0 45 $ 0
>   assertEqual "test_isHighRFITime_3" True res -- 19:45 EST
>   res <- isHighRFITime . fromGregorian 2011 3 13 1 0 $ 0
>   assertEqual "test_isHighRFITime_4" False res -- 20:00 EST
>   res <- isHighRFITime . fromGregorian 2011 3 13 11 45 $ 0
>   assertEqual "test_isHighRFITime_5" False res -- 07:45 EDT
>   res <- isHighRFITime . fromGregorian 2011 3 13 12 0 $ 0
>   assertEqual "test_isHighRFITime_6" True res -- 08:00 EDT
>   res <- isHighRFITime . fromGregorian 2011 3 13 23 45 $ 0
>   assertEqual "test_isHighRFITime_7" True res -- 19:45 EDT
>   res <-isHighRFITime . fromGregorian 2011 3 14 0 0 $ 0
>   assertEqual "test_isHighRFITime_8" False res -- 20:00 EDT
>   -- Fall back
>   res <- isHighRFITime . fromGregorian 2011 11 5 11 45 $ 0
>   assertEqual "test_isHighRFITime_9" False res -- 07:45 EDT
>   res <- isHighRFITime . fromGregorian 2011 11 5 12 0 $ 0
>   assertEqual "test_isHighRFITime_10" True res -- 08:00 EDT
>   res <- isHighRFITime . fromGregorian 2011 11 5 23 45 $ 0
>   assertEqual "test_isHighRFITime_11" True res -- 19:45 EDT
>   res <- isHighRFITime . fromGregorian 2011 11 6 0 0 $ 0
>   assertEqual "test_isHighRFITime_12" False res -- 20:00 EDT
>   res <- isHighRFITime . fromGregorian 2011 11 6 12 45 $ 0
>   assertEqual "test_isHighRFITime_13" False res -- 07:45 EST
>   res <- isHighRFITime . fromGregorian 2011 11 6 13 0 $ 0
>   assertEqual "test_isHighRFITime_14" True res -- 08:00 EST
>   res <- isHighRFITime . fromGregorian 2011 11 7 0 45 $ 0
>   assertEqual "test_isHighRFITime_15" True res -- 19:45 EST
>   res <- isHighRFITime . fromGregorian 2011 11 7 1 0 $ 0
>   assertEqual "test_isHighRFITime_16" False res -- 20:00 EST

> test_addMonth = TestCase $ do
>   let dt1 = fromGregorian 2006 11 15 11 0 0 
>   let dt2 = fromGregorian 2006 12 15 11 0 0 
>   let dt3 = fromGregorian 2007  1 15 11 0 0 
>   assertEqual "test_addMonth_1" dt2 (addMonth dt1)
>   assertEqual "test_addMonth_2" dt3 (addMonth dt2)

> test_time_translations =  TestCase $ do
>     let dt = fromGregorian 2009 10 11 12 15 0
>     -- DateTime <-> UTCTime
>     let utc = fromSeconds dt
>     edt <- getTimeZone utc -- need utc to handle DST
>     assertEqual "test_time_translations_1" dt (toSeconds utc)
>     -- UTCTime <-> LocalTime
>     let lt = utcToLocalTime edt utc
>     assertEqual "test_time_translations_2" utc (localTimeToUTC edt lt)
>     -- DateTime <-> (Http) String
>     let dt_http = toHttpString dt
>     assertEqual "test_time_translations_3" (Just dt) (fromHttpString dt_http)
>     -- DateTime <-> (Sql) String
>     let http_sql = toSqlString dt
>     assertEqual "test_time_translations_4" (Just dt) (fromSqlString http_sql)
>     -- UTCTime <-> (Http) String
>     let utc_http = formatUTCTime httpFormat utc
>     assertEqual "test_time_translations_5" (Just utc) (parseUTCTime httpFormat utc_http)
>     -- LocalTime <-> (Http) String
>     let lt_http = formatLocalTime httpFormat lt
>     assertEqual "test_time_translations_6" (Just lt) (parseLocalTime httpFormat lt_http)
>     -- Now for the nasty DST stuff
>     -- Spring forward
>     let dt = fromGregorian 2011 3 13 6 0 0
>     let utc = fromSeconds dt
>     edt <- getTimeZone utc
>     let lt = utcToLocalTime edt utc
>     assertEqual "test_time_translations_7" "2011-03-13 01A00A00" (formatLocalTime httpFormat lt) 
>     let dt = fromGregorian 2011 3 13 7 0 0
>     let utc = fromSeconds dt
>     edt <- getTimeZone utc
>     let lt = utcToLocalTime edt utc
>     assertEqual "test_time_translations_8" "2011-03-13 03A00A00" (formatLocalTime httpFormat lt) 
>     -- Fall back
>     let dt = fromGregorian 2011 11 6 5 0 0
>     let utc = fromSeconds dt
>     edt <- getTimeZone utc
>     let lt = utcToLocalTime edt utc
>     assertEqual "test_time_translations_9" "2011-11-06 01A00A00" (formatLocalTime httpFormat lt) 
>     let dt = fromGregorian 2011 11 6 6 0 0
>     let utc = fromSeconds dt
>     edt <- getTimeZone utc
>     let lt = utcToLocalTime edt utc
>     assertEqual "test_time_translations_10" "2011-11-06 01A00A00" (formatLocalTime httpFormat lt) 
>     let dt = fromGregorian 2011 11 6 7 0 0
>     let utc = fromSeconds dt
>     edt <- getTimeZone utc
>     let lt = utcToLocalTime edt utc
>     assertEqual "test_time_translations_11" "2011-11-06 02A00A00" (formatLocalTime httpFormat lt) 

> test_roundToHour = TestCase $ do
>     let inp = fromGregorian 2010 11 16 11 29 0
>     let exp = fromGregorian 2010 11 16 11  0 0
>     assertEqual "test_roundToHour_1" exp (roundToHour inp)
>     let inp = fromGregorian 2010 11 16 11 30 0
>     let exp = fromGregorian 2010 11 16 12  0 0
>     assertEqual "test_roundToHour_2" exp (roundToHour inp)
>     let inp = fromGregorian 2010 11 16 11 31 0
>     let exp = fromGregorian 2010 11 16 12  0 0
>     assertEqual "test_roundToHour_3" exp (roundToHour inp)

> test_roundToHalfPast = TestCase $ do
>     let inp = fromGregorian 2010 11 16 11 59 0
>     let exp = fromGregorian 2010 11 16 11 30 0
>     assertEqual "test_roundToHalfPast_1" exp (roundToHalfPast inp)
>     let inp = fromGregorian 2010 11 16 12  0 0
>     let exp = fromGregorian 2010 11 16 12 30 0
>     assertEqual "test_roundToHalfPast_2" exp (roundToHalfPast inp)
>     let inp = fromGregorian 2010 11 16 12  1 0
>     let exp = fromGregorian 2010 11 16 12 30 0
>     assertEqual "test_roundToHalfPast_3" exp (roundToHalfPast inp)

> test_isDayTime = TestCase $ do
>     assertEqual "test_isDayTime_1" False (isDayTime dt1)
>     assertEqual "test_isDayTime_2" True  (isDayTime dt2)
>     assertEqual "test_isDayTime_3" True  (isDayTime dt3)
>     assertEqual "test_isDayTime_4" False (isDayTime dt4)
>     assertEqual "test_isDayTime_5" False (isDayTime dt5)
>   where
>     -- physical rise = 12.6, set = 22.1
>     dt1 = fromGregorian 2006 1 1 10 0 0 
>     dt2 = fromGregorian 2006 1 1 14 0 0 
>     dt3 = fromGregorian 2006 1 1 15 0 0 
>     dt4 = fromGregorian 2006 1 1 23 10 0 
>     dt5 = fromGregorian 2006 1 2 5  0 0  

This next test is to be the mirror image of the test in:
antioch/admin/tests/TestSolarHeating.testIsDayTime

> test_isDayTime_2 = TestCase $ do
>     assertEqual "test_isDayTime_2_1" (exp) (isDayTimes)
>   where
>     start = fromGregorian 2010 1 1 0 0 0
>     dts = [ (qtr*15) `addMinutes` start | qtr <- [0 .. (4*24 - 1)]]
>     isDayTimes = map isDayTime dts
>     exp = (take (12*4 + 2) $ repeat False) ++ (take (9*4 + 2) $ repeat True) ++ (take (1*4 + 4) $ repeat False)

> test_isPTCSDayTime = TestCase $ do
>     assertEqual "test_isPTCSDayTime_1" False (isPTCSDayTime id dt1)
>     assertEqual "test_isPTCSDayTime_2" True (isPTCSDayTime id dt2)
>     assertEqual "test_isPTCSDayTime_3" True  (isPTCSDayTime id dt3)
>     assertEqual "test_isPTCSDayTime_4" True  (isPTCSDayTime id dt4)
>     assertEqual "test_isPTCSDayTime_5" False (isPTCSDayTime id dt5)
>     assertEqual "test_isPTCSDayTime_6" True (isPTCSDayTime id dt10)
>     assertEqual "test_isPTCSDayTime_7" True (isPTCSDayTime id dt11)
>     assertEqual "test_isPTCSDayTime_8" False (isPTCSDayTime id dt12)
>   where
>     -- physical rise = 12.6, set = 22.1
>     -- PTCS rise = 12.6 + 0 = 12.6
>     -- PTCS set  = 22.1 + 3 = 25.1 = 1.1
>     dt1 = fromGregorian 2006 1 1 10 0 0 
>     dt2 = fromGregorian 2006 1 1 14 0 0 
>     dt3 = fromGregorian 2006 1 1 15 0 0 
>     dt4 = fromGregorian 2006 1 1 23 10 0 
>     dt5 = fromGregorian 2006 1 2 5  0 0  
>     dt10 = fromGregorian 2006 1 10 23 0 0 -- is ptcsDayTime
>     dt11 = fromGregorian 2006 1 11 0 0 0 -- is ptcsDayTime
>     dt12 = fromGregorian 2006 1 11 2 0 0 -- is NOT ptcsDayTime

> test_getRise = TestCase $ do
>     assertEqual "test_getRise_1" dt1_2 (getRise dt1_1)
>     assertEqual "test_getRise_2" dt2_2 (getRise dt2_1)
>  where
>     getRise dt = fst $ sunRiseAndSet' dt
>     dt1_1 = fromGregorian 2006 1 1 10 0 0
>     dt1_2 = fromGregorian 2006 1 1 12 33 37
>     dt2_1 = fromGregorian 2006 7 1 0 0 0
>     dt2_2 = fromGregorian 2006 7 1 9 59 58

> test_getSet = TestCase $ do
>     assertEqual "test_getSet_1" dt1_2 (getSet dt1_1)
>     assertEqual "test_getSet_2" dt2_2 (getSet dt2_1)
>  where
>     getSet dt = snd $ sunRiseAndSet' dt
>     dt1_1 = fromGregorian 2006 1 1 1 0 0
>     dt1_2 = fromGregorian 2006 1 1 22 4 38
>     dt2_1 = fromGregorian 2006 7 1 0 0 0
>     dt2_2 = 1151801059 --fromGregorian 2006 7 2 0 44 19

