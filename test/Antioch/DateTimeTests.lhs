> module Antioch.DateTimeTests where

> import Antioch.DateTime
> import Antioch.SLALib
> import Antioch.Utilities
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score
> --import Antioch.SunRiseSet
> import Data.Time (getCurrentTimeZone, utcToLocalTime, localTimeToUTC)

> tests = TestList [test_secondsToMJD
>                 , test_addMonth
>                 , test_translations
>                 , test_setHour
>                 , test_isDayTime
>                 -- TBF, WTF: toggle this once sponsor testing is done
>                 --, test_isDayTime_2
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

> test_addMonth = TestCase $ do
>   let dt1 = fromGregorian 2006 11 15 11 0 0 
>   let dt2 = fromGregorian 2006 12 15 11 0 0 
>   let dt3 = fromGregorian 2007  1 15 11 0 0 
>   assertEqual "test_addMonth_1" dt2 (addMonth dt1)
>   assertEqual "test_addMonth_2" dt3 (addMonth dt2)

TBF must be some way to factor out the common code in these, but ...

> test_translations =  TestCase $ do
>     edt <- getCurrentTimeZone -- TBF does this adjust for daylight savings?
>     let dt = fromGregorian 2009 10 11 12 15 0
>     -- DateTime <-> UTCTime
>     let utc = fromSeconds dt
>     assertEqual "test_translations_1" dt (toSeconds utc)
>     -- UTCTime <-> LocalTime
>     let lt = utcToLocalTime edt utc
>     assertEqual "test_translations_2" utc (localTimeToUTC edt lt)
>     -- DateTime <-> (Http) String
>     let dt_http = toHttpString dt
>     assertEqual "test_translations_3" (Just dt) (fromHttpString dt_http)
>     -- DateTime <-> (Sql) String
>     let http_sql = toSqlString dt
>     assertEqual "test_translations_4" (Just dt) (fromSqlString http_sql)
>     -- UTCTime <-> (Http) String
>     let utc_http = formatUTCTime httpFormat utc
>     assertEqual "test_translations_5" (Just utc) (parseUTCTime httpFormat utc_http)
>     -- LocalTime <-> (Http) String
>     let lt_http = formatLocalTime httpFormat lt
>     assertEqual "test_translations_6" (Just lt) (parseLocalTime httpFormat lt_http)

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
>     dts = [ (qtr*15) `addMinutes'` start | qtr <- [0 .. (4*24 - 1)]]
>     isDayTimes = map isDayTime dts
>     exp = (take (12*4 + 3) $ repeat False) ++ (take (9*4 + 2) $ repeat True) ++ (take (1*4 + 3) $ repeat False)

> test_isPTCSDayTime = TestCase $ do
>     assertEqual "test_isPTCSDayTime_1" False (isPTCSDayTime dt1)
>     assertEqual "test_isPTCSDayTime_2" True (isPTCSDayTime dt2)
>     assertEqual "test_isPTCSDayTime_3" True  (isPTCSDayTime dt3)
>     assertEqual "test_isPTCSDayTime_4" True  (isPTCSDayTime dt4)
>     assertEqual "test_isPTCSDayTime_5" False (isPTCSDayTime dt5)
>     assertEqual "test_isPTCSDayTime_6" True (isPTCSDayTime dt10)
>     assertEqual "test_isPTCSDayTime_7" True (isPTCSDayTime dt11)
>     assertEqual "test_isPTCSDayTime_8" False (isPTCSDayTime dt12)
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
