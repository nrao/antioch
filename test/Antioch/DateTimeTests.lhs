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
>                 --, test_toDayOfYear
>                 --, test_fromHoursToHourMins
>                 , test_isDayTime
>                 -- TBF, WTF: toggle this once sponsor testing is done
>                 --, test_isDayTime_2
>                 , test_isPTCSDayTime
>                 , test_getRise
>                 --, test_getSunRiseSets
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

Below unit tests were deprecated when SunRiseSet.lhs was refactored.

> {-
> test_toDayOfYear =  TestCase $ do
>     assertEqual "test_toDayOfYear_1" 1 (toDayOfYear dt1)
>     assertEqual "test_toDayOfYear_2" 1 (toDayOfYear dt2)
>     assertEqual "test_toDayOfYear_3" 1 (toDayOfYear dt3)
>     assertEqual "test_toDayOfYear_4" 2 (toDayOfYear dt4)
>     assertEqual "test_toDayOfYear_5" 91 (toDayOfYear dt5)
>     assertEqual "test_toDayOfYear_6" 365 (toDayOfYear dt6)
>   where
>     dt1 = fromGregorian 2006 1 1 0 0 0
>     dt2 = fromGregorian 2006 1 1 1 0 0
>     dt3 = fromGregorian 2006 1 1 23 0 0
>     dt4 = fromGregorian 2006 1 2 0 0 0
>     dt5 = fromGregorian 2006 4 1 0 0 0
>     dt6 = fromGregorian 2006 12 31 12 0 0

> test_fromHoursToHourMins =  TestCase $ do
>     assertEqual "test_fromHoursToHourMins_1" (12,0) (fromHoursToHourMins 12.0)
>     assertEqual "test_fromHoursToHourMins_2" (12,30) (fromHoursToHourMins 12.5)
>     assertEqual "test_fromHoursToHourMins_3" (13,0) (fromHoursToHourMins 13.0)

> test_getSunRiseSets = TestCase $ do
>   assertEqual "test_getSunRiseSets_1" exp (getSunRiseSets dt sunRise sunSet)
>   assertEqual "test_getSunRiseSets_2" exp_2 (getSunRiseSets dt ptcsSunRise_V2 ptcsSunSet_V2)
>     where 
>   dt = fromGregorian 2006 1 1 0 0 0
>   dt1 = fromGregorian 2005 12 31 12 33 0
>   dt2 = fromGregorian 2005 12 31 22  4 0
>   dt3 = fromGregorian 2006  1  1 12 33 0
>   dt4 = fromGregorian 2006  1  1 22  5 0
>   dt5 = fromGregorian 2006  1  2 12 34 0
>   dt6 = fromGregorian 2006  1  2 22  6 0
>   exp = [(dt1,dt2),(dt3,dt4),(dt5,dt6)]
>   dt2_2 = fromGregorian 2006 1 1 1 4 0
>   dt4_2 = fromGregorian 2006 1 2 1 5 0
>   dt6_2 = fromGregorian 2006 1 3 1 6 0
>   exp_2 = [(dt1,dt2_2),(dt3,dt4_2),(dt5,dt6_2)]
> -}

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
>     assertEqual "test_isPTCSDayTime_2" False (isPTCSDayTime dt2)
>     assertEqual "test_isPTCSDayTime_3" True  (isPTCSDayTime dt3)
>     assertEqual "test_isPTCSDayTime_4" True  (isPTCSDayTime dt4)
>     assertEqual "test_isPTCSDayTime_5" False (isPTCSDayTime dt5)
>     assertEqual "test_isPTCSDayTime_6" True (isPTCSDayTime_V2 dt10)
>     assertEqual "test_isPTCSDayTime_7" True (isPTCSDayTime_V2 dt11)
>     assertEqual "test_isPTCSDayTime_8" False (isPTCSDayTime_V2 dt12)
>   where
>     -- physical rise = 12.6, set = 22.1
>     -- PTCS rise = 12.6 + 2 = 14.6
>     -- PTCS set  = 22.1 + 3 = 25.1 = 1.1
>     dt1 = fromGregorian 2006 1 1 10 0 0 
>     dt2 = fromGregorian 2006 1 1 14 0 0 
>     dt3 = fromGregorian 2006 1 1 15 0 0 
>     dt4 = fromGregorian 2006 1 1 23 10 0 
>     dt5 = fromGregorian 2006 1 2 5  0 0  
>     dt10 =  fromGregorian 2006 1 10 23 0 0 -- is ptcsDayTime_V2
>     dt11 = fromGregorian 2006 1 11 0 0 0 -- is ptcsDayTime_V2
>     dt12 = fromGregorian 2006 1 11 2 0 0 -- is NOT ptcsDayTime_V2

> test_getRise = TestCase $ do
>     assertEqual "test_getRise_1" dt1_2 (getRise dt1_1)
>  where
>     getRise dt = fst $ sunRiseAndSet dt
>     dt1_1 = fromGregorian 2006 1 1 10 0 0
>     dt1_2 = fromGregorian 2006 1 1 12 33 0


