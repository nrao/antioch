> module Antioch.DateTimeTests where

> import Antioch.DateTime
> import Antioch.SLALib
> import Antioch.Utilities
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score
> import Data.Time (getCurrentTimeZone, utcToLocalTime, localTimeToUTC)

> tests = TestList [test_secondsToMJD
>                 , test_addMonth
>                 , test_translations
>                 , test_setHour
>                 , test_toDayOfYear
>                 , test_fromHoursToHourMins
>                 , test_isDayTime
>                 , test_getRise
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

> test_isDayTime = TestCase $ do
>     assertEqual "test_getRise" False (isDayTime dt1)
>     assertEqual "test_getRise" True (isDayTime dt2)
>     assertEqual "test_getRise" False (isDayTime dt3)
>     assertEqual "test_getRise" False (isDayTime dt4)
>   where
>     dt1 = fromGregorian 2006 1 1 10 0 0 -- rise = 12.6, set = 22.1
>     dt2 = fromGregorian 2006 1 1 15 0 0 -- rise = 12.6, set = 22.1
>     dt3 = fromGregorian 2006 1 1 23 10 0 -- rise = 12.6, set = 22.1
>     dt4 = fromGregorian 2006 1 2 5  0 0  -- rise = 12.6, set = 22.1

> test_getRise = TestCase $ do
>     assertEqual "test_getRise_1" dt1_2 (getRise dt1_1)
>  where
>     dt1_1 = fromGregorian 2006 1 1 10 0 0
>     dt1_2 = fromGregorian 2006 1 1 12 33 0


