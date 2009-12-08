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
