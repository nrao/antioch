> module Antioch.DSSReversionTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.DSSReversion
> import Antioch.DSSData (connect)
> import Test.HUnit

This is a hard module to tests, since most of the code is closely tied
to writing things to a DB.  We don't test anything that writes to a DB
(see DSSDataTests to find out why).
So, we'll test here what we can.

> tests = TestList [test_serializePeriod
>                 , test_representPeriod
>                 , test_serializePeriodAccounting
>                 , test_getNewestID
>                  ]

> test_serializePeriod = TestCase $ do
>     assertEqual "serializePeriod" exp $ serializePeriod getTestPeriod 10 1
>   where
>     exp = "[{\"pk\": 101, \"model\": \"scheduler.period\", \"fields\": {\"score\": 1.05, \"moc_ack\": false, \"forecast\": \"2006-02-01 00:00:00\", \"start\": \"2006-02-01 12:30:00\", \"state\": 1, \"session\": 0, \"duration\": 120, \"accounting\": 10, \"backup\": false}}]"

> test_representPeriod = TestCase $ do
>     assertEqual "serializePeriod" exp $ representPeriod getTestPeriod 
>   where
>     exp = "Period for Session (101): \"2006-02-01 12:30:00\" for 2.0 Hrs ('P')"

> test_serializePeriodAccounting = TestCase $ do
>     let result = serializePeriodAccounting getTestPeriod 10
>     assertEqual "serializePeriodAccounting" exp result 
>   where
>     exp = "[{\"pk\": 10, \"model\": \"scheduler.period_accounting\", \"fields\": {\"scheduled\": 0.0, \"other_session_rfi\": 0.0, \"description\": null, \"other_session_weather\": 0.0, \"lost_time_other\": 0.0, \"short_notice\": 0.0, \"not_billable\": \"0\", \"lost_time_weather\": 0.0, \"other_session_other\": 0.0, \"lost_time_rfi\": 0.0}}]"

> test_getNewestID = TestCase $ do
>     cnn <- connect
>     id <- getNewestID cnn "periods"
>     assertEqual "getNewestID" 6 id

Utilities:

> getTestPeriod = defaultPeriod { peId = 101
>                               , pScore = 1.05
>                               , startTime = fromGregorian 2006 2 1 12 30 0
>                               , duration = 2*60
>                               , pForecast = fromGregorian 2006 2 1 0 0 0
>                               }
>                  

