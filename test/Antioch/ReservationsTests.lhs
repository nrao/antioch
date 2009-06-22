> module Antioch.ReservationsTests where

> import Antioch.DateTime
> import Antioch.Reservations
> import Test.HUnit

> tests = TestList [
>   test_parseResDatesXML
>   ]

> test_parseResDatesXML = TestCase $ do
>     assertEqual "test_parseResDatesXML" dateRanges (parseResDatesXML dateRangeXmlStr "error")  
>   where
>     start = fromGregorian 2009 8 8 0 0 0
>     end   = fromGregorian 2009 8 9 0 0 0
>     dateRanges = [(start, end)]

Test XML:

> dateRangeXmlStr :: String
> dateRangeXmlStr = "<nrao:user domestic='true' id='119'><nrao:reservation id='2367'><nrao:startDate>2009-08-08</nrao:startDate><nrao:endDate>2009-08-09</nrao:endDate></nrao:reservation></nrao:user>"
