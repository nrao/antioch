> module Antioch.ReservationsTests where

> import Antioch.DateTime
> import Antioch.Reservations
> import Test.HUnit

> tests = TestList [
>     test_parseResDatesXML
>   , test_parseResDatesXML2
>   ]

> test_parseResDatesXML = TestCase $ do
>     assertEqual "test_parseResDatesXML" dateRanges (parseResDatesXML xml1 "error")  
>   where
>     start = fromGregorian 2009 8 8 0 0 0
>     end   = fromGregorian 2009 8 9 0 0 0
>     dateRanges = [(start, end)]

> test_parseResDatesXML2 = TestCase $ do
>     --let r = parseResDatesXML xml2 "error"
>     --print r
>     --print $ map (\(s, e) -> (toSqlString s) ++ ", " ++ (toSqlString e)) r
>     assertEqual "test_parseResDatesXML2" dateRanges (parseResDatesXML xml2 "error")  
>   where
>     s1 = fromGregorian 2009 7 10 0 0 0
>     e1 = fromGregorian 2009 7 22 0 0 0
>     s2 = fromGregorian 2009 8  7 0 0 0
>     e2 = fromGregorian 2009 8 19 0 0 0
>     dateRanges = [(s1, e1), (s2, e2)]

Test XML:

> xml1 :: String
> xml1 = "<nrao:user domestic='true' id='119'><nrao:reservation id='2367'><nrao:startDate>2009-08-08</nrao:startDate><nrao:endDate>2009-08-09</nrao:endDate></nrao:reservation></nrao:user>"

> xml2 = "<nrao:user domestic='true' id='2265'><nrao:reservation id='2487'><nrao:startDate>2009-07-10</nrao:startDate><nrao:endDate>2009-07-22</nrao:endDate></nrao:reservation><nrao:reservation id='2488'><nrao:startDate>2009-08-07</nrao:startDate><nrao:endDate>2009-08-19</nrao:endDate></nrao:reservation></nrao:user>"
