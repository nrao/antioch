> module Antioch.Reservations where

> import Antioch.DateTime
> import Antioch.Types
> import Network.Browser
> import Network.HTTP
> import Network.HTTP.Wget
> import Network.URI
> import Text.XML.HaXml
> import Data.Maybe
> import System.IO

This module is responsible for retreiving reservation information (specified
by users and/or date ranges) in a format suitable for the Observer data structure.

TBF: key for specifying users should change in near future!

There are currently three types of BOS web service avaialable for res. info:
   1. Input : (bos id #) -> Output : (res id #,  [sdate - edate]) (TBF: will use CAS username in future?)
   2. Input : (sdate - edate) -> Output : (res id #, bos id #, sdate - edate)
   3. Input : ([bos id #], sdate - edate) -> Output : (res id #, bos id #, sdate - edate)

This method uses service #1 described above.
TBF: this won't work until we can stop BOS from redirecting us.

> getObserverResDates :: Int -> IO [DateRange]
> getObserverResDates bosId = do
>     let url = getServiceUrl1 bosId
>     datesXML <- downloadHTTP url 
>     -- TBF: check for errors!
>     -- TBF: parse this XML to get the dates.
>     return $ parseResDatesXML datesXML "error"

This method constructs the URL for service #1 described above.

> getServiceUrl1 :: Int -> String
> getServiceUrl1 bosId = urlRoot ++ (show bosId)
>   where
>     urlRoot = "https://bos.nrao.edu/resReports/reservationsByPerson/"

This method parses the results from the service #1 described above.

> parseResDatesXML:: String -> String -> [DateRange]
> parseResDatesXML content name = map toDateRange $ map toDateContents $ reservation doc
>   where
>     parseResult = xmlParse name content
>     doc = getContent parseResult

This returns a Content for each reservation the user has.

> reservations :: CFilter
> reservations doc = tag "nrao:user" /> tag "nrao:reservation" $ doc

For each reservation, get the Content containing the start & end dates.

> toDateContents :: Content -> ([Content], [Content])
> toDateContents res = ((getDate "nrao:startDate" res), (getDate "nrao:endDate" res))

> getDate :: String -> Content -> [Content]
> getDate tagName res = keep /> tag tagName /> txt $ res

Convert the Content representing the date range, first to strings, 
then to DateTime's.

> toDateRange :: ([Content], [Content]) -> DateRange
> toDateRange c = (toDt . fst $ c, toDt . snd $ c)
>   where
>     toDt c' = strToDateTime . contentToString $ c'

Assumes format of YYYY-MM-DD

> strToDateTime :: String -> DateTime
> strToDateTime dtStr = fromGregorian year month day 0 0 0
>   where
>     year  = read (take 4 dtStr) :: Int
>     month = read (drop 5 $ take 7  dtStr) :: Int
>     day   = read (drop 8 $ take 10 dtStr) :: Int

> contentToStringDefault :: String -> [Content] -> String
> contentToStringDefault msg [] = msg
> contentToStringDefault _ x = contentToString x

> contentToString :: [Content] -> String
> contentToString cs = concatMap procContent cs
>   where
>     procContent x = verbatim $ keep /> txt $ CElem (unesc (fakeElem x))

> fakeElem :: Content -> Element
> fakeElem x = Elem "fake" [] [x]

> unesc :: Element -> Element
> unesc = xmlUnEscape stdXmlEscaper

> getContent :: Document -> Content
> getContent (Document _ _ e _) = CElem e

> reservation :: CFilter
> reservation = tag "nrao:user" /> tag "nrao:reservation"

> downloadHTTP url = do
>      rsp <- simpleHTTP (getRequest url)
>              -- fetch document and return it (as a 'String'.)
>      getResponseBody rsp

For Reference:

This function seems to go into an infinite loop when it encounters
a redirect, so don't use it.

> {- | Download a URL.  (Left errorMessage) if an error,
> (Right doc) if success. -}
> downloadURL :: String -> IO (Either String String)
> downloadURL url =
>     do resp <- simpleHTTP request
>        case resp of
>          Left x -> return $ Left ("Error connecting: " ++ show x)
>          Right r -> 
>              case rspCode r of
>                (2,_,_) -> return $ Right (rspBody r)
>                (3,_,_) -> -- A HTTP redirect
>                  case findHeader HdrLocation r of
>                    Nothing -> return $ Left (show r)
>                    Just url -> downloadURL url
>                _ -> return $ Left (show r)
>     where request = Request {rqURI = uri,
>                              rqMethod = GET,
>                              rqHeaders = [],
>                              rqBody = ""}
>           uri = fromJust $ parseURI url


