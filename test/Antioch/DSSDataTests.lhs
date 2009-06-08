> module Antioch.DSSDataTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather (getWeather)
> import Antioch.Score
> import Antioch.DSSData
> import Antioch.Utilities
> import Antioch.Generators (internalConflicts, internalConflicts', validRA, validDec)
> import Maybe
> import List (nub, sort)
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)
> import Database.HDBC

The DB used for these unit tests is the DB used for the simulation of the
first two weeks of 09B, *w/ out* the resultant periods.

TBF: tests hang when all are run togethor - I don't think I'm handling the 
connection to the DB correctly.

> tests = TestList [
>       --test_fetchPeriods
>       test_getProjects
>     , test_getProjectData
>     , test_getProjectsProperties
>     --, test_putPeriods
>     , test_scoreDSSData
>     , test_session2
>     , test_session_scores
>     ]

> test_getProjectData = TestCase $ do
>     cnn <- connect
>     d <- fetchProjectData cnn
>     assertEqual "test_getProjectData1" 106 (length d)  
>     assertEqual "test_getProjectData2" "BB240" (pName . head $ d)  
>     assertEqual "test_getProjectData3" 48480 (timeTotal . head $ d)  

> test_getProjects = TestCase $ do
>     ps <- getProjects 
>     let ss = sessions . head $ ps
>     let allPeriods = sort $ concatMap periods $ concatMap sessions ps
>     assertEqual "test_getProjects1" 106 (length ps)  
>     assertEqual "test_getProjects5" 2 (pId . head $ ps)  
>     assertEqual "test_getProjects2" "BB240" (pName . head $ ps)  
>     assertEqual "test_getProjects3" 48480 (timeTotal . head $ ps)  
>     assertEqual "test_getProjects4" 16 (length . sessions . head $ ps)  
>     assertEqual "test_getProjects8" Windowed (sType . head $ ss)
>     assertEqual "test_getProjects6" 2 (pId . project . head $ ss)    
>     assertEqual "test_getProjects7" 1 (length . nub $ map (pId . project) $ ss) 
>     assertEqual "test_getProjects9" [] (dropWhile (/=W) (map band ss))    
>     assertEqual "test_getProjects10" 137 (length allPeriods)    
>     assertEqual "test_getProjects11" (fromGregorian 2009 6 1 11 0 0) (startTime . head $ allPeriods)    
>     assertEqual "test_getProjects12" 630 (duration . head $ allPeriods)    
>     assertEqual "test_getProjects13" 3 (length . nub $ map (sType . session) allPeriods) 
>     assertEqual "test_getProjects14" Fixed (sType . session . head $ allPeriods) 
>     assertEqual "test_getProjects15" True ((length $ concatMap pBlackouts ps) > 0) 
>     assertEqual "test_getProject99" [[Rcvr8_10]] (receivers . head . tail $ ss)

TBF: cant' run this one automatically because it doesn't clean up yet, 
so, clean up by hand for now.

> test_numPeriods = TestCase $ do
>   projs <- getProjects
>   let ps = concatMap periods $ concatMap sessions projs
>   let numPs = length ps
>   assertEqual "test_numPeriods_1" 137 numPs
>   --  now create a new period identical to an existing period
>   -- and make sure it doesn't get translated to a period
>   assertEqual "test_numPeriods_2" [identicalToOpt] (filter (==identicalToOpt) ps)
>   -- TBF: Oops!  We're supposed to put in a new opportunity, not a window!
>   --putPeriods [identicalToOpt]
>   projs <- getProjects
>   let ps = concatMap periods $ concatMap sessions projs
>   assertEqual "test_numPeriods_3" numPs (length ps)
>   -- need to clean up!
>     where
>       identicalToOpt = defaultPeriod { session = defaultSession { sId = 48 }
>                             , startTime = fromGregorian 2009 7 15 4 0 0
>                             , duration = hrsToMinutes 3.75 
>                             , pForecast = fromGregorian 2009 7 15 4 0 0
>                                      }
>   

Makes sure that there is nothing so wrong w/ the import of data that a given
session scores zero through out a 24 hr period.

> test_scoreDSSData = TestCase $ do
>     w <- getWeather . Just $ starttime 
>     ps <- getProjects
>     let ss = concatMap sessions ps
>     let sess = head ss
>     let score' w dt = runScoring w [] $ do
>         fs <- genScore ss 
>         s <- fs dt sess
>         return $ eval s
>     scores <- mapM (score' w) times
>     let nonZeros = filter (/=0.0) scores
>     assertEqual "test_scoreDSSData" True ((length nonZeros) /= 0)
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     times = [(15*q) `addMinutes'` starttime | q <- [0..96]]

How a session scores can also reveal errors in how it was imported
from the database.

> test_session_scores = TestCase $ do
>     w <- getWeather $ Just start
>     ps <- getProjects
>     let ss = concatMap sessions ps
>     let s = head $ filter (\s -> (sName s) == name) ss
>     let score' w dt = runScoring w [] $ do
>         fs <- genScore ss 
>         sf <- fs dt s
>         return $ eval sf
>     scores <- mapM (score' w) times
>     assertEqual "test_session_scores" expScores scores
>     where
>       name = "GBT09B-010-02"
>       start = fromGregorian 2006 6 6 3 0 0 -- 11 PM ET
>       --start = fromGregorian 2009 6 5 12 0 0 -- 11 PM ET
>       times = [(15*q) `addMinutes'` start | q <- [0..16]]
>       expScores = [4.896624,5.0632706,5.120554,5.138654
>                   ,5.15567,5.15567,5.181927,5.181927
>                   ,5.181927,5.181927,5.201093,5.1857715
>                   ,5.169486,5.152158,5.1498766,5.1305795,5.087766]

Test a specific session's attributes:

> test_session2 = TestCase $ do
>   ps <- getProjects 
>   let ss = concatMap sessions ps
>   let s = head $ filter (\s -> (sName s == "GBT09A-081-02")) ss
>   assertEqual "test_session2_1" GradeB (grade s)
>   assertEqual "test_session2_2" Open (sType s)
>   assertEqual "test_session2_3" 124 (sId s)
>   assertEqual "test_session2_4" "GBT09A-081-02" (sName s)
>   assertEqual "test_session2_5" "GBT09A-081" (pName . project $ s)
>   assertEqual "test_session2_6" "09A" (semester . project $ s)
>   assertEqual "test_session2_7" 210 (totalTime s)
>   assertEqual "test_session2_8" 180 (minDuration s)
>   assertEqual "test_session2_9" 210 (maxDuration s)
>   assertEqual "test_session2_10" 0 (timeBetween s)
>   assertEqual "test_session2_11" 0.34 (frequency s)
>   assertEqual "test_session2_12" 5.861688  (ra s)
>   assertEqual "test_session2_13" (-0.11362094) (dec s)
>   assertEqual "test_session2_14" [[Rcvr_342]] (receivers s)
>   assertEqual "test_session2_15" L (band s)

Perhaps these should be Quick Check properities, but the input is not 
generated: it's the input we want to test, really.

> test_getProjectsProperties = TestCase $ do
>   ps <- getProjects
>   let ss = concatMap sessions ps
>   let allPeriods = sort $ concatMap periods ss 
>   assertEqual "test_getProjects_properties_1" True (all validProject ps)  
>   assertEqual "test_getProjects_properties_2" True (all validSession ss)  
>   assertEqual "test_getProjects_properties_3" True (validPeriods allPeriods)  
>   assertEqual "test_getProjects_properties_4" True (2 < length (filter (\s -> grade s == GradeB) ss) )
>     where
>       validProject proj = "0" == (take 1 $ semester proj)
>       validSession s = (maxDuration s) >= (minDuration s)
>                    -- TBF!! &&  (totalTime s)     >= (minDuration s)
>                     &&  (validRA s) && (validDec s)
>       validPeriods allPeriods = not . internalConflicts $ allPeriods
 
> test_putPeriods = TestCase $ do
>   r1 <- getNumRows "periods"
>   putPeriods [p1]
>   r2 <- getNumRows "periods"
>   cleanup "periods"
>   assertEqual "test_putPeriods" True (r2 == (r1 + 1)) 
>     where
>       dt = fromGregorian 2006 1 1 0 0 0
>       p1 = defaultPeriod { session = defaultSession { sId = 1 }
>                          , startTime = dt
>                          , pForecast = dt }

> test_fetchPeriods = TestCase $ do
>   putPeriods [p1]
>   cnn <- connect
>   ps' <- fetchPeriods cnn s
>   -- fetchPeriods doesn't set the period's session, so we'l do that
>   let ps = map (\p -> p { session = s }) ps'
>   print [p1]
>   print ps
>   disconnect cnn
>   cleanup "periods"
>   assertEqual "test_fetchPeriods" [p1] ps 
>     where
>       dt = fromGregorian 2006 1 1 0 0 0
>       s  = defaultSession { sId = 1 }
>       p1 = defaultPeriod { session = s
>                          , startTime = dt
>                          , pForecast = dt }




Test Utilities: 

> getNumRows :: String -> IO Int
> getNumRows tableName = do 
>     cnn <- connect
>     r <- quickQuery' cnn ("SELECT * FROM " ++ tableName) []
>     disconnect cnn
>     return $ length r

> cleanup :: String -> IO () 
> cleanup tableName = do
>     cnn <- connect
>     run cnn ("TRUNCATE TABLE " ++ tableName) []
>     commit cnn
>     disconnect cnn

