> module Antioch.DSSDataTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather (getWeather)
> import Antioch.Score
> import Antioch.DSSData
> import Antioch.Utilities
> import Antioch.Generators (internalConflicts, validRA, validDec)
> import Maybe
> import List (nub, sort)
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)
> import Database.HDBC

TBF: all these tests are based off a DB that could change; we need to set up a framework so that the unit tests don't break so easy.

TBF: tests hang when all are run togethor - I don't think I'm handling the 
connection to the DB correctly.

> tests = TestList [
>       --test_fetchPeriods
>       test_getProjects
>     , test_getProjectData
>     , test_getProjectsProperties
>     --, test_putPeriods
>     , test_scoreDSSData
>     ]

> test_getProjectData = TestCase $ do
>     cnn <- connect
>     d <- fetchProjectData cnn
>     assertEqual "test_getProjectData1" 104 (length d)  
>     assertEqual "test_getProjectData2" "BB240" (pName . head $ d)  
>     assertEqual "test_getProjectData3" 48480 (timeTotal . head $ d)  

> test_getProjects = TestCase $ do
>     ps <- getProjects 
>     let ss = sessions . head $ ps
>     let allPeriods = sort $ concatMap periods $ concatMap sessions ps
>     assertEqual "test_getProjects1" 104 (length ps)  
>     assertEqual "test_getProjects5" 2 (pId . head $ ps)  
>     assertEqual "test_getProjects2" "BB240" (pName . head $ ps)  
>     assertEqual "test_getProjects3" 48480 (timeTotal . head $ ps)  
>     assertEqual "test_getProjects4" 16 (length . sessions . head $ ps)  
>     assertEqual "test_getProjects8" Windowed (sType . head $ ss)
>     assertEqual "test_getProjects6" 2 (pId . project . head $ ss)    
>     assertEqual "test_getProjects7" 1 (length . nub $ map (pId . project) $ ss) 
>     assertEqual "test_getProjects9" [] (dropWhile (/=W) (map band ss))    
>     assertEqual "test_getProjects10" 9 (length allPeriods)    
>     assertEqual "test_getProjects11" (fromGregorian 2009 6 9 17 30 0) (startTime . head $ allPeriods)    
>     assertEqual "test_getProjects11" 60 (duration . head $ allPeriods)    
>     assertEqual "test_getProjects12" 1 (length . nub $ map (sType . session) allPeriods) 
>     assertEqual "test_getProjects12" Fixed (sType . session . head $ allPeriods) 
>     assertEqual "test_getProjects13" True ((length $ concatMap pBlackouts ps) > 0) 
>     assertEqual "test_getProject99" [[Rcvr8_10]] (receivers . head . tail $ ss)

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

Perhaps these should be Quick Check properities, but the input is not 
generated: it's the input we want to test, really.

> test_getProjectsProperties = TestCase $ do
>   ps <- getProjects
>   let ss = concatMap sessions ps
>   let allPeriods = sort $ concatMap periods ss 
>   assertEqual "test_getProjects_properties_1" True (all validProject ps)  
>   assertEqual "test_getProjects_properties_2" True (all validSession ss)  
>   assertEqual "test_getProjects_properties_3" True (validPeriods allPeriods)  
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

TBF: this fails because there is something going on with our datetimes in
the DB regarding the time zone - our times are off by ~5 hrs.  So a datetime
written to the DB doesn't come back as your wrote it ...
Need to straighten all this shit out.

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
