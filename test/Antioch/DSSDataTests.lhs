> module Antioch.DSSDataTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather (getWeatherTest)
> import Antioch.Score
> import Antioch.DSSData
> import Antioch.Utilities
> import Antioch.ReceiverTemperatures (getRT)
> import Antioch.Generators (internalConflicts, internalConflicts', validRA, validDec)
> import Maybe
> import List (nub, sort)
> import Data.List (find)
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)
> import Database.HDBC
> --import Database.HDBC.PostgreSQL            (Connection, connectPostgreSQL) -- dbug

The DB used for these unit tests is created and populated via the
instructions in admin/genDssTestDatagase.py.

> tests = TestList [
>       test_fetchPeriods
>     , test_getPeriods
>     , test_getProjects
>     -- , test_numPeriods
>     , test_getProjectData
>     -- , test_getProjectsProperties
>     -- , test_putPeriods
>     , test_makeSession
>     , test_scoreDSSData
>     , test_session2
>     , test_session_scores
>     , test_totaltime
>     , test_toDateRangesFromInfo_1
>     , test_toDateRangesFromInfo_2
>     , test_toDateRangesFromInfo_3
>     ]

> test_getProjectData = TestCase $ do
>     cnn <- connect
>     d <- fetchProjectData cnn
>     assertEqual "test_getProjectData1" 1 (length d)  
>     assertEqual "test_getProjectData2" "GBT09A-001" (pName . head $ d)  
>     assertEqual "test_getProjectData3" False (thesis . head $ d)  
>     disconnect cnn

> test_getProjects = TestCase $ do
>     ps <- getProjects 
>     let ss = sessions . head $ ps
>     let allPeriods = sort $ concatMap periods $ concatMap sessions ps
>     assertEqual "test_getProjects1" 1 (length ps)  
>     assertEqual "test_getProjects5" 1 (pId . head $ ps)  
>     assertEqual "test_getProjects2" "GBT09A-001" (pName . head $ ps)  
>     assertEqual "test_getProjects3" 720 (pAllottedT . head $ ps)  
>     assertEqual "test_getProjects4" 1 (length . sessions . head $ ps)  
>     assertEqual "test_getProjects8" Open (sType . head $ ss)
>     assertEqual "test_getProjects6" 1 (pId . project . head $ ss)    
>     assertEqual "test_getProjects7" 1 (length . nub $ map (pId . project) $ ss) 
>     assertEqual "test_getProjects9" [] (dropWhile (/=W) (map band ss))    
>     assertEqual "test_getProjects10" 1 (length allPeriods)    
>     assertEqual "test_getProject99" [[Rcvr8_10]] (receivers . head $ ss)

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

Makes sure that a project with hrs for more then one grade is imported
once and has a total time that is the sum of the grade hrs.

> test_totaltime = TestCase $ do
>   projs <- getProjects
>   let ps = filter (\p -> (pName p) == "GBT09A-001") projs
>   assertEqual "test_sAllottedT_1" 1 (length ps)
>   assertEqual "test_sAllottedT_2" (12*60) (pAllottedT . head $ ps)

Makes sure that there is nothing so wrong w/ the import of data that a given
session scores zero through out a 24 hr period.

> test_scoreDSSData = TestCase $ do
>     w <- getWeatherTest . Just $ starttime 
>     rt <- getRT
>     ps <- getProjects
>     let ss = concatMap sessions ps
>     let sess' = fromJust . find (\s -> (sType s) == Open) $ ss
>     -- give it an observer
>     let p' = project sess'
>     let p = p' { observers = [defaultObserver] }
>     let sess = sess' { project = p }
>     let score' w dt = runScoring w [] rt $ do
>         fs <- genScore starttime ss 
>         s <- fs dt sess
>         return $ eval s
>     scores <- mapM (score' w) times
>     let nonZeros = filter (/= 0.0) scores
>     assertEqual "test_scoreDSSData" True ((length nonZeros) /= 0)
>   where
>     starttime = fromGregorian 2006 11 8 12 0 0
>     times = [(15*q) `addMinutes'` starttime | q <- [0..96]]

How a session scores can also reveal errors in how it was imported
from the database.

> test_session_scores = TestCase $ do
>     w <- getWeatherTest $ Just start
>     rt <- getRT
>     ps <- getProjects
>     let ss = concatMap sessions ps
>     -- get the session and give it an observer
>     let s' = head $ filter (\s -> (sName s) == name) ss
>     let p' = project s'
>     let p = p' { observers = [defaultObserver] }
>     let s = s' { project = p }
>     let score' w dt = runScoring w [] rt $ do
>         fs <- genScore start ss 
>         sf <- fs dt s
>         return $ eval sf
>     scores <- mapM (score' w) times
>     assertEqual "test_session_scores" expScores scores
>     where
>       name = "GBT09A-001-02"
>       --start = fromGregorian 2006 6 6 3 0 0 -- 11 PM ET
>       start = fromGregorian 2006 6 6 6 30 0
>       times = [(15*q) `addMinutes'` start | q <- [0..16]]
>       expScores = [0.0,1.0911006,1.0982922,1.1073105,1.1103191,1.1170144,1.120861,1.1242979,1.1320864,1.1346878,1.137038,1.139167,1.1408842,1.141817,1.133193,1.1340336,1.1333339]

Test a specific session's attributes:

> test_session2 = TestCase $ do
>   ps <- getProjects 
>   let ss = concatMap sessions ps
>   let s = head $ filter (\s -> (sName s == "GBT09A-001-02")) ss
>   assertEqual "test_session2_1" 3.0 (grade s)
>   assertEqual "test_session2_2" Open (sType s)
>   assertEqual "test_session2_3" 1 (sId s)
>   assertEqual "test_session2_4" "GBT09A-001-02" (sName s)
>   assertEqual "test_session2_5" "GBT09A-001" (pName . project $ s)
>   assertEqual "test_session2_6" "09A" (semester . project $ s)
>   assertEqual "test_session2_7" 210 (sAllottedT s)
>   assertEqual "test_session2_8" 180 (minDuration s)
>   assertEqual "test_session2_9" 210 (maxDuration s)
>   assertEqual "test_session2_10" 0 (timeBetween s)
>   assertEqual "test_session2_11" 9.3 (frequency s)
>   assertEqual "test_session2_12" 5.861688  (ra s)
>   assertEqual "test_session2_13" (-0.11362094) (dec s)
>   assertEqual "test_session2_14" [[Rcvr8_10]] (receivers s)
>   assertEqual "test_session2_15" X (band s)
>   assertEqual "test_session2_16" False (lowRFI s)
>   assertEqual "test_session2_17" 1 (length . lstExclude $ s)

Perhaps these should be Quick Check properities, but the input is not 
generated: it's the input we want to test, really.

> test_getProjectsProperties = TestCase $ do
>   ps <- getProjects
>   let ss = concatMap sessions ps
>   let allPeriods = sort $ concatMap periods ss 
>   assertEqual "test_getProjects_properties_1" True (all validProject ps)  
>   assertEqual "test_getProjects_properties_2" True (all validSession ss)  
>   assertEqual "test_getProjects_properties_3" True (validPeriods allPeriods)  
>   assertEqual "test_getProjects_properties_4" True (2 < length (filter (\s -> grade s == 3.0) ss) )
>   assertEqual "test_getProjects_properties_5" 46 (length $ filter lowRFI ss)
>   let lsts = filter (\s -> (length . lstExclude $ s) > 0) ss
>   assertEqual "test_getProjects_properties_6" 4 (length lsts)
>   assertEqual "test_getProjects_properties_7" [(15.0,21.0)] (lstExclude . head $ lsts)
>   assertEqual "test_getProjects_properties_8" [(14.0,9.0)] (lstExclude . last $ lsts)
>   -- TBF, BUG: Session (17) BB261-01 has no target, 
>   -- so is not getting imported.
>   assertEqual "test_getProjects_properties_9" 255 (length ss)  
>   assertEqual " " True True
>     where
>       validProject proj = "0" == (take 1 $ semester proj)
>       validSession s = (maxDuration s) >= (minDuration s)
>                    -- TBF!! &&  (sAllottedT s)     >= (minDuration s)
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
>                          , pScore = 0.0
>                          , pForecast = dt }

Kluge, data base has to be prepped manually for test to work, see
example in comments.

> test_populateSession = TestCase $ do
>   -- using session 194 GBT09B-028-02
>   -- insert into periods_accounting (scheduled, not_billable, other_session_weather, other_session_rfi, other_session_other, lost_time_weather, lost_time_rfi, lost_time_other, short_notice) values (4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
>   -- 86 = select MAX(id) from periods_accounting;
>   -- INSERT INTO periods (session_id, start, duration, score, forecast, backup, accounting_id, state_id, moc_ack) VALUES (194, "2009-06-15 12:00:00", 4.0, 3.1, "2009-06-13 00:08:00", false, 86, 1, false);
>   -- 1760 = select MAX(id) from periods;
>   -- insert into windows (session_id, default_period_id, start_date, duration) values (194, 1760, '2009-06-10 00:00:00', 7);
>   -- 1 = select MAX(elect MAX(id) from windows;
>   cnn <- connect
>   s <- getSession sId cnn
>   ios <- populateSession cnn s
>   assertEqual "test_populateSession 1" s ios
>   assertEqual "test_populateSession 2" ios (session . head . periods $ ios)
>   p <- fetchPeriod pId cnn
>   assertEqual "test_populateSession 3" (fromGregorian 2009 6 15 12 0 0) (startTime p)
>   assertEqual "test_populateSession 4" (4*60) (duration . head . periods $ ios)
>   assertEqual "test_populateSession 5" (4*60) (pDuration . head . periods $ ios)
>   assertEqual "test_populateSession 7" Nothing (wPeriod . head . windows $ ios)
>   assertEqual "test_populateSession 8" (Just . head . periods $ ios) (wPeriod . head . windows $ ios)
>   assertEqual "test_populateSession 9" (fromGregorian 2009 6 10 0 0 0) (wStart . head . windows $ ios)
>     where
>       sId =  194
>       pId = 1760

> mkSqlLst  :: Int -> DateTime -> Int -> Int -> Int -> Int -> String -> [SqlValue]
> mkSqlLst id strt dur def per pid st =
>     [toSql id
>    , toSql . toSqlString $ strt
>    , toSql dur
>    , if def == 0
>      then SqlNull
>      else toSql def
>    , if per == 0
>      then SqlNull
>      else toSql def
>    , toSql pid
>    , toSql st
>     ]

> test_makeSession = TestCase $ do
>   let s = makeSession s' [w'] [p']
>   assertEqual "test_makeSession 1" s' s
>   assertEqual "test_makeSession 2" s (session . head . periods $ s)
>   assertEqual "test_makeSession 3" p' (head . periods $ s)
>   assertEqual "test_makeSession 4" (Just p') (wPeriod . head . windows $ s)
>   assertEqual "test_makeSession 5" (head . periods $ s) (fromJust . wPeriod . head . windows $ s)
>     where
>       s' = defaultSession { sAllottedT = (8*60) }
>       p' = defaultPeriod { duration = (4*60) }
>       w' = defaultWindow { wDuration = 7, wPeriodId = peId p' }

> test_getPeriods = TestCase $ do
>   cnn <- connect
>   s <- getSession 1 cnn
>   ps' <- getPeriods cnn s
>   -- note fetchPeriods doesn't set the period's session
>   let ps = [defaultPeriod { session = defaultSession 
>                           , startTime = dt
>                           , duration = 240}]
>   disconnect cnn
>   assertEqual "test_getPeriods" ps ps'
>     where
>       dt = fromGregorian 2006 1 1 0 0 0

> test_fetchPeriods = TestCase $ do
>   cnn <- connect
>   s <- getSession 1 cnn
>   ps' <- fetchPeriods cnn s
>   -- note fetchPeriods doesn't set the period's session
>   let ps = [defaultPeriod { session = defaultSession
>                           , startTime = dt
>                           , duration = 240}]
>   disconnect cnn
>   assertEqual "test_fetchPeriods" ps ps' 
>     where
>       dt = fromGregorian 2006 1 1 0 0 0

> fromFloat2Sql :: Float ->  SqlValue
> fromFloat2Sql = toSql

> test_toDateRangesFromInfo_1 = TestCase $ do
>   let dtrs = toDateRangesFromInfo start end repeat until 
>   assertEqual "test_toDateRangesFromInfo_1" [(start, end)] dtrs
>     where
>       start = fromGregorian 2009 1 1 0 0 0
>       end   = fromGregorian 2009 1 1 4 0 0
>       until = fromGregorian 2009 1 1 4 0 0
>       repeat = "Ounce" 
>     

> test_toDateRangesFromInfo_2 = TestCase $ do
>   let dtrs = toDateRangesFromInfo start end repeat until 
>   assertEqual "test_toDateRangesFromInfo_2" exp dtrs
>     where
>       start = fromGregorian 2009 1 1 0 0 0
>       end   = fromGregorian 2009 1 1 4 0 0
>       until = fromGregorian 2009 1 23 0 0 0
>       repeat = "Weekly" 
>       exp = [(start, end)
>            , (fromGregorian 2009 1 8 0 0 0
>            ,  fromGregorian 2009 1 8 4 0 0)
>            , (fromGregorian 2009 1 15 0 0 0
>            ,  fromGregorian 2009 1 15 4 0 0)
>            , (fromGregorian 2009 1 22 0 0 0
>            ,  fromGregorian 2009 1 22 4 0 0)
>             ]

> test_toDateRangesFromInfo_3 = TestCase $ do
>   let dtrs = toDateRangesFromInfo start end repeat until 
>   assertEqual "test_toDateRangesFromInfo_3" exp dtrs
>     where
>       start = fromGregorian 2009 11  2 0 0 0
>       end   = fromGregorian 2009 11  2 4 0 0
>       until = fromGregorian 2010  2 23 0 0 0
>       repeat = "Monthly" 
>       exp = [(start, end)
>            , (fromGregorian 2009 12 2 0 0 0
>            ,  fromGregorian 2009 12 2 4 0 0)
>            , (fromGregorian 2010  1 2 0 0 0
>            ,  fromGregorian 2010  1 2 4 0 0)
>            , (fromGregorian 2010  2 2 0 0 0
>            ,  fromGregorian 2010  2 2 4 0 0)
>             ]

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
>     run cnn ("TRUNCATE TABLE " ++ tableName ++ " CASCADE") []
>     commit cnn
>     disconnect cnn

