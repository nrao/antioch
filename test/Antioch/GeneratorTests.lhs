> module Antioch.GeneratorTests where

> import Antioch.Generators
> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities (printList)
> {-
> import Antioch.Score
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Control.Monad.Trans  (lift, liftIO)
> -}
> import Test.HUnit
> import System.Random   
> import Test.QuickCheck    (generate)
> import Data.List
> import System.CPUTime

> tests = TestList [test_genMaintenanceProject
>                 , test_genMaintenancePeriodsByYear
>                 , test_genMaintenancePeriods
>                 , test_genSimTime
>                 , test_genFixedSchedule
>                 , test_genWindowedSchedule
>                 , test_genWeeklyMaintPeriods]

> test_genSimTime = TestCase $ do
>     let g = mkStdGen 1
>     let projs = generate 0 g $ genSimTime simHrs start days
>     --print projs
>     let totalMins = sum $ map sAllottedT $ concatMap sessions projs
>     assertEqual "test_genSimYear_1" True (1 < length projs)
>     --print $ simHrs * 60
>     --print totalMins
>     --assertEqual "test_genSimYear_12" True ((simHrs*60) < totalMins)
>     assertEqual "test_genSimYear_2" "Maintenance" (pName . last $ projs)
>     let maintMins = sum $ map sAllottedT $ sessions $ last projs
>     let nonMaintHrs = totalMins - maintMins
>     --print nonMaintMins
>     --assertEqual "test_genSimYear_12" True ((simHrs*60) < nonMaintMins)
>     --assertEqual "test_genSimYear_3" True ((nonMaintHrs < shi) && (slow < nonMaintHrs))
>     let projs = generate 0 g $ genSimTime 0 start days 
>     assertEqual "test_genSimYear_4" 1 (length projs)
>     assertEqual "test_genSimYear_5" "Maintenance" (pName . head $ projs)
>     --print "small!:"
>     --let projs2 = generate 0 g $ genSimTime 150 start 10
>     --let ps = sort $ concatMap periods $ concatMap sessions projs2
>     --printList ps
>     --print $ length ps
>     --print $ sum $ map duration ps 
>   where
>     start = fromGregorian 2006 1 1 0 0 0
>     days  = 365
>     simHrs = 2000
>     tolerance = 200
>     shi  = fromIntegral $ (simHrs + tolerance)
>     slow = fromIntegral $ (simHrs - tolerance)



> test_genMaintenanceProject = TestCase $ do
>     let g = mkStdGen 1
>     let dt = fromGregorian 2006 1 1 0 0 0
>     let proj = generate 0 g $ genMaintenanceProj dt 365
>     assertEqual "test_genMaintProj_1" "06A" (semester proj)
>     assertEqual "test_genMaintProj_1" "Maintenance" (pName proj)
>     -- test that we tied the knots
>     let ss = sessions proj
>     assertEqual "test_genMaintProj_1" 1 (length ss)
>     let s = head ss
>     assertEqual "test_genMaintProj_1" "Maintenance" (sName s)
>     assertEqual "test_genMaintProj_1" Fixed (sType s)
>     assertEqual "test_genMaintProj_1" proj (project s)
>     let ps = periods s
>     let totalTime = sum $ map duration ps
>     assertEqual "test_genMaintProj_1" totalTime (pAllottedT proj)
>     assertEqual "test_genMaintProj_1" totalTime (pAllottedS proj)
>     assertEqual "test_genMaintProj_1" totalTime (sAllottedT s)
>     assertEqual "test_genMaintProj_1" totalTime (sAllottedS s)
>     assertEqual "test_genMaintProj_1" s (session $ head ps)
>     assertEqual "test_genMaintProj_1" False (internalConflicts ps)
>     assertEqual "test_genMaintProj_1" 51840 totalTime
>     -- now see if changing the dates makes a difference
>     let proj = generate 0 g $ genMaintenanceProj dt 30
>     let ps = periods . head . sessions $ proj
>     let totalTime = sum $ map duration ps
>     assertEqual "test_genMaintProj_1" False (internalConflicts ps)
>     assertEqual "test_genMaintProj_1" 2400 totalTime

> test_genMaintenancePeriodsByYear = TestCase $ do
>     let g = mkStdGen 1
>     let ps = generate 0 g $ genMaintenancePeriodsByYear defaultSession 2006
>     let totalMins = sum $ map duration ps
>     assertEqual "test_genMaintPsByYear" 51840 totalMins 
>     assertEqual "test_genMaintPsByYear" (fromGregorian 2006 1 1 12 0 0) (startTime . head $ ps) 
>     assertEqual "test_genMaintPsByYear" (fromGregorian 2006 12 29 12 0 0) (startTime . last $ ps) 

> test_genMaintenancePeriods = TestCase $ do
>     let g = mkStdGen 1
>     -- first do exactly one year
>     let dt1 = fromGregorian 2006 1 1 0 0 0
>     let ps = generate 0 g $ genMaintenancePeriods dt1 365 defaultSession
>     let totalMins = sum $ map duration ps
>     assertEqual "test_genMaintPs_1" 51840 totalMins 
>     assertEqual "test_genMaintPs_2" (fromGregorian 2006 1 1 12 0 0) (startTime . head $ ps) 
>     assertEqual "test_genMaintPs_3" (fromGregorian 2006 12 29 12 0 0) (startTime . last $ ps) 
>     -- now do just a few months
>     let dt1 = fromGregorian 2006 2 1 0 0 0
>     let ps = generate 0 g $ genMaintenancePeriods dt1 60 defaultSession
>     let totalMins = sum $ map duration ps
>     assertEqual "test_genMaintPsBy_1" 3840 totalMins 
>     assertEqual "test_genMaintPsBy_2" (fromGregorian 2006 2 5 12 0 0) (startTime . head $ ps) 
>     assertEqual "test_genMaintPsBy_3" (fromGregorian 2006 3 26 12 0 0) (startTime . last $ ps) 
>     -- now span mutliple years
>     let dt1 = fromGregorian 2006 12 1 0 0 0
>     let ps = generate 0 g $ genMaintenancePeriods dt1 60 defaultSession
>     let totalMins = sum $ map duration ps
>     assertEqual "test_genMaintPs_4" 4800 totalMins 
>     assertEqual "test_genMaintPs_5" (fromGregorian 2006 12 1 12 0 0) (startTime . head $ ps) 
>     assertEqual "test_genMaintPs_6" (fromGregorian 2007 1 29 12 0 0) (startTime . last $ ps) 

> test_genWeeklyMaintPeriods = TestCase $ do
>     let g = mkStdGen 1
>     let ps = generate 0 g $ genWeeklyMaintPeriods start end defaultSession
>     assertEqual "test_genWeeklyMaintPeriods" True (all (<= end) $ map startTime ps) 
>     assertEqual "test_genWeeklyMaintPeriods" True (all (>= start) $ map startTime ps) 
>     assertEqual "test_genWeeklyMaintPeriods" 22 (length ps) 
>   where
>     start = fromGregorian 2006 1 1 0 0 0
>     end   = fromGregorian 2006 6 1 0 0 0

> test_genFixedSchedule = TestCase $ do
>     start <- getCurrentTime
>     --print ("start", toSqlString start)
>     let g = mkStdGen 1
>     let ps = generate 0 g $ genFixedSchedule dt days schd hrs
>     --print ps
>     end <- getCurrentTime
>     --print ("end", toSqlString end)
>     --print ("exec time: ", end - start)
>     let total = (sum $ map duration ps) `div` 60
>     let final = sort $ schd ++ ps
>     let dts = map startTime ps
>     assertEqual "test_createFS_1" True (hrs < total)
>     assertEqual "test_createFS_2" False (internalConflicts final) 
>     assertEqual "test_createFS_3" True ((dt<(minimum dts)) && ((maximum dts)<dt2)) 
>     -- change the date range and see if it still works
>     let ps = generate 0 g $ genFixedSchedule dt3 60 schd hrs
>     let total = (sum $ map duration ps) `div` 60
>     let final = sort $ schd ++ ps
>     let dts = map startTime ps
>     assertEqual "test_createFS_4" True (hrs < total)
>     assertEqual "test_createFS_5" False (internalConflicts final) 
>     assertEqual "test_createFS_6" True ((dt3<(minimum dts)) && ((maximum dts)<dt4)) 
>   where
>     dt = fromGregorian 2006 1 1 0 0 0
>     dt2 = fromGregorian 2007 1 1 0 0 0
>     dt3 = fromGregorian 2006 2 1 0 0 0
>     dt4 = fromGregorian 2006 4 2 0 0 0
>     days = 365
>     -- 5000 : > 5 mins
>     -- 2000 : ~225 secs
>     -- 1000 : ~30 secs
>     -- 100 : < 1 sec
>     hrs = 100
>     -- give them something to work around
>     schd = createSummerMaintenance 2006 defaultSession
> 

> test_genWindowedSchedule = TestCase $ do
>     start <- getCurrentTime
>     --print ("start", toSqlString start)
>     let g = mkStdGen 1
>     let wp = generate 0 g $ genWindowedSchedule dt days schd hrs
>     --print wp
>     end <- getCurrentTime
>     --print ("end", toSqlString end)
>     --print ("exec time: ", end - start)
>     let ps = concat wp
>     let total = (sum $ map duration ps) `div` 60
>     let final = sort $ schd ++ ps
>     let dts = map startTime ps
>     assertEqual "test_createWS_1" True (hrs < total)
>     assertEqual "test_createWS_2" False (internalConflicts final) 
>     assertEqual "test_createWS_3" True ((dt<(minimum dts)) && ((maximum dts)<dt2)) 
>     -- change the date range and see if it still works
>     let ps = concat $ generate 0 g $ genWindowedSchedule dt3 60 schd hrs
>     let total = (sum $ map duration ps) `div` 60
>     let final = sort $ schd ++ ps
>     let dts = map startTime ps
>     assertEqual "test_createWS_4" True (hrs < total)
>     assertEqual "test_createWS_5" False (internalConflicts final) 
>     assertEqual "test_createWS_6" True ((dt3<(minimum dts)) && ((maximum dts)<dt4)) 
>     -- try sticking a lot of hours in a small time range
>     --let ps = concat $ generate 0 g $ genWindowedSchedule dt3 10 [] 60
>     --let total = (sum $ map duration ps) `div` 60
>   where
>     dt = fromGregorian 2006 1 1 0 0 0
>     dt2 = fromGregorian 2007 1 1 0 0 0
>     dt3 = fromGregorian 2006 2 1 0 0 0
>     dt4 = fromGregorian 2006 4 2 0 0 0
>     days = 365
>     -- 2000 : > 5 mins!!! 
>     -- 1000 : ~24 secs
>     -- 100 : < 1 sec
>     hrs = 100
>     schd = createSummerMaintenance 2006 defaultSession

