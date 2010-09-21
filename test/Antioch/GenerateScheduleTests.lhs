> module Antioch.GenerateScheduleTests where

> import Antioch.GenerateSchedule
> import Antioch.Generators
> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Statistics
> import Antioch.Utilities    (printList, periodInWindow, validWindow)
> {-
> import Antioch.Score
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.PProjects
> import Control.Monad.Trans  (lift, liftIO)
> -}
> import Test.HUnit
> import System.Random   
> import Test.QuickCheck      (generate)
> import Data.List
> import System.CPUTime

> tests = TestList [test_genMaintenanceProject
>                 , test_genMaintenancePeriodsByYear
>                 , test_genMaintenancePeriods
>                 , test_genSimTime
>                 , test_genSimTime2
>                 , test_genFixedSchedule
>                 , test_genWindowedSchedule
>                 , test_genWindows
>                 , test_validSimulatedWindows
>                 , test_genWeeklyMaintPeriods]

> test_genSimTime = TestCase $ do
>     let g = mkStdGen 1
>     let simMins = days*24*60
> 
>     -- a year with just open sessions 
>     let projs = generate 0 g $ genSimTime start days False (1.0, 0.0, 0.0) 0 --(0.6, 0.3, 0.1) 0.25
>     let sems = nub . sort $ map semester projs
>     assertEqual "test_genSimYear_0" ["05C", "06A", "06B", "06C"] sems
>     let totalMins = projTime projs 
>     assertEqual "test_genSimYear_1" True (1 < length projs)
>     assertEqual "test_genSimYear_1" True ((totalMins > simMins) && (totalMins < (simMins + (simMins `div` 2))))
>     assertEqual "test_genSimYear_1" True (all (==Open) (map sType $ concatMap sessions projs))
>
>     -- a year of just Maintenance!
>     let projs = generate 0 g $ genSimTime start days  True (0.0, 0.0, 0.0) 0
>     assertEqual "test_genSimYear_4" 1 (length projs)
>     assertEqual "test_genSimYear_5" "Maintenance" (pName . head $ projs)
>
>     -- a year with just open sessions and Maintenance
>     let projs = generate 0 g $ genSimTime start days True (1.0, 0.0, 0.0) 0 
>     --print totalMins
>     --assertEqual "test_genSimYear_12" True ((simHrs*60) < totalMins)
>     let totalMins = projTime projs 
>     assertEqual "test_genSimYear_2" "Maintenance" (pName . last $ projs)
>     let maintMins = sum $ map duration $ periods . last $ sessions . last $ projs
>     --print ("totalMins", totalMins)
>     --print ("maintMins", maintMins)
>     assertEqual "test_genSimYear_1" 51840 maintMins
>     assertEqual "test_genSimYear_1" True (1 < length projs)
>     assertEqual "test_genSimYear_1" True ((totalMins > simMins) && (totalMins < (simMins + (simMins `div` 2))))
>     --assertEqual "test_genSimYear_1" True (all (==Open) (map sType $ concatMap sessions $ init projs))
>
>     -- a year with everyting!
>     let projs = generate 0 g $ genSimTime start days True (0.90, 0.05, 0.05) 2000 
>     ---print "windows: "
>     ---printList $ map windows $ filter (\s-> (sType s) == Windowed) $ concatMap sessions projs
>     let totalMins = projTime projs 
>     assertEqual "test_genSimYear_12" True (simMins < totalMins)
>     let schedule = concatMap periods $ concatMap sessions projs
>     assertEqual "test_genMaintProj_1" False (internalConflicts schedule)
>     assertEqual "test_genSimYear_2" "Maintenance" (pName . last $ projs)
>     let maintMins = sum $ map duration $ periods . last $ sessions . last $ projs
>     assertEqual "test_genSimYear_1" True (1 < length projs)
>     assertEqual "test_genSimYear_1" True ((totalMins > simMins) && (totalMins < (simMins + (simMins `div` 2))))
>     assertEqual "test_genSimYear_1" True (any (==Fixed) (map sType $ concatMap sessions projs))
>     assertEqual "test_genSimYear_1" True (any (==Windowed) (map sType $ concatMap sessions projs))
>     -- all this is to make sure that the number of hours of fixed is okay
>     let fixedPs = filter (\p -> (sType . session $ p) == Fixed) $ concatMap periods $ concatMap sessions $ init projs -- don't include Maint. 
>     let fixedMins = sum $ map duration fixedPs
>     let fixedMins' = fromIntegral fixedMins
>     let nonMaintMins = simMins - maintMins
>     let expFixedMinsLower = (fromIntegral nonMaintMins) * (0.05::Float)
>     let expFixedMinsUpper = expFixedMinsLower * 1.25
>     --print ("simMins", simMins)
>     --print ("maintMins", maintMins)
>     --print ("fixedMins", fixedMins)
>     --print ("expFixedMins", expFixedMinsLower, expFixedMinsUpper)
>     assertEqual "test_genSimYear_10" True (expFixedMinsLower < fixedMins' && fixedMins' < expFixedMinsUpper) 
>     -- all this is to make sure that the number of hours of windowed is okay
>     let winPs = filter (\p -> (sType . session $ p) == Windowed) $ concatMap periods $ concatMap sessions $ projs 
>     let winSs = filter (\s -> (sType $ s) == Windowed) $ concatMap sessions $ projs 
>     let winMins = sum $ map duration winPs
>     let winMins' = fromIntegral winMins
>     --let nonMaintMins = totalMins - maintMins
>     let expMinsLower = (fromIntegral nonMaintMins) * (0.05::Float)
>     let expMinsUpper = expMinsLower * 1.25
>     --print ("simMins", simMins)
>     --print ("maintMins", maintMins)
>     --print ("winMins", winMins)
>     --print ("expMins", expMinsLower, expMinsUpper)
>     assertEqual "test_genSimYear_10" True (expMinsLower < winMins' && winMins' < expMinsUpper) 
>     let allValidWinSess = all (==True) $ map validSimulatedWindows $ winSs
>     assertEqual "test_genSimYear_10b" True allValidWinSess  
      
>     -- all this is to make sure that the number of hours of open is okay
>     let openSs = filter (\s -> (sType s) == Open) $ concatMap sessions $ projs 
>     let openMins = sum $ map sAllottedT openSs
>     let openMins' = fromIntegral openMins
>     --let nonMaintMins = totalMins - maintMins
>     let expMinsLower = (fromIntegral nonMaintMins) * ((0.90+0.25)::Float)
>     let expMinsUpper = expMinsLower * 1.50
>     --print ("simMins", simMins)
>     --print ("maintMins", maintMins)
>     --print ("nonMaintMins", nonMaintMins)
>     --print ("openMins", openMins)
>     --print ("expMins", expMinsLower, expMinsUpper)
>     assertEqual "test_genSimYear_10" True (expMinsLower < openMins' && openMins' < expMinsUpper) 
>
>
>     
>     --let maintMins = sum $ map sAllottedT $ sessions $ last projs
>     --let nonMaintHrs = totalMins - maintMins
>     --print nonMaintMins
>     --assertEqual "test_genSimYear_12" True ((simHrs*60) < nonMaintMins)
>     --assertEqual "test_genSimYear_3" True ((nonMaintHrs < shi) && (slow < nonMaintHrs))
>
>     --print "small!:"
>     --let projs2 = generate 0 g $ genSimTime 150 start 10
>     --let ps = sort $ concatMap periods $ concatMap sessions projs2
>     --printList ps
>     --print $ length ps
>     --print $ sum $ map duration ps 
>     --genSimTimeTest start days True (0.90, 0.05, 0.05) 0.25
>   where
>     start = fromGregorian 2006 1 1 0 0 0
>     days  = 365
>     simHrs = 365*24 
>     tolerance = 200
>     shi  = fromIntegral $ (simHrs + tolerance)
>     slow = fromIntegral $ (simHrs - tolerance)
>     projTime projs = sum $ map sAllottedT $ concatMap sessions projs


> test_genSimTime2 = TestCase $ do
>     let g = mkStdGen 1
>     let simMins = days*24*60
>     --let projs = generate 0 g $ genSimTime start days True (6.0, 0.1, 0.3) 0 --(0.6, 0.3, 0.1) 0.25
>     let projs = generate 0 g $ genSimTime start days True (0.5, 0.5, 0.0) 0 --(0.6, 0.3, 0.1) 0.25
>     let sems = nub . sort $ map semester projs
>     assertEqual "test_genSimYear2_0" ["07C", "08A", "08B", "08C"] sems
>     let isFrom08A = nub . sort $ map (flip isPeriodFromSemester "08A")  $ concatMap periods $ concatMap sessions projs
>     print isFrom08A
>     let sems = map (semester . project . session) $ concatMap periods $ concatMap sessions projs
>     print sems
>     -- TBF: we haven't fixed this yet! sems should be in 08!!!
>   where
>     start = fromGregorian 2008 2 2 0 0 0
>     days  = 35




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
>     -- try a different year
>     let dt = fromGregorian 2009 2 1 0 0 0
>     let proj = generate 0 g $ genMaintenanceProj dt 7 
>     assertEqual "test_genMaintProj_10" "09A" (semester proj)
>     let sems = nub . sort $ map (semester . project . session) $ concatMap periods $ sessions proj
>     assertEqual "test_genMaintProj_10" ["09A"] sems

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
>     let smallDays = 11
>     let ratio = 0.30
>     let bigHrs = round $ ((fromIntegral smallDays) * 24) * ratio
>     let ps = concat $ generate 0 g $ genWindowedSchedule dt smallDays [] bigHrs
>     assertEqual "test_createWS_7" False (internalConflicts ps) 
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

> test_genWindows = TestCase $ do
>     let g = mkStdGen 1
>     let ps = map mkPeriod dts
>     let wins = generate 0 g $ genWindows ps
>     let allPsInAWin = all (\(w, p) -> periodInWindow p w) $ zip wins ps
>     assertEqual "test_genWindows_1" True allPsInAWin
>     -- make sure each period is only in one window
>     let allInOne = all (==True) $ map (inJustOneWindow wins) ps
>     assertEqual "test_genWindows_2" True allInOne
>     let wins2 = generate 0 g $ genWindows []
>     assertEqual "test_genWindows_3" [] wins2 
>     let wins3 = generate 0 g $ genWindows [head ps] 
>     --print wins3
>     assertEqual "test_genWindows_4" 1 (length wins3) 
>     -- see how it fares with randomly generated periods
>     let wps  = generate 0 g $ genWindowedSchedule start days [] (10*24)
>     let wins4 = generate 0 g $ genWindows (head wps)
>     let s = makeSession defaultSession wins4 (head wps)
>     assertEqual "test_genWindows_5" True (validSimulatedWindows s) 
>     let wins5 = generate 0 g $ genWindows (last wps)
>     let s = makeSession defaultSession wins5 (last wps)
>     assertEqual "test_genWindows_6" True (validSimulatedWindows s) 
>   where
>     start = fromGregorian 2006 2 10 5 30 0
>     days = 30
>     psWidth = 30*24*60 -- days in minutes
>     numPs = 5
>     dts = [ addMinutes' (i*psWidth) start | i <- [0..numPs]]
>     mkPeriod dt = defaultPeriod { startTime = dt, duration = 60 }
>     inJustOneWindow ws p = 1 == (length $ filter (==True) $ map (periodInWindow p) ws)

> test_validSimulatedWindows = TestCase $ do
>     assertEqual "test_validSimulatedWindows_1" True (validSimulatedWindows validSess) 
>     assertEqual "test_validSimulatedWindows_2" False (validSimulatedWindows invalidSess) 
>     assertEqual "test_validSimulatedWindows_3" True (validSimulatedWindows validSess2) 
>     assertEqual "test_validSimulatedWindows_4" False (validSimulatedWindows invalidSess2) 
>   where
>     start  = fromGregorian 2006 2 10 5 30 0
>     start2 = fromGregorian 2006 2 14 5 30 0
>     s = defaultSession
>     p  = defaultPeriod { session = s, startTime = start,  duration = 4*60 }
>     p2 = defaultPeriod { session = s, startTime = start2, duration = 4*60 }
>     validW = defaultWindow { wSession = s
>                            , wStart = fromGregorian 2006 2 8 0 0 0
>                            , wDuration = (3*24*60) } 
>     invalidW = defaultWindow { wSession = s
>                            , wStart = fromGregorian 2006 2 8 0 0 0
>                            , wDuration = (1*24*60) } 
>     w2 = defaultWindow { wSession = s
>                        , wStart = fromGregorian 2006 2 13 0 0 0
>                        , wDuration = (3*24*60) } 
>     w3 = defaultWindow { wSession = s
>                        , wStart = fromGregorian 2006 2 10 0 0 0
>                        , wDuration = (7*24*60) } 
>     validSess    = makeSession s [validW] [p]
>     invalidSess  = makeSession s [invalidW] [p]
>     validSess2   = makeSession s [validW, w2] [p, p2]
>     invalidSess2 = makeSession s [validW, w3] [p, p2] -- overlapping windows
> 
