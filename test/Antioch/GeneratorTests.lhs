> module Antioch.GeneratorTests where

> import Antioch.Generators
> import Antioch.DateTime
> import Antioch.Types
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

> tests = TestList [test_genMaintenanceProject
>                 , test_genSimYear
>                 , test_genWeeklyMaintPeriods]

> test_genSimYear = TestCase $ do
>     let g = mkStdGen 1
>     let projs = generate 0 g $ genSimYear simHrs 2006
>     let totalMins = sum $ map sAllottedT $ concatMap sessions projs
>     assertEqual "test_genSimYear_1" True (100 < length projs)
>     assertEqual "test_genSimYear_1" "Maintenance" (pName . last $ projs)
>     let maintMins = sum $ map sAllottedT $ sessions $ last projs
>     let nonMaintHrs = (fromIntegral (totalMins - maintMins)) / 60
>     assertEqual "test_genSimYear_1" True ((nonMaintHrs < shi) && (slow < nonMaintHrs))
>     let projs = generate 0 g $ genSimYear 0 2006
>     assertEqual "test_genSimYear_1" 1 (length projs)
>     assertEqual "test_genSimYear_1" "Maintenance" (pName . head $ projs)
>   where
>     simHrs = 10000
>     tolerance = 200
>     shi  = fromIntegral $ (simHrs + tolerance)
>     slow = fromIntegral $ (simHrs - tolerance)



> test_genMaintenanceProject = TestCase $ do
>     let g = mkStdGen 1
>     let proj = generate 0 g $ genMaintenanceProj 2006
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

> test_genWeeklyMaintPeriods = TestCase $ do
>     let g = mkStdGen 1
>     let ps = generate 0 g $ genWeeklyMaintPeriods start end defaultSession
>     assertEqual "test_genWeeklyMaintPeriods" True (all (<= end) $ map startTime ps) 
>     assertEqual "test_genWeeklyMaintPeriods" True (all (>= start) $ map startTime ps) 
>     assertEqual "test_genWeeklyMaintPeriods" 22 (length ps) 
>   where
>     start = fromGregorian 2006 1 1 0 0 0
>     end   = fromGregorian 2006 6 1 0 0 0

