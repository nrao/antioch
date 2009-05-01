> module Antioch.DSSDataTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather (getWeather)
> import Antioch.Score
> import Antioch.DSSData
> import Maybe
> import List (nub)
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)

> tests = TestList [
>       test_getProjectData
>     , test_getProjects
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
>     assertEqual "test_getProjects1" 104 (length ps)  
>     assertEqual "test_getProjects5" 2 (pId . head $ ps)  
>     assertEqual "test_getProjects2" "BB240" (pName . head $ ps)  
>     assertEqual "test_getProjects3" 48480 (timeTotal . head $ ps)  
>     assertEqual "test_getProjects4" 16 (length . sessions . head $ ps)  
>     assertEqual "test_getProjects8" Windowed (sType . head $ ss)
>     assertEqual "test_getProjects6" 2 (pId . project . head $ ss)    
>     assertEqual "test_getProjects7" 1 (length . nub $ map (pId . project) $ ss) 

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

