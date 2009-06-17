> module Antioch.BenchmarkTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Schedule.Pack
> import Antioch.Score
> import Antioch.Weather
> import Antioch.PProjects
> import Control.Exception (assert)
> import System.CPUTime

> benchmarks :: IO ()
> benchmarks = do
>   benchmark_packWorker_1
>   benchmark_packWorker_2
>   benchmark_pack_1
>   benchmark_pack_2

> showExecTime name start stop = do 
>   if fromIntegral (stop-start) / 1.0e9 == 0.0 then showExecTimeNs name start stop else showExecTimeMs name start stop

> showExecTimeMs name start stop = do 
>   let execTime = fromIntegral (stop-start) / 1.0e9
>   putStrLn $ name ++ " Execution Speed: " ++ show execTime ++ " ms"

> showExecTimeNs name start stop = do 
>   print start
>   print stop
>   let execTime = fromIntegral (stop-start)  -- / 1.0e3
>   putStrLn $ name ++ " Execution Speed: " ++ show execTime ++ " ns"

TBF: It seems that laziness leads to some of the tests executing in NO time.
So write to dev/nell to force some execution to take place.

> forceExec :: String -> IO ()
> forceExec str =  writeFile "/dev/null" str

> benchmark_pack_1 :: IO ()
> benchmark_pack_1 = do 
>   w <- getWeather . Just $ starttime 
>   start <- getCPUTime
>   periods' <- runScoring w [] $ do
>       fs <- genScore sess
>       pack fs starttime duration [] sess
>   stop <- getCPUTime
>   showExecTime "benchmark_pack_1" start stop
>     where
>       sess = getOpenPSessions 
>       starttime = fromGregorian 2006 11 8 12 0 0
>       duration = 24*60

> benchmark_pack_2 :: IO ()
> benchmark_pack_2 = do 
>   w <- getWeather . Just $ starttime 
>   start <- getCPUTime
>   periods' <- runScoring w [] $ do
>       fs <- genScore sess
>       pack fs starttime duration [] sess
>   stop <- getCPUTime
>   showExecTime "benchmark_pack_2" start stop
>     where
>       sess = getOpenPSessions 
>       starttime = fromGregorian 2006 6 1 0 0 0
>       duration = 24*60

> benchmark_packWorker_1 :: IO ()
> benchmark_packWorker_1 = do
>   start <- getCPUTime
>   let zs = packWorker xs ys
>   forceExec (show . length $ zs)
>   stop <- getCPUTime
>   showExecTime "benchmark_packWorker_1" start stop
>     where 
>       xs = replicate 97 Nothing -- 24 blank hours
>       enoughTime = 10000
>       scores1 = (replicate 50 1.0) ++ (replicate 50 0.0)
>       i1s = map (\id -> Item id 1 2 4 enoughTime enoughTime 8 scores1 []) [0 .. 100]
>       scores2 = (replicate 50 0.0) ++ (replicate 50 1.0)
>       i2s = map (\id -> Item id 1 3 6 enoughTime enoughTime 6 scores2 []) [101 .. 200]
>       ys = i1s ++ i2s
>   
> benchmark_packWorker_2 :: IO ()
> benchmark_packWorker_2 = do
>   start <- getCPUTime
>   let zs = packWorker xs ys
>   forceExec (show .length $ zs)
>   stop <- getCPUTime
>   showExecTime "benchmark_packWorker_2" start stop
>     where 
>       xs = replicate 97 Nothing -- 24 blank hours
>       scores1 = (replicate 50 1.0) ++ (replicate 50 0.0)
>       i1s = map (\id -> Item id 1 2 4 5 5 8 scores1 []) [0 .. 100]
>       scores2 = (replicate 50 0.0) ++ (replicate 50 1.0)
>       i2s = map (\id -> Item id 1 3 6 10 10 6 scores2 []) [101 .. 200]
>       ys = i1s ++ i2s

