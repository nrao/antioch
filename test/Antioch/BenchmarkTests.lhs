> module Antioch.BenchmarkTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Schedule.Pack as P 
> import Antioch.Schedule 
> import Antioch.Score
> import Antioch.Weather
> import Antioch.PProjects
> import Antioch.Simulate
> import Antioch.RunSimulation
> import Antioch.Utilities
> import Antioch.Reports
> import Control.Exception (assert)
> import System.CPUTime

> benchmarks :: IO ()
> benchmarks = do
>   benchmark_weather_1
>   benchmark_score_1
>   benchmark_score_2
>   benchmark_packWorker_1
>   benchmark_packWorker_2
>   benchmark_pack_1
>   benchmark_pack_2
>   benchmark_simulate_1
>   benchmark_simulate_2
>   benchmark_simulateScheduling_1
>   benchmark_simulateScheduling_2
>   benchmark_generatePlots_1

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

Introduced in rev 1028; no difference from rev 959

> benchmark_weather_1 :: IO ()
> benchmark_weather_1 = do
>   w <- getWeather . Just $ starttime 
>   start <- getCPUTime
>   tsysValues <- mapM (getWeatherData freq w) times
>   stop <- getCPUTime
>   showExecTime "benchmark_weather_1" start stop
>     where
>       freq = 3.0
>       starttime = fromGregorian 2006 11 8 12 0 0
>       days = 7
>       numQtrs = days * 24 * 4
>       times = [(15*q) `addMinutes'` starttime | q <- [0..numQtrs]]
>       getWeatherData freq w dt = tsys w dt freq
>       

Introduced in rev 1028; no difference from rev 959

> benchmark_score_1 :: IO ()
> benchmark_score_1 = do
>   w <- getWeather . Just $ starttime 
>   start <- getCPUTime
>   score <- runScoring w [] $ genScore starttime sess >>= \f -> f starttime s
>   stop <- getCPUTime
>   showExecTime "benchmark_score_1" start stop
>     where
>       sess = getOpenPSessions 
>       s = head $ sess
>       starttime = fromGregorian 2006 11 8 12 0 0

Introduced in rev 1028; 
Twice as slow as when run in rev 959!
Reintroducing static isDayTime code brings it back to almost same time as 959!

> benchmark_score_2 :: IO ()
> benchmark_score_2 = do
>   w <- getWeather . Just $ starttime 
>   --score <- runScoring w [] $ genScore starttime sess >>= \f -> f starttime s
>   let score' w dt = runScoring w [] $ do
>       fs <- genScore dt sess
>       score <- fs dt s
>       return $ eval score
>   start <- getCPUTime
>   scores <- mapM (score' w) times
>   stop <- getCPUTime
>   showExecTime "benchmark_score_2" start stop
>     where
>       sess = getOpenPSessions 
>       s = head $ sess
>       starttime = fromGregorian 2006 11 8 12 0 0
>       times = [(15*q) `addMinutes'` starttime | q <- [0..96]]


> benchmark_pack_1 :: IO ()
> benchmark_pack_1 = do 
>   w <- getWeather . Just $ starttime 
>   start <- getCPUTime
>   periods' <- runScoring w [] $ do
>       fs <- genScore starttime sess
>       P.pack fs starttime duration [] sess
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
>       fs <- genScore starttime sess
>       P.pack fs starttime duration [] sess
>   stop <- getCPUTime
>   showExecTime "benchmark_pack_2" start stop
>     where
>       sess = getOpenPSessions 
>       starttime = fromGregorian 2006 6 1 0 0 0
>       duration = 24*60

> benchmark_simulate_1 :: IO ()
> benchmark_simulate_1 = do
>   w <- getWeather Nothing
>   start <- getCPUTime
>   (results, trace) <- simulateDailySchedule rs dt packdays simdays hist ss quiet sched trace 
>   stop <- getCPUTime
>   showExecTime "benchmark_simulate_1" start stop
>     where
>       ss = getOpenPSessions
>       rs = [] -- rcvr schedule
>       hist = [] -- pre-schedule periods
>       dt = fromGregorian 2006 6 1 0 0 0
>       simdays = 3
>       packdays = 2
>       quiet = True
>       sched = []
>       trace = []

More Sessions, for longer

> benchmark_simulate_2 :: IO ()
> benchmark_simulate_2 = do
>   w <- getWeather Nothing
>   start <- getCPUTime
>   (results, trace) <- simulateDailySchedule rs dt packdays simdays hist ss quiet sched trace 
>   stop <- getCPUTime
>   print . show . length $ ss
>   showExecTime "benchmark_simulate_2" start stop
>     where
>       ss = getBigSessionPool
>       rs = [] -- rcvr schedule
>       hist = [] -- pre-schedule periods
>       dt = fromGregorian 2006 6 1 0 0 0
>       simdays = 10 
>       packdays = 2
>       quiet = True
>       sched = []
>       trace = []

> benchmark_simulateScheduling_1 :: IO ()
> benchmark_simulateScheduling_1 = do
>   w <- getWeather Nothing
>   start <- getCPUTime
>   (results, trace) <- simulateDailySchedule rs dt packdays simdays hist ss quiet sched trace 
>   stop <- getCPUTime
>   showExecTime "benchmark_simulateScheduling_1" start stop
>     where
>       ss = getOpenPSessions
>       rs = [] -- rcvr schedule
>       hist = [] -- pre-schedule periods
>       dt = fromGregorian 2006 6 1 0 0 0
>       simdays = 3
>       packdays = 2
>       quiet = True
>       sched = []
>       trace = []

More sessions, for longer 

> benchmark_simulateScheduling_2 :: IO ()
> benchmark_simulateScheduling_2 = do
>   w <- getWeather Nothing
>   start <- getCPUTime
>   (results, trace) <- simulateDailySchedule rs dt packdays simdays hist ss quiet sched trace 
>   stop <- getCPUTime
>   showExecTime "benchmark_simulateScheduling_2" start stop
>     where
>       ss = getBigSessionPool
>       rs = [] -- rcvr schedule
>       hist = [] -- pre-schedule periods
>       dt = fromGregorian 2006 6 1 0 0 0
>       simdays = 10
>       packdays = 2
>       quiet = True
>       sched = []
>       trace = []

> benchmark_packWorker_1 :: IO ()
> benchmark_packWorker_1 = do
>   start <- getCPUTime
>   let zs = packWorker xs ys
>   forceExec (show . length $ zs)
>   stop <- getCPUTime
>   showExecTime "benchmark_packWorker_1" start stop
>     where 
>       o = Optional
>       --dur = 24 * 60
>       xs = replicate 97 Nothing -- 24 blank hours
>       enoughTime = 10000
>       scores1 = (replicate 50 1.0) ++ (replicate 50 0.0)
>       i1s = map (\id -> Item id 1 2 4 enoughTime enoughTime 8 o [] scores1 []) [0 .. 100]
>       scores2 = (replicate 50 0.0) ++ (replicate 50 1.0)
>       i2s = map (\id -> Item id 1 3 6 enoughTime enoughTime 6 o [] scores2 []) [101 .. 200]
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
>       o = Optional
>       --dur = 24 * 60
>       xs = replicate 97 Nothing -- 24 blank hours
>       scores1 = (replicate 50 1.0) ++ (replicate 50 0.0)
>       i1s = map (\id -> Item id 1 2 4 5 5 8 o [] scores1 []) [0 .. 100]
>       scores2 = (replicate 50 0.0) ++ (replicate 50 1.0)
>       i2s = map (\id -> Item id 1 3 6 10 10 6 o [] scores2 []) [101 .. 200]
>       ys = i1s ++ i2s

This benchmark test is not very reliable, since it relies on randomly generated
input, which can vary everytime (unrelated) parts of the code change.  But
I thought it would be interesting to do anyways.

> benchmark_generatePlots_1 :: IO ()
> benchmark_generatePlots_1 = do
>   start <- getCPUTime
>   runSim dt 120 "sims" -- a whole semester!
>   -- don't need this, due to production of things in the real world (plots)
>   -- forceExec (show .length $ zs)
>   stop <- getCPUTime
>   showExecTime "benchmark_generatePlots_1" start stop
>     where 
>       dt = fromGregorian 2006 6 1 0 0 0

Utilities:

We can't use the Generators to give us a large pool of sessions to test with, 
because we don't want this input to our tests to change.  So we'll take the
small pool we get from PProjects, and build off that.

> getBigSessionPool :: [Session]
> getBigSessionPool = concatMap sessions $ map expand pTestProjects
>   where
>     applyUniqueIds ss = zipWith (\s n -> s {sId = n}) ss [0..]

Takes a project's sessions, uses them as a template to make more of them,
and recreates the project to include these new ones.

> expand :: Project -> Project
> expand p = makeProject p enoughTime enoughTime $ newSessions p
>   where
>     enoughTime = 1000000000 * 60

> newSessions :: Project -> [Session]
> newSessions p = concatMap gimmeMore $ sessions p

> gimmeMore :: Session -> [Session]
> gimmeMore s = take 10 $ iterate incrementSession s

> incrementSession :: Session -> Session
> incrementSession s = s {frequency = newF s, ra = newRa s, dec = newDec s}
>   where
>     newF s = (frequency s) + 2.0 -- GHz
>     newRa s = if (validRA ((ra s) + 0.5)) then ((ra s) + 0.5) else 0.5 -- rads
>     newDec s =  if validDec ((dec s) + 0.5) then ((dec s) + 0.5) else 0.5 -- rads

> validRA :: Radians -> Bool
> validRA r = 0.0 <= ra' && ra' <= 24.0
>   where 
>     ra' = rad2hrs r

> validDec :: Radians -> Bool
> validDec r = -45.0 <= dec' && dec' <= 90.0
>   where
>     dec' = rad2deg r

