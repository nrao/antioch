> module Antioch.RunSimulation where

> import Antioch.DSSData
> --import Antioch.Generators (internalConflicts, endTime, genProjects, genSessions, genPeriods, generateVec)
> import Antioch.HardwareSchedule
> import Antioch.Filters
> import Antioch.Reports
> import Antioch.Simulate
> import Antioch.DateTime
> import Antioch.GenerateSchedule
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities    
> import Antioch.Weather      (Weather(..), getWeather)
> import Control.Monad.Writer
> import Data.List
> import Data.Maybe
> import System.CPUTime
> import System.Random
> import Test.QuickCheck hiding (promote, frequency)

> runSim start days filepath = runSimulation Pack filepath (statsPlotsToFile filepath "") start days "" False True

> runSimulation :: StrategyName -> String -> [[Session] -> [Period] -> [Trace] -> IO ()] -> DateTime -> Int -> String -> Bool -> Bool -> IO ()
> runSimulation strategyName outdir sps dt days name simInput quiet = do
>     now <- getCurrentTime
>     print $ "Scheduling for " ++ show days ++ " days."
>     w <- getWeather Nothing
>     (rs, ss, projs, history') <- if simInput then simulatedInput dt days else dbInput dt
>     let rs = [] -- TBF
>     --print . show $ rs
>     -- print . show $ ss
>     -- print . show $ projs
>     -- print . show $ history'
>     --history'' <- fakeWindows dt days
>     let history = filterHistory history' dt days 
>     print "pre-scheduled history: "
>     printList history
>     let total = sum $ map sAllottedT ss
>     print ("total session time (mins): ", total, total `div` 60)
>     let wss = filter (\s -> (sType s) == Windowed) ss
>     let badWinSessions = filter (not . validSimulatedWindows) wss
>     if (length badWinSessions == 0) then print "Simulated Windows OK" else do
>       print "Invalid Windows Detected; exiting simulation."
>       return ()
>     --(results, trace) <- simulateScheduling strategyName w rs dt dur int history [] ss
>     begin <- getCurrentTime
>     (results, trace) <- simulateDailySchedule rs dt 2 days history ss quiet False [] []
>     end <- getCurrentTime
>     let execTime = end - begin
>     print "done"
>     -- post simulation analysis
>     let quiet = True -- I don't think you every want this verbose?
>     createPlotsAndReports sps name outdir now execTime dt days (show strategyName) ss results trace simInput rs history quiet 
>     -- new schedule to DB; only write the new periods
>     --putPeriods $ results \\ history

> fakeWindows :: DateTime -> Int -> IO [Period]
> fakeWindows dt days = fakeWindows' dt (24 * days)
>   where
>     chunk = 10
>     fakeWindows' dt hours
>       | hours < chunk = return []
>       | otherwise     = do
>           r <- randomIO :: IO Float
>           if r < 0.4
>             then do
>               let p = defaultPeriod { startTime = dt, duration = 60 * chunk }
>               ps <- fakeWindows' (chunk `addHours` dt) (hours - chunk)
>               return $ p : ps
>             else
>               fakeWindows' (chunk `addHours` dt) (hours - chunk)


Get everything we need from the Database.

> dbInput :: DateTime -> IO (ReceiverSchedule, [Session], [Project], [Period])
> dbInput dt = do
>     rs <- getReceiverSchedule $ Just dt
>     projs <- getProjects
>     let ss = concatMap sessions projs
>     let history = sort $ concatMap periods ss
>     return $ (rs, ss, projs, history)

Get everything we need from the simulated input (Generators).
WARNING: the ratio of the amount of session time requested to the
simulation time range is important.  The generators will try to create 
a random fixed and windowed schedule that satisfies a percentage of the
session time requested all within the sim. time range.
For examples:
genSimTime 500 dt 10 : won't work, too many hours of pre-scheduled periods
                       to cram into those 10 days.
genSimTime 150 dt 10 : less pre-scheduled periods can fit in 10 days.

NOTE: The old simulations ran for 365 days using 255 projects, which came 
out to be about 10,000 hours.

> simulatedInput :: DateTime -> Int -> IO (ReceiverSchedule, [Session], [Project], [Period])
> simulatedInput start days = return $ (rs, ss, projs, history)
>   where
>     rs = [] -- [] means all rcvrs up all the time; [(DateTime, [Receiver])]
>     g = mkStdGen 1
>     -- genSimTime start numDays Maint? (open, fixed, windowed) backlogHrs
>     projs = generate 0 g $ genSimTime start days True (0.6, 0.1, 0.3) 0 
>     --projs = generate 0 g $ genProjects 255
>     ss' = concatMap sessions projs
>     ss  = zipWith (\s n -> s {sId = n}) ss' [0..]
>     history = sort $ concatMap periods ss 



