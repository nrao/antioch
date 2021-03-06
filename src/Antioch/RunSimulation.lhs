Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

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
> --import Antioch.Weather      (Weather(..), getWeather)
> import Antioch.Weather
> import Antioch.Debug
> import Control.Monad.Writer
> import Data.List
> import Data.Maybe
> import System.CPUTime
> import System.Random
> import Test.QuickCheck hiding (promote, frequency)

Shortcut to runSimulation:

> runSim start days filepath = runSimulation Pack start days False 100 0 0 2000 filepath "" False True False

This high-level function sets up all the input (sessions, periods, etc.), 
passes it to simulateDailySchedule, and processes the output (ex: reports and plots are generated). 

   * strategyName: one of Pack, MinimumDuration ... 
   * outdir: where the plots and text report go
   * dt: starting datetime
   * days: num days of simulation
   * maint: generate maintenance sessions
   * open: per cent open sessions
   * fixed: per cent fixed sessions
   * windowed: per cent windowed sessions
   * backlog: hours of backlog sessions
   * name: name of simulation (a label in report and plots)
   * simInput: use real projs from DB, or simulated?
   * quiet: sssh!
   * test: if this is a test, use weatherTestDB

> runSimulation :: StrategyName -> DateTime -> Int -> Bool -> Int -> Int -> Int -> Int -> String -> String -> Bool -> Bool -> Bool -> IO ()
> runSimulation strategyName dt days maint open fixed windowed backlog outdir name simInput quiet test = do
>     now <- getCurrentTime
>     print $ "Scheduling for " ++ show days ++ " days."
>     (rs, ss, projs, history') <- if simInput then simulatedInput dt days maint open fixed windowed backlog else dbInput dt
>     let rs = [] -- We aren't simulating w/ receiver schedule yet.
>     -- print . show $ rs
>     -- print . show $ ss
>     -- print . show $ projs
>     -- print . show $ history'
>     --history'' <- fakeWindows dt days
>     let history = truncateHistory history' dt days 
>     print "pre-scheduled history: "
>     printList history
>     let total = sum $ map sAllottedT ss
>     print ("total session time (mins): ", total, total `div` 60)
>     let wss = filter typeWindowed ss
>     let badWinSessions = filter (not . validSimulatedWindows) wss
>     if (length badWinSessions == 0) then print "Simulated Windows OK" else do
>       print "Invalid Windows Detected; exiting simulation."
>       printList badWinSessions
>       printList $ concatMap windows badWinSessions
>       return ()
>     --(results, trace) <- simulateScheduling strategyName w rs dt dur int history [] ss
>     begin <- getCurrentTime
>     let quiet = True
>     let wType = if test then TestWeather else NormalWeather
>     (results, trace, finalSess) <- simulateDailySchedule rs dt 2 days history ss quiet wType [] []
>     end <- getCurrentTime
>     let execTime = end - begin
>     print "done"
>     -- post simulation analysis
>     let quiet = True -- I don't think you every want this verbose?
>     let plots = True -- yes, create the plots
>     createPlotsAndReports name outdir now execTime dt days (show strategyName) finalSess results trace simInput rs history quiet wType plots 
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
Note how genSimTime works: here you specify lots of things about the input.

NOTE: the last parameter to genSimTime is the hours of backlog, which you may want
to change depending on how long the simulation is running.

NOTE: The old simulations ran for 365 days using 255 projects, which came 
out to be about 10,000 hours.

> simulatedInput :: DateTime -> Int -> Bool -> Int -> Int -> Int -> Int -> IO (ReceiverSchedule, [Session], [Project], [Period])
> simulatedInput start days maint open fixed windowed backlog = return $ (rs, ss, projs, history)
>   where
>     rs = [] -- [] means all rcvrs up all the time; [(DateTime, [Receiver])]
>     g = mkStdGen 1
>     -- genSimTime start numDays Maint? (open, fixed, windowed) backlogHrs
>     pc2frac i = (fromIntegral i) / 100.0
>     projs = generate 0 g $ genSimTime start days maint (pc2frac open, pc2frac fixed, pc2frac windowed) backlog 
>     --projs = generate 0 g $ genSimTime start days True (0.6, 0.1, 0.3) 0 
>     --projs = generate 0 g $ genSimTime start days False (1.0, 0.0, 0.0) 2000 
>     --projs = generate 0 g $ genProjects 255
>     ss' = concatMap sessions projs
>     -- assign Id's to the open sessions
>     maxId = maximum $ map sId ss'
>     ss  = (filter (not . typeOpen) ss') ++ (zipWith (\s n -> s {sId = n}) (filter typeOpen ss') [(maxId+1)..])
>     history = sort $ concatMap periods ss 

> runDailyEfficiencies :: DateTime -> Int -> Bool -> IO [()]
> runDailyEfficiencies dt days simInput = do
>     w <- getWeather Nothing
>     let g = mkStdGen 1
>     -- make number of projects independent of number of days
>     let projs = generate 0 g $ genProjects 255
>     let ss = concatMap sessions projs
>     print $ "Session Hours: " ++ (show $ sum (map sAllottedS ss))
>     print "Plotting daily mean efficiencies:"
>     plotEfficienciesByTime w ss dt days
