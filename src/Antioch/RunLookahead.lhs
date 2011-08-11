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

> module Antioch.RunLookahead where

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
> import Antioch.Weather
> import Antioch.Debug
> import Control.Monad.Writer
> import Data.List
> import Data.Maybe
> import System.CPUTime
> import System.Random
> import Test.QuickCheck hiding (promote, frequency)

This performs a 'lookahead' simulation.

This high-level function sets up all the input (sessions, periods, etc.), 
from the database specified in settings, passes it to simulateDailySchedule, and writes the results to the database again. 

   * outdir: where the plots and text report go
   * dt: starting datetime
   * days: num days of simulation
   * name: name of simulation (a label in report and plots)
   * quiet: sssh!
   * test: if this is a test, use weatherTestDB, otherwise, the lookahead

> runLookahead :: DateTime -> Int -> String -> String -> Bool -> Bool -> IO ()
> runLookahead dt days outdir name quiet test = do
>     now <- getCurrentTime
>     print $ "Scheduling for " ++ show days ++ " days."
>     (rs, ss, projs, history') <- dbInput dt
>     let history = truncateHistory history' dt days 
>     print "pre-scheduled history: "
>     printList history
>     let total = sum $ map sAllottedT ss
>     print ("total session time (mins): ", total, total `div` 60)
>     begin <- getCurrentTime
>     let wType = if test then TestWeather else LookaheadWeather
>     (results, trace, finalSess) <- simulateDailySchedule rs dt 2 days history ss quiet wType [] []
>     end <- getCurrentTime
>     let execTime = end - begin
>     print "done"
>     -- post simulation analysis
>     let quiet = True -- I don't think you every want this verbose?
>     let plots = False -- the plots aren't needed and crash anyways
>     createPlotsAndReports name outdir now execTime dt days "Pack" finalSess results trace False rs history quiet wType plots 
>     -- new schedule to DB; only write the new periods
>     let newPeriods = results \\ history
>     print "new periods: "
>     printList newPeriods
>     putPeriods newPeriods (Just Scheduled)
>     -- update sessions - many may now be completed;
>     let completedSess = filter (sessionLookaheadCompleted ss) finalSess
>     print "sessions completed: "
>     printList $ map sName completedSess
>     updateCompletedSessions completedSess

Did this session go from not completed, to completed?

> sessionLookaheadCompleted :: [Session] -> Session -> Bool
> sessionLookaheadCompleted originalSess s = (sClosed s) && (not . sClosed $ origS)
>   where
>     origS = fromJust $ find (\s' -> (sId s') == (sId s)) originalSess


Get everything we need from the Database.

> dbInput :: DateTime -> IO (ReceiverSchedule, [Session], [Project], [Period])
> dbInput dt = do
>     rs <- getReceiverSchedule $ Just dt
>     projs <- getProjects
>     let ss = concatMap sessions projs
>     let history = sort $ concatMap periods ss
>     return $ (rs, ss, projs, history)

