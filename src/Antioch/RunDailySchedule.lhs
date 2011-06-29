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

> module Antioch.RunDailySchedule where

> import Antioch.DailySchedule
> import Antioch.Types
> import Antioch.Filters
> import Antioch.Weather
> import Antioch.ReceiverTemperatures
> import Antioch.Score
> --import Antioch.Reports
> import Antioch.DateTime
> import Antioch.Utilities
> import Antioch.DSSData
> import Antioch.Schedule
> import Antioch.HardwareSchedule
> import Data.List ((\\), sort, find, partition, intersect)
> import Data.Maybe (fromJust, isNothing, listToMaybe)
> import Control.Monad (filterM)

> runDailySchedulePack :: DateTime -> Int -> IO ([Period], [Period])
> runDailySchedulePack dt days = runDailySchedule Pack dt days False

This is simply a wrapper for dailySchedule that takes care of the inputs
and outputs:
   * read data from the DB
   * call dailySchedule
   * write new periods to the DB
Note, if this is a test:
   * don't print all that shit
   * don't actually write anything to the test DB

> runDailySchedule :: StrategyName -> DateTime -> Int -> Bool -> IO ([Period],[Period])
> runDailySchedule strategyName dt days test = do
>     w <- if test then getWeatherTest Nothing else getWeather Nothing
>     rt <- getReceiverTemperatures
>     -- now get all the input from the DB
>     rs <- getReceiverSchedule $ Just dt
>     projs <- getProjects
>     let ss = concatMap sessions projs
>     let all_history = sort $ concatMap periods ss
>     -- only periods in (wholly or partially) in the scheduling range
>     let current_history = truncateHistory all_history dt (days + 1) 
>     quietPrint test False "original history: "
>     quietPrint test True current_history
>     -- remove non-viable periods
>     scheduling_history <- filterHistory w rs rt current_history ss projs test
>     quietPrint test False "scheduling history: "
>     quietPrint test True scheduling_history
>     schd <- runScoring w rs rt $ do
>         sf <- genScore dt . scoringSessions dt undefined $ ss
>         dailySchedule sf strategyName dt days scheduling_history ss test
>     quietPrint test True schd
>     -- new schedule to DB; only write the new periods
>     let newPeriods = schd \\ scheduling_history
>     quietPrint test False "writing new periods to DB: " 
>     quietPrint test True newPeriods
>     -- only write to DB if we aren't testing
>     if not test then putPeriods newPeriods else putStrLn ""
>     -- do we need to remove any failed electives or default periods?
>     let periodsToDelete =  current_history \\ scheduling_history
>     quietPrint test False "moving to deleted: "
>     quietPrint test True periodsToDelete
>     -- only write to DB if we aren't testing
>     if not test then movePeriodsToDeleted periodsToDelete else putStrLn ""
>     return (newPeriods, periodsToDelete)

If we're running tests and/or trying to be quiet, we don't want to print

> quietPrint t l x = if t then putStrLn "" else (if l then printList x else print x)

Filter out deprecated periods:
   * unused elective periods
   * un-needed default periods
   * inactive periods
   * blacked-out periods

> filterHistory w rs rt history ss projs quiet = do
>     -- Shield maintenance periods from filtering
>     let (history_maintenance, history_nonmaintenance) =
>             partition (\p -> (oType . session $ p) == Maintenance) history
>     quietPrint quiet False "history_maintenance"
>     quietPrint quiet True history_maintenance
>     quietPrint quiet False "history_nonmaintenance"
>     quietPrint quiet True history_nonmaintenance
>
>     -- Filter out periods from disabled/unauthorized sessions.
>     history'inactive <- filterInactivePeriods history_nonmaintenance
>     quietPrint quiet False "original history - maintenance - inactive"
>     quietPrint quiet True history'inactive
>
>     -- Filter out periods having inadequate representation.
>     let history'no_observer = filter (flip periodObsAvailable ss) history'inactive
>     quietPrint quiet False "original history - maintenance - inactive - no observers: "
>     quietPrint quiet True history'no_observer
>
>     -- Filter out failing elective periods.
>     history'electives <- filterElectives w rs rt history'no_observer
>     quietPrint quiet False "original history - maintenance - inactive - no observers - electives: "
>     quietPrint quiet True history'electives
>
>     -- Filter out default periods of non-guaranteed, windowed
>     -- sessions if they fail MOC.
>     history'defaulted <- filterDefaultPeriods w rs rt history'electives
>     quietPrint quiet False "original history - maintenance - inactive - no observers - electives - default: "
>     quietPrint quiet True history'defaulted
>
>     -- Return maintenance periods into the mix
>     let scheduling_history =
>             sort . concat $ [history'defaulted, history_maintenance]
>     quietPrint quiet False "original history - inactive - no observers - electives - default: "
>     quietPrint quiet True scheduling_history
>
>     return scheduling_history

Determines whether some observer, any observer is available for
the entire length of the period.  We need the pool of sessions
because the programmers never could figure out how to tie knots
across several layers.  The gist is that the session referenced
by a period contains an empty period list!

> periodObsAvailable :: Period -> [Session] -> Bool
> periodObsAvailable p ss =
>     if isNothing ms
>     then False
>     else all (flip obsAvailable (fromJust ms)) dts
>   where
>     id = peId p
>     ms = find (\s -> elem id (map peId (periods s))) ss
>     dts = [(i*quarter) `addMinutes` (startTime p) | i <- [0..((duration p) `div` quarter)]]


Note - this really should be in Filters, but it requires
Score, which would cause cyclical imports.
Filter out of the history any elective periods that shouldn't
stay on the schedule.
Note: Really, this should be done inside dailySchedule so
that electives can be covered by simualtions as well,
but it's so much simpler to do it here, and I doubt
simulations will need to cover electives.  so there.

> filterElectives :: Weather -> ReceiverSchedule -> ReceiverTemperatures -> [Period] -> IO [Period]
> filterElectives w rs rt ps = do
>   geps <- cleanElectives w rs rt [] eps
>   --return . sort . (++) neps $ geps
>   print ("filterElectives", map peId ps, map peId neps, map peId geps, map peId seps)
>   return . sort . concat $ [neps, geps, seps]
>     where
>       (eps', neps) = partition (typeElective . session) ps
>       (seps, eps) = partition (\p -> (pState p) == Scheduled) eps'

Search a list of elective periods, and whenever a "good" elective is
found, i.e., one which will be placed on the schedule for the
scheduler's approval, remove all of its siblings, i.e., other
periods belonging to the same elective. An elective is considered
good if it is the last elective (default), already scheduled, or
meets the MOC.

> -- cleanElectives   weather  rcvr_sched          rcvr_temps      good_elec_periods all_elec_periods
> cleanElectives :: Weather -> ReceiverSchedule -> ReceiverTemperatures -> [Period] -> [Period] -> IO [Period]

> cleanElectives _  _  _ geps  [] = do
>     return geps

> cleanElectives w rs rt geps aeps = do
>   eps <- filterM goodElective' aeps
>   if eps == [] then return geps
>                else let gep = head eps
>                     in cleanElectives w rs rt (gep:geps) . cleanElectives' gep $ aeps
>     where
>       goodElective' p = do
>         runScoring w rs rt $ goodElective p

Remove all the periods in the list of elective periods which share the
same elective as the give period. Note that the function does not need
sessions because though not all the knots are tied, the session found
through the period does have the list electives.

> cleanElectives' :: Period -> [Period] -> [Period]
> cleanElectives' gep aeps = do
>   let mge = maybe defaultElective id $ getElective gep
>   filter (\p -> not . elem mge . electives . session $ p) aeps
>     where
>       sameElective mge ep = if mge == Nothing
>                             then True
>                             else elem (fromJust mge) ep

Note - this really should be in Filters, but it requires
Score, which would cause cyclical imports.
Filter out of the history any default periods that:
   * belong to non-guaranteed sessions
   * and don't pass their MOC

> filterDefaultPeriods :: Weather -> ReceiverSchedule -> ReceiverTemperatures -> [Period] -> IO ([Period])
> filterDefaultPeriods w rs rt ps =  concatMapM goodDefaultPeriod' ps
>   where
>     goodDefaultPeriod' p = do
>       r <- runScoring w rs rt $ goodDefaultPeriod p
>       case r of
>         True -> return $ [p]
>         False -> return []

