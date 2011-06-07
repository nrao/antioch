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

> runDailySchedulePack :: DateTime -> Int -> IO ()
> runDailySchedulePack dt days = runDailySchedule Pack dt days

This is simply a wrapper for dailySchedule that takes care of the inputs
and outputs:
   * read data from the DB
   * call dailySchedule
   * write new periods to the DB

> runDailySchedule :: StrategyName -> DateTime -> Int -> IO ()
> runDailySchedule strategyName dt days = do
>     w <- getWeather Nothing
>     rt <- getReceiverTemperatures
>     -- now get all the input from the DB
>     (rs, ss, projs, all_history) <- dbInput dt
>     -- only periods in (wholly or partially) in the scheduling range
>     let current_history = truncateHistory all_history dt (days + 1) 
>     print "original history: "
>     printList current_history
>     -- remove non-viable periods
>     scheduling_history <- filterHistory w rs rt current_history ss projs
>     schd <- runScoring w rs rt $ do
>         sf <- genScore dt . scoringSessions dt undefined $ ss
>         dailySchedule sf strategyName dt days scheduling_history ss False
>     print . length $ schd
>     printList schd
>     -- new schedule to DB; only write the new periods
>     let newPeriods = schd \\ scheduling_history
>     print "writing new periods to DB: " 
>     printList newPeriods
>     putPeriods newPeriods
>     -- do we need to remove any failed electives or default periods?
>     print "moving to deleted: "
>     periods <- filterMaintenancePeriods $ current_history \\ scheduling_history
>     let periodsToDelete = periods
>     printList periodsToDelete
>     movePeriodsToDeleted periodsToDelete

Filter out deprecated periods:
   * unused elective periods
   * un-needed default periods
   * inactive periods
   * blacked-out periods

> filterHistory w rs rt history ss projs = do
>     -- Note: Really, this should be done inside dailySchedule so
>     -- that electives can be covered by simualtions as well,
>     -- but it's so much simpler to do it here, and I doubt
>     -- simulations will need to cover electives.  so there.
>     -- Only elective periods that will be scheduled.
>     history'electives <- filterElectives w rs rt history
>     print "original history - electives: "
>     printList history'electives
>
>     -- similarly, default periods of non-guaranteed, windowed
>     -- sessions only run if they pass MOC
>     history'defaulted <- filterDefaultPeriods w rs rt history'electives
>     print "original history - electives - default: "
>     printList history'defaulted
>     print . length $ history'defaulted
>
>     -- filter out periods having inadequate representation
>     let history'no_observer = filter (flip periodObsAvailable ss) history'defaulted 
>     print . length $ history'no_observer
>     print "original history - electives - default - no observers: "
>     printList history'no_observer
>
>     -- filter out inactive/unauthorized periods
>     scheduling_history <- filterInactivePeriods history'no_observer
>     print "original history - electives - default - no observers - inactive"
>     print " and scheduling around periods: "
>     printList scheduling_history
>     return scheduling_history

Determines whether some observer, any observer is available for
the entire length of the period.  We need the pool of sessions
because the programmers never could figure out how to tie knots
across several layers.

> periodObsAvailable :: Period -> [Session] -> Bool
> periodObsAvailable p ss =
>     if isNothing ms
>     then False
>     else all (flip obsAvailable (fromJust ms)) dts
>   where
>     id = peId p
>     ms = find (\s -> elem id (map peId (periods s))) ss
>     dts = [(i*quarter) `addMinutes` (startTime p) | i <- [0..((duration p) `div` quarter)]]


TBF: HACK HACK - this really should be in Filters, but it requires
Score, which would cause cyclical imports.
Filter out of the history any elective periods that shouldn't
stay on the schedule.

> filterElectives :: Weather -> ReceiverSchedule -> ReceiverTemperatures -> [Period] -> IO [Period]
> filterElectives w rs rt ps = do
>   geps <- cleanElectives w rs rt [] eps
>   return . sort . (++) neps $ geps
>     where
>       (eps, neps) = partition (typeElective . session) ps

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
same elective as the give period.

> cleanElectives' :: Period -> [Period] -> [Period]
> cleanElectives' gep aeps = do
>   let mge = maybe defaultElective id $ getElective gep
>   filter (\p -> not . elem mge . electives . session $ p) aeps
>     where
>       sameElective mge ep = if mge == Nothing
>                             then True
>                             else elem (fromJust mge) ep

TBF: HACK HACK - this really should be in Filters, but it requires
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


TBF: HACK HACK - dbInput is in RunSimulation too!
Get everything we need from the Database.

> dbInput :: DateTime -> IO (ReceiverSchedule, [Session], [Project], [Period])
> dbInput dt = do
>     rs <- getReceiverSchedule $ Just dt
>     projs <- getProjects
>     let ss = concatMap sessions projs
>     let history = sort $ concatMap periods ss
>     return $ (rs, ss, projs, history)


