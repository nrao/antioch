
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
> import Data.List ((\\), sort)

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
>     (rs, ss, projs, history') <- dbInput dt
>     let history' = filterHistory history' dt (days + 1) 
>     -- TBF: Really, this should be done inside dailySchedule so that electives
>     -- can be covered by simualtions as well, but it's so much simpler to do
>     -- it here, and I doubt sims will need to cover electives.  so there.
>     history <- filterElectives history'
>     print "scheduling around periods: "
>     printList history
>     schd <- runScoring w rs rt $ do
>         sf <- genScore dt . scoringSessions dt undefined $ ss
>         dailySchedule sf strategyName dt days history ss False
>     print . length $ schd
>     printList schd
>     -- new schedule to DB; only write the new periods
>     let newPeriods = schd \\ history
>     print "writing new periods to DB: " 
>     printList newPeriods
>     putPeriods newPeriods
>     -- do we need to remove any failed electives?
>     movePeriodsToDeleted $ history \\ history' 

TBF: HACK HACK - this really should be in Filters, but it requires
Score, which would cause cyclical imports.
Filter out of the history any elective periods that shouldn't
stay on the schedule.

> filterElectives :: Weather -> ReceiverSchedule -> ReceiverTemperatures -> [Period] -> IO ([Period])
> filterElectives w rs rt ps =  runScoring w rs rt $ filter goodElective ps

TBF: HACK HACK - dbInput is in RunSimulation too!
Get everything we need from the Database.

> dbInput :: DateTime -> IO (ReceiverSchedule, [Session], [Project], [Period])
> dbInput dt = do
>     rs <- getReceiverSchedule $ Just dt
>     projs <- getProjects
>     let ss = concatMap sessions projs
>     let history = sort $ concatMap periods ss
>     return $ (rs, ss, projs, history)


