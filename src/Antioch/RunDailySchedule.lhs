
> module Antioch.RunDailySchedule where

> import Antioch.DailySchedule
> import Antioch.Types
> import Antioch.Filters
> import Antioch.Weather
> import Antioch.Score
> --import Antioch.Reports
> import Antioch.DateTime
> import Antioch.Utilities
> import Antioch.DSSData
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
>     -- now get all the input from the DB
>     (rs, ss, projs, history') <- dbInput dt
>     let history = filterHistory history' dt (days + 1) 
>     print "scheduling around periods: "
>     printList history
>     schd <- runScoring w rs $ do
>         sf <- genScore dt . scoringSessions dt $ ss
>         dailySchedule sf strategyName dt days history ss False
>     print . length $ schd
>     printList schd
>     -- new schedule to DB; only write the new periods
>     let newPeriods = schd \\ history
>     print "writing new periods to DB: " 
>     printList newPeriods
>     putPeriods newPeriods

TBF: HACK HACK - dbInput is in RunSimulation too!
Get everything we need from the Database.

> dbInput :: DateTime -> IO (ReceiverSchedule, [Session], [Project], [Period])
> dbInput dt = do
>     rs <- getReceiverSchedule $ Just dt
>     projs <- getProjects
>     let ss = concatMap sessions projs
>     let history = sort $ concatMap periods ss
>     return $ (rs, ss, projs, history)


