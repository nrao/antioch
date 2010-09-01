> module Antioch.DailySchedule where

> import Antioch.DateTime
> import Antioch.Score
> --import Antioch.Simulate
> import Antioch.Filters
> import Antioch.Types
> import Antioch.Utilities (rad2deg, rad2hrs, printList, overlie)
> import Antioch.Weather
> import Antioch.Debug
> import Antioch.Schedule
> import Antioch.ReceiverTemperatures
> --import Antioch.HardwareSchedule
> --import Antioch.DSSData
> import Antioch.Settings (dssDataDB)
> --import Antioch.Reports
> import Data.Time           (getCurrentTimeZone, localTimeToUTC)
> import Data.Time.LocalTime (timeZoneMinutes)
> import Control.Monad.Trans (liftIO)
> import Data.List (intercalate, sort, (\\), find)
> import Control.Monad.Writer

Daily Schedule is a "meta-strategy".  For example, if the strategy is "Pack",
then this module applies the correct filtering to the pool of sessions given,
schedules using a buffer (over-schedule by a few hours), then removes the buffer
to avoid artificial boundaries.

Daily Schedule is a specialty function used in the daily process of actually
scheduling the GBT every morning.  It is distinct from all the other scheduling
functions which are more appropriate for *simulations*.

Each day the GBT is scheduled for the next 24 - 48 hours.  The time range
begins at 8 AM ET to 8 AM ET (12:00/13:00 - 12:00/13:00 UTC) the next day.  
Therefore, the datetime passed
to this function will ignore the time element and interpret the datetime as
simply the day to begin the scheduling at 8 AM ET.
In addition, the strategy used (ex: Pack), is called directly, avoid any 
artificial boundaries (as we see with calling a strategy multiple times for
each day, as in simulations).

An additional attempt at avoiding artificial boundaries is the way in which 
the endpoint for our scheduling is determined: we run the strategy from 7 AM
EST to 7 AM EST + some overhead, then ignore the periods scheduled into the 
overhead until we have a reasonable boundary condition at the end of the 24
hour scheduling period.

> dailySchedulePack :: DateTime -> Int -> [Period] -> [Session] -> Scoring [Period]
> dailySchedulePack dt days history ss = do
>   sf <- genScore dt . scoringSessions dt $ ss
>   dailySchedule sf Pack dt days history ss False

> dailySchedule :: ScoreFunc -> StrategyName -> DateTime -> Int -> [Period] -> [Session] -> Bool -> Scoring [Period]
> dailySchedule sf strategyName dt days history ss quiet = do
>     -- figure out the time period to schedule for
>     let workStartMinutes = 8*60  -- TBF when do they get to work?
>     endTime <- liftIO $ getEndTime dt days workStartMinutes
>     let dur = endTime `diffMinutes'` dt 
>     liftIO . pr quiet $ "Daily Schedule, from " ++ (show . toSqlString $ dt) ++ " to " ++ (show . toSqlString $ endTime) ++ " (UTC)." 
>     let history' = filterHistory history dt (days + 1)
>     liftIO $ pr quiet $ "scheduling around periods: "
>     liftIO $ prl quiet $ history'
>     -- schedule with a buffer
>     schdWithBuffer <- dailySchedule' sf strategyName dt dur history' . map crop_session $ ss
>     liftIO $ pr quiet $ "scheduled w/ buffer: "
>     liftIO $ pr quiet $ show . length $ schdWithBuffer
>     liftIO $ prl quiet $ schdWithBuffer
>     -- get rid of the buffer
>     let results = removeBuffer dt dur schdWithBuffer history'
>     liftIO $ pr quiet $ "removed buffer: "
>     liftIO $ pr quiet $ show . length $ results
>     liftIO $ prl quiet $ results
>     tell [Timestamp dt] -- for trace plots
>     return results

Make a windowed session's allotted time equal to one period to prevent
scheduling of multiple periods

> crop_session :: Session -> Session
> crop_session s
>     | Windowed == sType s  = s {sAllottedT = min (sAllottedT s) (maxDuration s)}
>     | otherwise            = s

> pr :: Bool -> String -> IO ()
> pr quiet str = do
>     if quiet then return () else print str
 
> prl :: Show a => Bool -> [a] -> IO ()
> prl quiet list = do
>     if quiet then return () else printList list

Computes the scheduling period finish in UTC on the last day at the
start hour of the work day in ET.

> getEndTime :: DateTime -> Int -> Minutes -> IO Minutes
> getEndTime dt days workStart = do
>     edt <- getCurrentTimeZone
>     let endTimeMinutes = workStart - (timeZoneMinutes edt) 
>     return $ getEndTime' dt days endTimeMinutes

Actually calls the strategy (ex: Pack) for the days we are interested in, 
scheduling a 'buffer' zone, and then removing this 'buffer' to avoid 
boundary affects.

> dailySchedule' :: ScoreFunc -> StrategyName -> DateTime -> Minutes -> [Period] -> [Session] -> Scoring [Period]
> dailySchedule' sf strategyName dt dur history ss = do
>   let strategy = getStrategy strategyName 
>   schedPeriods <- strategy sf dt (dur + bufferHrs) history . schedulableSessions dt $ ss
>   return schedPeriods
>     where
>       bufferHrs = 12*60 -- length of buffer in minutes

Given the start of the scheduling period, number of days plus the
offset on the last day, returns the time in UTC of the end of the
scheduling period.

> getEndTime' :: DateTime -> Int -> Minutes -> DateTime
> getEndTime' start days endMinutes = setHour endHour lastDay
>   where
>     endHour = endMinutes `div` 60
>     daysMins = 24*60*days -- lastdays in minutes 
>     lastDay = daysMins `addMinutes'` start 

Remove those periods that are outside the given time range, if they *aren't*
part of the history of pre-scheduled periods

> removeBuffer :: DateTime -> Minutes -> [Period] -> [Period] -> [Period]
> removeBuffer dt dur ps history = filter (not . remove) ps
>   where
>     remove p = (not $ overlie dt dur p)  && (notInHistory p history)

> notInHistory :: Period -> [Period] -> Bool
> notInHistory p ps = case find (==p) ps of
>     Just _  -> False
>     Nothing -> True

Very basic function for calling Daily Schedule multiple times.
NOTE: you may want to change getWeather in dailySchedule to use the
start time if you use this function.
TBF: this also needs to be refactored to merge this with Simulations.lhs

> {-
> simDailySchedulePack :: DateTime -> Int -> Int -> IO ()
> simDailySchedulePack start packDays simDays 
>     | packDays > simDays = return ()
>     | otherwise = do 
>         dailySchedulePack start packDays
>         simDailySchedulePack (nextDay start) packDays (simDays - 1)
>   where
>     nextDay dt = addMinutes (1 * 24 * 60) dt 
> -}

Debugging Utilities:

Use these for making sure the recently created Periods can reproduce their
scores.
NOTE: currently, all period-dependent scoring factors must be ignored in order
to reproduce scores.

> scoreThesePeriods :: [Period] -> IO ()
> scoreThesePeriods     [] = return ()
> scoreThesePeriods (p:ps) = do 
>     scoreThisPeriod' p
>     scoreThesePeriods ps

> scoreThisPeriod' p = scoreThisPeriod (sName . session $ p) (startTime p)

> scoreThisPeriod :: String -> DateTime -> IO ()
> scoreThisPeriod sessName dt = do
>     -- get the session in question
>     --projs <- getProjects
>     let projs = [] -- TBF: place this all somewhere else?
>     let ss = concatMap sessions projs
>     let s = head $ filter (\s -> (sName s) == sessName) ss
>     -- now get it's period
>     let ps = periods s
>     let p = head $ filter (\p -> (startTime p) == dt) ps
>     --print $ "session : " ++ (show . session $ p)
>     w <- getWeather $ Just . pForecast $ p
>     rt <- getReceiverTemperatures
>     -- make sure we can reproduce the period's score
>     periodScore <- scorePeriod p s ss w [] rt
>     print " "
>     print $ "result: " ++ (show periodScore) ++ " vs. " ++ (show . pScore $ p)
>     print " "

