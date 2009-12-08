> module Antioch.DailySchedule where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Schedule
> import Antioch.Simulate
> import Antioch.Types
> import Antioch.Utilities (rad2deg, rad2hrs, printList)
> import Antioch.Weather
> import Antioch.Debug
> import Antioch.HardwareSchedule
> import Antioch.DSSData
> import Antioch.Settings (dssDataDB)
> import Control.Monad.Trans (liftIO)
> import Data.List (intercalate, sort, (\\), find)
> import Antioch.Reports

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

> dailySchedulePack :: DateTime -> Int -> IO ()
> dailySchedulePack dt days = dailySchedule Pack dt days

> dailySchedule :: StrategyName -> DateTime -> Int -> IO ()
> dailySchedule strategyName dt days = do
>     w <- getWeather Nothing
>     print $ "Daily Schedule, from " ++ (show . toSqlString $ dt) ++ " to " ++ (show . toSqlString $ endTime) ++ " (UTC)." 
>     -- now get all the input from the DB
>     (rs, ss, projs, history') <- dbInput dt
>     let history = filterHistory history' dt (days + 1) 
>     print "scheduling around periods: "
>     printList history
>     schdWithBuffer <- runScoring w rs $ runDailySchedule strategyName dt days history ss
>     print "scheduled w/ buffer: "
>     print . length $ schdWithBuffer
>     let results = removeBuffer dt (days*24*60) schdWithBuffer history
>     print "removed buffer: "
>     print . length $ results
>     -- new schedule to DB; only write the new periods
>     let newPeriods = results \\ history
>     print "writing new periods to DB: " 
>     printList newPeriods
>     putPeriods newPeriods
>   where
>     endTime = getEndTime dt days

Actually calls the strategy (ex: Pack) for the days we are interested in, 
scheduling a 'buffer' zone, and then removing this 'buffer' to avoid 
boundary affects.

> runDailySchedule :: StrategyName -> DateTime -> Int -> [Period] -> [Session] -> Scoring [Period]
> runDailySchedule strategyName dt days history ss = do
>   let strategy = getStrategy strategyName 
>   sf <- genScore . scoringSessions dt $ ss
>   schedPeriods <- strategy sf dt (dur + bufferHrs) history . schedulableSessions dt $ ss
>   return schedPeriods
>     where
>       -- calculate the duration from when we know it will end
>       endTime = getEndTime dt days
>       dur = endTime `diffMinutes'` dt 
>       bufferHrs = 12*60 -- length of buffer in minutes

We can't gaurantee when it will start, but we know that it must end on 
the last day, by 8 AM ET.

> getEndTime :: DateTime -> Int -> DateTime
> getEndTime start days = setHour endHour lastDay
>   where
>     endHour = 12 -- TBF: 12 UTC == 8 ET.  NOT!
>     daysMins = 24*60*days -- lastdays in minutes 
>     lastDay = daysMins `addMinutes'` start 

Remove those periods that are outside the given time range, if they *aren't*
part of the history of pre-scheduled periods

> removeBuffer :: DateTime -> Minutes -> [Period] -> [Period] -> [Period]
> removeBuffer dt dur ps history = filter (not . remove) ps
>   where
>     remove p = (not $ overlap dt dur p)  && (notInHistory p history)

> notInHistory :: Period -> [Period] -> Bool
> notInHistory p ps = case find (==p) ps of
>     Just _  -> False
>     Nothing -> True

> overlap :: DateTime -> Int -> Period -> Bool
> overlap start dur p = s1 < e2 &&  s2 < e1
>   where
>     s1 = startTime p
>     e1 = periodEndTime p
>     s2 = start
>     e2 = dur `addMinutes` start  

