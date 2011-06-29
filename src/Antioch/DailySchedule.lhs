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
> import Data.Time           (getTimeZone, localTimeToUTC)
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
the endpoint for our scheduling is determined: we run the strategy from 8 AM
EST to 8 AM EST + some overhead, then ignore the periods scheduled into the 
overhead until we have a reasonable boundary condition at the end of the 24
hour scheduling period.

> dailySchedulePack :: DateTime -> Int -> [Period] -> [Session] -> Scoring [Period]
> dailySchedulePack dt days history ss = do
>   sf <- genScore dt . scoringSessions dt undefined $ ss
>   dailySchedule sf Pack dt days history ss False

> dailySchedule :: ScoreFunc -> StrategyName -> DateTime -> Int -> [Period] -> [Session] -> Bool -> Scoring [Period]
> dailySchedule sf strategyName dt days history ss quiet = do
>     -- Figure out the time period to schedule for.
>     let workStartMinutes = 8*60  
>     endTime <- liftIO $ getEndTime dt days workStartMinutes
>     let dur = endTime `diffMinutes` dt 
>     liftIO . pr quiet $ "Daily Schedule, from " ++ (show . toSqlString $ dt) ++ " to " ++ (show . toSqlString $ endTime) ++ " (UTC)." 
>     let history' = truncateHistory history dt (days + 1)
>     liftIO $ pr quiet $ "scheduling around periods: "
>     liftIO $ prl quiet $ history'
>     -- schedule with a buffer
>     schdWithBuffer <- dailySchedule' sf strategyName dt dur history' ss
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

Convenience funtions for printing stuff.

> pr :: Bool -> String -> IO ()
> pr quiet str = do
>     if quiet then return () else print str
 
> prl :: Show a => Bool -> [a] -> IO ()
> prl quiet list = do
>     if quiet then return () else printList list

Computes the scheduling period finish in UTC on the last day at the
start hour of the work day in ET.

> getEndTime :: DateTime -> Int -> Minutes -> IO DateTime
> getEndTime start days workStart = do
>     edt <- getTimeZone . fromSeconds . addMinutes pastDstMinutes $ lastDay
>     let endTimeMinutes = workStart - (timeZoneMinutes edt) 
>     return $ getEndTime' lastDay endTimeMinutes
>   where
>     daysMinutes    = 24*60*days -- lastdays in minutes 
>     lastDay        = daysMinutes `addMinutes` start
>     pastDstMinutes = 8*60 -- DST takes place at about 06:00:00 UT

Given the end of the scheduling period and the offset on the last day,
returns the time in UTC of the end of the scheduling period.

> getEndTime' :: DateTime -> Minutes -> DateTime
> getEndTime' lastDay endMinutes = setHour endHour lastDay
>   where
>     endHour = endMinutes `div` 60

Actually calls the strategy (ex: Pack) for the days we are interested in, 
including the 'buffer' that will be removed in dailySchedule.
Also of importance here is the filtering of the pool of sessions, and
the necessary adjustments to windowed sessions.

> dailySchedule' :: ScoreFunc -> StrategyName -> DateTime -> Minutes -> [Period] -> [Session] -> Scoring [Period]
> dailySchedule' sf strategyName dt dur history ss = do
>   let strategy = getStrategy strategyName
>   schedPeriods <- strategy sf dt (dur + bufferHrs) history ss' 
>   return schedPeriods
>     where
>       bufferHrs = 12*60 -- length of buffer in minutes
>       ss' = map (adjustWindowSessionDuration dt dur) $ schedulableSessions dt dur $ ss

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



