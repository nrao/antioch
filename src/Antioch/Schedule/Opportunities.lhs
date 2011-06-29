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

> module Antioch.Schedule.Opportunities (genOpportunities) where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.Weather
> import Database.HDBC
> import Database.HDBC.PostgreSQL

> minutesPerDay = 60 * 24
> secondsPerDay = fromIntegral $ 60 * minutesPerDay

> genOpportunities :: Connection -> Session -> DateTime -> Minutes -> IO [(DateTime, Minutes)]
> genOpportunities cnn session start duration
>     | duration <= 0 = return []
>     | otherwise     = do
>         ha   <- lookupHourAngle cnn session
>         let ha'    = floor $ ha * secondsPerDay
>         let start' = max start $ -ha' `addSeconds` transit
>         let end'   = min end $ ha' `addSeconds` transit
>         let duration' = end' `diffMinutes` start'
>         rest <- genOpportunities cnn session (minutesPerDay `addMinutes` start) (duration - minutesPerDay)
>         return $ genOppts session start' duration' ++ rest
>   where
>     end     = duration `addMinutes` start
>     transit = lst2utc start $ ra session

> genOppts :: Session -> DateTime -> Minutes -> [(DateTime, Minutes)]
> genOppts session start duration =
>     [(st, dur) | st <- startTimes, dur <- [minDur, minDur+15 .. end `diffMinutes` st]]
>   where
>     end    = duration `addMinutes` start
>     minDur = minDuration session
>     startTimes = map (`addMinutes` start) [0, 15 .. duration - minDur]

> lst2utc :: DateTime -> Float -> DateTime
> lst2utc utc lst
>     | lst < now = lst2utc utc $ lst + solarDay
>     | otherwise = floor ((lst - now) * secondsPerDay) `addSeconds` utc
>   where
>     now      = utc2lstHours utc
>     solarDay = 24.0 * (365.25 / 366.25)

> lookupHourAngle :: Connection -> Session -> IO Float
> lookupHourAngle cnn session = do
>     result <- quickQuery' cnn query [toSql freqIdx, toSql decIdx]
>     return $ case result of
>       [[x]] -> fromSql x
>       _     -> hourAngle session
>   where
>     query   = "SELECT boundary FROM hour_angle_boundaries WHERE frequency = ? AND declination = ?"
>     freqIdx = freq2Index . frequency $ session
>     decIdx  = elev2Index . dec $ session

> hourAngle :: Session -> Float
> hourAngle session
>     | denominator <  1.0e-3 = 12.0
>     | cosha       >= 1.0    = 12.0
>     | otherwise             = rad2hrs cosha
>   where
>     denominator = cos gbtLat * cos (dec session)
>     cosha       = (cos (deg2rad 85.0) - sin gbtLat * sin (dec session)) / denominator
