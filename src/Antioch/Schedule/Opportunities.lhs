> module Antioch.Schedule.Opportunities (genOpportunities) where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities

> minutesPerDay = 60 * 24
> secondsPerDay = fromIntegral $ 60 * minutesPerDay

> genOpportunities :: Session -> DateTime -> Minutes -> [(DateTime, Minutes)]
> genOpportunities session start duration
>     | duration <= 0 = []
>     | otherwise     =
>         genOppts session start' duration' ++
>         genOpportunities session (minutesPerDay `addMinutes` start) (duration - minutesPerDay)
>   where
>     end       = duration `addMinutes` start
>     transit   = lst2utc start $ ra session
>     ha        = hourAngle session
>     ha'       = floor $ ha * secondsPerDay
>     start'    = max start $ -ha' `addSeconds` transit
>     end'      = min end $ ha' `addSeconds` transit
>     duration' = end' `diffMinutes` start'

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

> hourAngle :: Session -> Float
> hourAngle session
>     | denominator <  1.0e-3 = 12.0
>     | cosha       >= 1.0    = 12.0
>     | otherwise             = rad2hr cosha
>   where
>     denominator = cos gbtLat * cos (dec session)
>     cosha       = (cos (deg2rad 85.0) - sin gbtLat * sin (dec session)) / denominator
