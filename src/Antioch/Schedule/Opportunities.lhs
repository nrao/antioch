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
>     | otherwise             = rad2hr cosha
>   where
>     denominator = cos gbtLat * cos (dec session)
>     cosha       = (cos (deg2rad 85.0) - sin gbtLat * sin (dec session)) / denominator
