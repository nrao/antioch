> module Antioch.DateTime where

> import Data.Time.Calendar
> import Data.Time.Clock
> import Data.Time.LocalTime

> type DateTime = UTCTime

TBF: Needs seconds!!!?

> dateTime2MJD    :: DateTime -> Float
> dateTime2MJD dt = let
>     dt' = utcToLocalTime utc dt
>     ModifiedJulianDay day = localDay dt'
>     TimeOfDay { todHour = hr, todMin = min, todSec = sec } = localTimeOfDay dt'
>     in fromIntegral day + (3600.0 * fromIntegral hr + 60.0 * fromIntegral min) / 86400.0
