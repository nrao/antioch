This module contains methods to help the user better understand how sessions
were scored and how that scoring influenced the scheduling of the telescope.

> module Antioch.Debug where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Control.Monad.RWS.Strict

Extract the debugging info that was relevant at a specific date and time.

> findTrace          :: DateTime -> [Trace] -> [Trace]
> findTrace dt trace =
>     case takeWhile (\(ts, _) -> ts <= dt) . byTimestamp $ trace of
>         []   -> []
>         prev -> let ((_, trace') : _) = reverse prev in trace'

Organize traces by date and time.

> byTimestamp :: [Trace] -> [(DateTime, [Trace])]
> byTimestamp (Timestamp dt : trace) = byTimestamp' dt trace
>   where
>     byTimestamp' dt trace = (dt, prev) : byTimestamp' dt' next
>       where
>         (prev, (Timestamp dt' : next)) = break isTimestamp trace

Reconstruct the scoring function that was in place at a specific date and time.

> getScoring          :: DateTime -> [Trace] -> Scoring ScoreFunc
> getScoring dt trace = do
>     tell [Timestamp dt]
>     raPressure   <- genRightAscensionPressure' raFactors
>     freqPressure <- genFrequencyPressure' freqFactors
>     genScore' raPressure freqPressure
>   where
>     trace' = findTrace dt trace
>     (FreqPressureHistory freqFactors : _) = getFreqPressureHistory trace'
>     (RaPressureHistory   raFactors   : _) = getRaPressureHistory trace'

Reconstruct the factors that were used in scoring a given period.

> getFactors            :: ReceiverSchedule -> [Trace] -> Period -> IO Factors
> getFactors rs trace p = do
>     w <- getWeather (Just dt)
>     runScoring w rs $ do
>         sf <- getScoring dt trace
>         sf dt (session p)
>   where
>     dt = pForecast p

> getFreqPressureHistory :: [Trace] -> [Trace]
> getFreqPressureHistory = filter isFreqPressureHistory

> getRaPressureHistory :: [Trace] -> [Trace]
> getRaPressureHistory = filter isRaPressureHistory

> getTimestampHistory :: [Trace] -> [Trace]
> getTimestampHistory = filter isTimestamp

> isTimestamp (Timestamp _) = True
> isTimestamp _             = False

> getTimestamp (Timestamp dt) = dt

> getFreqPressure (FreqPressureHistory dt) = dt

> isFreqPressureHistory (FreqPressureHistory _) = True
> isFreqPressureHistory _                       = False

> isRaPressureHistory (RaPressureHistory _) = True
> isRaPressureHistory _                     = False

> isCancellation (Cancellation _) = True
> isCancellation _                = False

Find the total amount of unused time in the schedule.

> deadTime periods = sum $ zipWith gapBetween periods (tail periods)
>   where
>     gapBetween p1 p2 = startTime p2 `diffMinutes` (duration p1 `addMinutes` startTime p1)

Find the total amount of time given to backup projects in the schedule.

> totalBackup periods = sum [duration p | p <- periods, pBackup p]

Find the total amount of scheduled periods that were cancelled due to bad weather.

> totalCancelled trace = sum [duration p | Cancellation p <- trace]
