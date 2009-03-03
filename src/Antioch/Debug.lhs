This module contains methods to help the user better understand how sessions
were scored and how that scoring influenced the scheduling of the telescope.

> module Antioch.Debug where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather

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

> isTimestamp (Timestamp _) = True
> isTimestamp _             = False

> isFreqPressureHistory (FreqPressureHistory _) = True
> isFreqPressureHistory _                       = False

> isRaPressureHistory (RaPressureHistory _) = True
> isRaPressureHistory _                     = False
