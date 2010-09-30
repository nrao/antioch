This module contains methods to help the user better understand how sessions
were scored and how that scoring influenced the scheduling of the telescope.

TBF, WTF: these functions are verbose and need to be consistent!

> module Antioch.Debug where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Utilities (rad2hrs)
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

> {-
> getScoring          :: DateTime -> [Trace] -> Scoring ScoreFunc
> getScoring dt trace = do
>     tell [Timestamp dt]
>     let accessor s = (round . rad2hrs . ra $ s) `mod` 24
>     raPressure   <- genRightAscensionPressure' accessor raFactors
>     freqPressure <- genFrequencyPressure' freqFactors
>     genScore' raPressure freqPressure
>   where
>     trace' = findTrace dt trace
>     (FreqPressureHistory freqFactors : _) = getFreqPressureHistory trace'
>     (RaPressureHistory   raFactors   : _) = getRaPressureHistory trace'
> -}

Reconstruct the factors that were used in scoring a given period.

> {-
> getFactors            :: ReceiverSchedule -> [Trace] -> Period -> IO Factors
> getFactors rs trace p = do
>     w <- getWeather (Just dt)
>     runScoring w rs $ do
>         sf <- getScoring dt trace
>         sf dt (session p)
>   where
>     dt = pForecast p
> -}

> getCanceledPeriods :: [Trace] -> [Period]
> getCanceledPeriods trace = canceled
>   where
>     canceled' = getCancellationHistory trace
>     canceled  = [getCancellation c | c <- canceled']

> getFreqPressureHistory :: [Trace] -> [Trace]
> getFreqPressureHistory = filter isFreqPressureHistory

> getFreqPressureBinHistory :: [Trace] -> [Trace]
> getFreqPressureBinHistory = filter isFreqPressureBinHistory

> getRaPressureHistory :: [Trace] -> [Trace]
> getRaPressureHistory = filter isRaPressureHistory

> getTimestampHistory :: [Trace] -> [Trace]
> getTimestampHistory = filter isTimestamp

> getCancellationHistory :: [Trace] -> [Trace]
> getCancellationHistory = filter isCancellation

> getWindowPeriodsHistory :: [Trace] -> [Trace]
> getWindowPeriodsHistory = filter isWindowPeriods

> getWindowPeriodsFromTrace :: [Trace] -> [(Window, Maybe Period, Period)]
> getWindowPeriodsFromTrace trace = whs
>   where
>     whs' = getWindowPeriodsHistory trace
>     whs  = [getWindowPeriods w | w <- whs']

TBF: add types sometime ...

> getCancellation (Cancellation period) = period

> getTimestamp (Timestamp dt) = dt

> getFreqPressure (FreqPressureHistory fp) = fp

> getFreqPressureBin (FreqPressureBinHistory fpb) = fpb

> getRaPressure (RaPressureHistory rp) = rp

> getWindowPeriods (WindowPeriods wh) = wh

> isTimestamp (Timestamp _) = True
> isTimestamp _             = False

> isFreqPressureHistory (FreqPressureHistory _) = True
> isFreqPressureHistory _                       = False

> isFreqPressureBinHistory (FreqPressureBinHistory _) = True
> isFreqPressureBinHistory _                       = False

> isRaPressureHistory (RaPressureHistory _) = True
> isRaPressureHistory _                     = False

> isCancellation (Cancellation _) = True
> isCancellation _                = False

> isWindowPeriods :: Trace -> Bool
> isWindowPeriods (WindowPeriods _) = True
> isWindowPeriods _                 = False


Find the total amount of unused time in the schedule.

> deadTime periods = sum $ zipWith gapBetween periods (tail periods)
>   where
>     gapBetween p1 p2 = startTime p2 `diffMinutes` (duration p1 `addMinutes` startTime p1)

Find the total amount of time given to backup projects in the schedule.

> totalBackup periods = sum [duration p | p <- periods, pBackup p]

Find the total amount of scheduled periods that were cancelled due to bad weather.

> totalCancelled trace = sum [duration p | Cancellation p <- trace]
