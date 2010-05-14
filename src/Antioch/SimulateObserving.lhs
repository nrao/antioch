> module Antioch.SimulateObserving where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities    (between, showList', dt2semester, overlie)
> import Antioch.Weather      (Weather(..), getWeather)
> import Antioch.Filters
> import Control.Monad.Writer
> import Data.List
> import Data.Maybe           (fromMaybe, mapMaybe, isJust, fromJust)
> import System.CPUTime

By simulating observing, we refer to the process of taking a simulated schedule,
simulating the detection of Min. Obs. Conditions failures, and attempts
to replace these canceled periods with backups.

> type BackupStrategy = StrategyName -> ScoreFunc -> [Session] -> Period -> Scoring (Maybe Period) 

> scheduleBackups :: StrategyName -> [Session] -> [Period] -> DateTime -> Minutes -> Scoring [Period]
> scheduleBackups sn ss ps dt dur = do
>     sf <- genScore dt . scoringSessions dt $ ss
>     let ss' = schedulableSessions dt $ ss
>     scheduleBackups' sn sf ss ps dt dur 

> scheduleBackups' :: StrategyName -> ScoreFunc -> [Session] -> [Period] -> DateTime -> Minutes -> Scoring [Period]
> scheduleBackups' _  _  _  [] _ _    = return []
> scheduleBackups' sn sf ss ps dt dur = do
>     sched' <- mapM (\p -> scheduleBackup sn sf ss p dt dur) ps
>     let sched = mapMaybe id sched'
>     return sched

> forceSeq []     = []
> forceSeq (x:xs) = x `seq` case forceSeq xs of { xs' -> x : xs' }

> findCanceledPeriods :: [Period] -> [Period] -> [Period]
> findCanceledPeriods scheduled observed = filter (isPeriodCanceled observed) scheduled

> 
> isPeriodCanceled :: [Period] -> Period -> Bool
> isPeriodCanceled ps p = not $ isJust $ find (==p) ps


What backups are even condsidered when looking for one to fill a hole due to
a cancelation may depend on the strategy being used.  For now it doesn't.

> filterBackups :: StrategyName -> [Session] -> Period -> [Session]
> filterBackups _ ss p = [ s | s <- ss, backup s, between (duration p) (minDuration s) (maxDuration s)]

If a scheduled period fails it's Minimum Observing Conditions criteria,
then try to replace it with the best backup that can (according to it's
min and max duration limits).  If no suitable backup can be found, then
schedule this as deadtime.

We only we want to be scheduling backups during the specified time range, 
and we don't want to alter periods that are in the Scheduled state.

> --scheduleBackup :: BackupStrategy
> scheduleBackup :: StrategyName -> ScoreFunc -> [Session] -> Period -> DateTime -> Minutes -> Scoring (Maybe Period) 
> --scheduleBackup sn sf ss p dt dur | pState p == Scheduled = return $ Just p
> --                                 | not $ overlie dt dur p = return $ Just p
> scheduleBackup sn sf ss p dt dur | not $ inCancelRange p dt dur = return $ Just p
>                                  | otherwise = do 
>   moc <- minimumObservingConditions (startTime p) (session p)
>   if fromMaybe False moc then return $ Just p else cancelPeriod sn sf backupSessions p
>   where
>     backupSessions  = filterBackups sn ss p 

We only want to check each period for cancelation once.  We do this by 
only checking those that are within the specified range.
For example, if we are stepping the simulation by one day, and every simulation
day we are first scheduling two days into the future, then we only want to 
check for cancelations on the present day. The next day will be checked in
the next simulation step.

Specifically, we want to exclude any period that overlaps with the start, but
*include* any period that overlaps with the end point.

> inCancelRange :: Period -> DateTime -> Minutes -> Bool
> inCancelRange p start dur | pStart >= start && pEnd < end = True
>                           | pStart < end && pEnd > end    = True
>                           | otherwise                     = False
>   where
>     end = dur `addMinutes'` start
>     pStart = startTime p
>     pEnd   = periodEndTime p


> cancelPeriod :: BackupStrategy
> cancelPeriod sn sf backups p = do
>   tell [Cancellation p]
>   liftIO $ print "canceling period: "
>   liftIO . print $ p
>   liftIO . print . show . length $ backups
>   if length backups == 0 
>     then return Nothing
>     else replaceWithBackup sn sf backups p

Find the best backup for a given period according to the strategy being used.

> findBestBackup :: StrategyName -> ScoreFunc -> [Session] -> Period -> Scoring (Session, Score)
> findBestBackup sn sf backups p =
>   case sn of
>     Pack ->  best (avgScoreForTimeRealWind sf (startTime p) (duration p)) backups
>     ScheduleMinDuration ->  best (avgScoreForTimeRealWind sf (startTime p) (duration p)) backups
>     ScheduleLittleNell ->  best (scoreForTime sf (startTime p) True) backups
>     

Find the best backup for a given period.  The backups are scored using the
best forecast and *not* rejecting zero scored quarters.  If the backup in turn
fails it's MOC, then, since it is likely all the others will as well, then 
schedule deadtime.

> replaceWithBackup :: BackupStrategy
> replaceWithBackup sn sf backups p = do
>   (s, score) <- findBestBackup sn sf backups p
>   moc        <- minimumObservingConditions (startTime p) s 
>   w <- weather
>   liftIO . print $ "found backup? " ++ (show (score > 0.0 && fromMaybe False moc))
>   let result = if score > 0.0 && fromMaybe False moc then Just $ Period 0 s (startTime p) (duration p) score Pending (forecast w) True (pDuration p) else Nothing
>   --    then Just $ Period 0 s (startTime p) (duration p) score Pending (forecast w) True (pDuration p)
>   --    else Nothing -- no decent backups, must be bad weather -> Deadtime
>   liftIO . print . show $ result 
>   return result
>   {-
>   if score > 0.0 && fromMaybe False moc
>     then return $ Just $ Period 0 s (startTime p) (duration p) score Pending (forecast w) True (pDuration p)
>     else return Nothing -- no decent backups, must be bad weather -> Deadtime
>    -}
    
