> module Antioch.SimulateObserving where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities    (between, showList', dt2semester, overlie)
> import Antioch.Weather      (Weather(..), getWeather)
> import Control.Monad.Writer
> import Data.List
> import Data.Maybe           (fromMaybe, mapMaybe, isJust, fromJust)
> import System.CPUTime

By simulating observing, we refer to the process of taking a simulated schedule,
simulating the detection of Min. Obs. Conditions failures, and attempts
to replace these canceled periods with backups.

> forceSeq []     = []
> forceSeq (x:xs) = x `seq` case forceSeq xs of { xs' -> x : xs' }

> findCanceledPeriods :: [Period] -> [Period] -> [Period]
> findCanceledPeriods scheduled observed = filter (isPeriodCanceled observed) scheduled

> 
> isPeriodCanceled :: [Period] -> Period -> Bool
> isPeriodCanceled ps p = not $ isJust $ find (==p) ps

Replace any badly performing periods with either backups or deadtime.

> type BackupStrategy = StrategyName -> ScoreFunc -> [Session] -> Period -> Scoring (Maybe Period) 

> scheduleBackups :: StrategyName -> ScoreFunc -> [Session] -> [Period] -> Scoring [Period]
> scheduleBackups _  _  _  [] = return []
> scheduleBackups sn sf ss ps = do
>     sched' <- mapM (scheduleBackup sn sf ss) ps
>     let sched = mapMaybe id sched'
>     return sched

What backups are even condsidered when looking for one to fill a hole due to
a cancelation may depend on the strategy being used.  For now it doesn't.

> filterBackups :: StrategyName -> [Session] -> Period -> [Session]
> filterBackups _ ss p = [ s | s <- ss, backup s, between (duration p) (minDuration s) (maxDuration s)]

If a scheduled period fails it's Minimum Observing Conditions criteria,
then try to replace it with the best backup that can (according to it's
min and max duration limits).  If no suitable backup can be found, then
schedule this as deadtime.

> scheduleBackup :: BackupStrategy
> scheduleBackup sn sf ss p = do 
>   moc <- minimumObservingConditions (startTime p) (session p)
>   if fromMaybe False moc then return $ Just p else cancelPeriod sn sf backupSessions p
>   where
>     backupSessions  = filterBackups sn ss p 

> cancelPeriod :: BackupStrategy
> cancelPeriod sn sf backups p = do
>   tell [Cancellation p]
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
>   if score > 0.0 && fromMaybe False moc
>     then return $ Just $ Period 0 s (startTime p) (duration p) score Pending (forecast w) True (pTimeBilled p)
>     else return Nothing -- no decent backups, must be bad weather -> Deadtime
    
