> module Antioch.Simulate where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.Utilities    (between, rad2hr, showList', dt2semester)
> import Antioch.Weather      (Weather(..), getWeather)
> import Control.Monad.Writer
> import Data.List            (find, partition, nub)
> import Data.Maybe           (fromMaybe, mapMaybe, isJust)
> import System.CPUTime

> simulate06 :: StrategyName -> IO ([Period], [Trace])
> simulate06 strategyName = do
>     w  <- liftIO $ getWeather Nothing
>     ps <- liftIO $ generateVec 400
>     let ss = zipWith (\s n -> s { sId = n }) (concatMap sessions ps) [0..]
>     liftIO $ print $ length ss
>     start  <- liftIO getCPUTime
>     result <- simulate strategyName w rs dt dur int [] [] ss
>     stop   <- liftIO getCPUTime
>     liftIO $ putStrLn $ "Test Execution Speed: " ++ show (fromIntegral (stop-start) / 1.0e12) ++ " seconds"
>     return result
>   where
>     rs  = []
>     dt  = fromGregorian 2006 1 2 0 0 0
>     dur = 60 * 24 * 30
>     int = 60 * 24 * 1
>     history = []
  
Not all sessions should be considered for scheduling.  We may not one to pass
Sessions that:
   * are disabled/unauthorized
   * have no time left (due to Periods)
   * have been marked as complete
   * more ...
TBF: only have implemented time left so far ...

> type SelectionCriteria = DateTime -> Session -> Bool

TBF: > instead of >= below is bad because Carls data is often set with
minDuration == totalTime (and totalUsed == 0).

> timeLeft :: SelectionCriteria
> timeLeft _ s     = ((totalTime s) - (totalUsed s)) > (minDuration s)

TBF: we need to be using 'isScheduableSemester', that looks at past semesters
dependeing on grade.

> isMySemester :: SelectionCriteria 
> isMySemester dt s = (semester $ project s) <= current_semester
>    where
>      current_semester = dt2semester dt

> filterSessions :: DateTime -> [SelectionCriteria] -> [Session] -> [Session]
> filterSessions dt []       ss = ss
> filterSessions dt (sc:scs) ss = filterSessions dt scs $ filter (sc dt) ss

> simulate :: StrategyName -> Weather -> ReceiverSchedule -> DateTime -> Minutes -> Minutes -> [Period] -> [Period] -> [Session] -> IO ([Period], [Trace])
> simulate sched w rs dt dur int history canceled sessions =
>     simulate' w dt dur history sessions [] []
>   where
>     simulate' w dt dur history sessions pAcc tAcc
>         | dur < int  = return (pAcc, tAcc)
>         | otherwise  = do
>             w' <- liftIO $ newWeather w $ Just dt
>             ((schedPeriods, obsPeriods), t1) <- runScoring' w' rs $ runSimStrategy sched start int' sessions history
>             --liftIO $ putStrLn $ debugSimulation schedPeriods obsPeriods t1
>             let sessions' = updateSessions sessions obsPeriods
>             liftIO $ putStrLn $ "Time: " ++ show (toGregorian' dt) ++ "\r"
>             -- This writeFile is a necessary hack to force evaluation of the pressure histories.
>             liftIO $ writeFile "/dev/null" (show t1)
>             simulate' w' (hint `addMinutes'` dt) (dur - hint) (reverse schedPeriods ++ history) sessions' (pAcc ++ obsPeriods) $! (tAcc ++ t1)
>       where
>         -- make sure we avoid an infinite loop in the case that a period of time
>         -- can't be scheduled with anyting
>         hint   = int `div` 2
>         start' = case history of
>             (h:_) -> duration h `addMinutes'` startTime h
>             _     -> dt
>         start  = max dt start' -- strategy never starts before 'now'
>         end    = int `addMinutes'` dt
>         int'   = end `diffMinutes'` start

Run the strategy to produce a schedule, then replace with backups where necessary.

> runSimStrategy :: StrategyName -> DateTime -> Minutes -> [Session] -> [Period] -> Scoring ([Period], [Period])
> runSimStrategy strategyName dt dur sessions history = do
>   tell [Timestamp dt]
>   let strategy = getStrategy strategyName 
>   let schedSessions = filterSessions dt [timeLeft, isMySemester] sessions
>   sf <- genScore $ filterSessions dt [isMySemester] sessions
>   schedPeriods <- strategy sf dt dur history schedSessions
>   obsPeriods <-  scheduleBackups strategyName sf schedSessions schedPeriods
>   return (schedPeriods, obsPeriods)

> debugSimulation :: [Period] -> [Period] -> [Trace] -> String
> debugSimulation schdPs obsPs trace = concat [schd, obs, bcks, "\n"]
>   where
>     schd = "Scheduled: \n" ++ (showList' schdPs) ++ "\n"
>     --freqs = show $ map (frequency . session) schedPeriods
>     obs = "Observed: \n" ++ (showList' obsPs) ++ "\n"
>     backups = [p | p <- obsPs, pBackup p]
>     bcks = if length backups == 0 then "" else  "Backups: \n" ++ (showList' backups) ++ "\n"

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
>     Pack ->  best (avgScoreForTime sf (startTime p) (duration p)) backups
>     ScheduleMinDuration ->  best (avgScoreForTime sf (startTime p) (duration p)) backups
>     ScheduleLittleNell ->  best (scoreForTime sf (startTime p)) backups
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
>     then return $ Just $ Period s (startTime p) (duration p) score (forecast w) True
>     else return Nothing -- no decent backups, must be bad wthr -> Deadtime

> updateSessions sessions periods = map update sessions
>   where
>     pss      = partitionWith session periods
>     update s =
>         case find (\(p:_) -> session p == s) pss of
>           Nothing -> s
>           Just ps -> updateSession s ps

> partitionWith            :: Eq b => (a -> b) -> [a] -> [[a]]
> partitionWith _ []       = []
> partitionWith f xs@(x:_) = as : partitionWith f bs
>   where
>     (as, bs) = partition (\t -> f t == f x) xs
