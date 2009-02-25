> module Antioch.Simulate where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.Utilities    (between, rad2hr)
> import Antioch.Weather      (Weather(..), getWeather)
> import Control.Monad.Writer
> import Data.List            (find, partition)
> import Data.Maybe           (fromMaybe, mapMaybe, isJust)
> import System.CPUTime

> simulate06 :: Strategy -> IO [Period]
> simulate06 sched = do
>     w  <- liftIO $ getWeather Nothing
>     ps <- liftIO $ generateVec 400
>     let ss = zipWith (\s n -> s { sId = n }) (concatMap sessions ps) [0..]
>     liftIO $ print $ length ss
>     start  <- liftIO getCPUTime
>     result <- simulate sched w rs dt dur int history ss
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

> filterSessions :: [Session] -> [Session]
> filterSessions ss = filter timeLeft ss
>   where
>     timeLeft s = ((totalTime s) - (totalUsed s)) > (minDuration s) 

> simulate :: Strategy -> Weather -> ReceiverSchedule -> DateTime -> Minutes -> Minutes -> [Period] -> [Session] -> IO [Period]
> simulate sched w rs dt dur int history sessions
>     | dur < int  = return []
>     | otherwise  = do
>         w' <- liftIO $ newWeather w $ Just (negate hint `addMinutes'` dt)
>         sf <- genScore sessions
>         let schedSessions = filterSessions sessions
>         --liftIO $ putStrLn $ "numSess before &  after filter: " ++ (show . length $ sessions) ++ ", " ++ (show . length $ schedSessions)
>         --liftIO $ putStrLn $ "num backups: " ++ show (length [s | s <- schedSessions, backup s])
>         --liftIO $ putStrLn $ "starting schedule at: " ++ (toSqlString start)
>         schedPeriods <- runScoring'' w' rs $ sched sf start int' history schedSessions
>         --liftIO $ putStrLn $ "schedPeriods: " ++ show (schedPeriods)
>         -- now see if all these new periods meet Min. Obs. Conditions         
>         obsPeriods <- runScoring'' w' rs $ scheduleBackups sf schedPeriods schedSessions
>         --liftIO $ putStrLn $ "obsPeriods: " ++ show (obsPeriods)
>         --let canceled = findCanceledPeriods schedPeriods obsPeriods
>         --liftIO $ putStrLn $ "canceled periods: " ++ show (canceled)
>         let sessions' = updateSessions sessions obsPeriods
>         liftIO $ putStrLn $ "Time: " ++ show (toGregorian' dt) ++ "\r"
>         result <- simulate sched w' rs (hint `addMinutes'` dt) (dur - hint) int (reverse obsPeriods ++ history) sessions' 
>         return $ obsPeriods ++ result
>   where
>     -- make sure we avoid an infinite loop in the case that a period of time
>     -- can't be scheduled with anyting
>     hint   = int `div` 2
>     start' = case history of
>         (h:_) -> duration h `addMinutes'` startTime h
>         _     -> dt
>     start  = max (negate hint `addMinutes'` dt) start'
>     end    = int `addMinutes'` dt
>     int'   = end `diffMinutes'` start

> findCanceledPeriods :: [Period] -> [Period] -> [Period]
> findCanceledPeriods scheduled observed = filter (isPeriodCanceled observed) scheduled

> 
> isPeriodCanceled :: [Period] -> Period -> Bool
> isPeriodCanceled ps p = not $ isJust $ find (==p) ps

Replace any badly performing periods with either backups or deadtime.

> scheduleBackups :: ScoreFunc -> [Period] -> [Session] -> Scoring [Period]
> scheduleBackups _  [] _  = return []
> scheduleBackups sf ps ss = do
>     sched' <- mapM (scheduleBackup sf ss) ps
>     let sched = mapMaybe id sched'
>     return sched

If a scheduled period fails it's Minimum Observing Conditions criteria,
then try to replace it with the best backup that can (according to it's
min and max duration limits).  If no suitable backup can be found, then
schedule this as deadtime.

> scheduleBackup :: ScoreFunc -> [Session] -> Period -> Scoring (Maybe Period)
> scheduleBackup sf ss p = do 
>   moc <- minimumObservingConditions (startTime p) (session p)
>   if fromMaybe False moc then return $ Just p else
>     if length backupSessions == 0
>     then return Nothing -- no appropriate backups -> Deadtime!
>     else replaceWithBackup sf backupSessions p
>   where
>     backupSessions  = [ s | s <- ss, backup s, between (duration p) (minDuration s) (maxDuration s)]

Find the best backup for a given period - if the backup in turn fails it's
MOC, then, since it is likely all the others will as well, then schedule
deadtime.

> replaceWithBackup :: ScoreFunc -> [Session] -> Period -> Scoring (Maybe Period) 
> replaceWithBackup sf backups p = do
>   (s, score) <- best (averageScore sf (startTime p)) backups
>   if score > 0.0
>     then return $ Just $ Period s (startTime p) (duration p) score
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
