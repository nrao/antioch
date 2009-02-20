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
> import Data.Maybe           (fromMaybe)
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
>         --liftIO $ putStrLn $ "starting schedule at: " ++ (toSqlString start)
>         schedPeriods <- runScoring'' w' rs $ sched sf start int' history schedSessions
>         --liftIO $ putStrLn $ "schedPeriods: " ++ show (schedPeriods)
>         -- now see if all these new periods meet Min. Obs. Conditions         
>         obsPeriods <- runScoring'' w' rs $ scheduleBackups sf schedPeriods schedSessions
>         --liftIO $ putStrLn $ "obsPeriods: " ++ show (obsPeriods)
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

> scheduleBackups :: ScoreFunc -> [Period] -> [Session] -> Scoring [Period]
> scheduleBackups _  [] _  = return []
> scheduleBackups _  ps [] = return ps
> scheduleBackups sf ps ss = case backupSessions of
>       [] -> return ps -- no backups?  then don't change the schedule!
>       _  -> mapM (scheduleBackup sf backupSessions) ps
>   where
>     backupSessions  = [ s | s <- ss, backup s]
>     

Tries to schedule a backup for the given period, but gaurds against there
being no backups that will fit in this period's duration
TBF: if there is no backup candidate for this bad period, should we really
be keeping the bad period?  Seems like that should be telescope dead time

> scheduleBackup :: ScoreFunc -> [Session] -> Period -> Scoring Period
> scheduleBackup sf ss p = case length candidates of
>   0 -> return p -- no backups that fit here? then use orig. bad period!
>   _ -> scheduleBackup' sf candidates p 
>   where
>     candidates = [s | s <- ss, between (duration p) (minDuration s) (maxDuration s)]

Tries to schedule a backup for the given period, assuming there are candidates

> scheduleBackup' :: ScoreFunc -> [Session] -> Period -> Scoring Period 
> scheduleBackup' sf candidates p = do
>   moc        <- minimumObservingConditions (startTime p) (session p)
>   (s, score) <- best (averageScore sf (startTime p)) candidates
>   if fromMaybe False moc then return p else
>     if score > 0.0 
>     then return $ Period s (startTime p) (duration p) score
>     else return p

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
