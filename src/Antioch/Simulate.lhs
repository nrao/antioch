> module Antioch.Simulate where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.Utilities (between)
> import Antioch.Weather   (Weather(..), getWeather)
> import Data.List         (find, partition)
> import Data.Maybe        (fromMaybe)
> import System.CPUTime

> simulate06 :: Strategy -> IO [Period]
> simulate06 sched = do
>     w <- getWeather Nothing
>     ps <- generateVec 400
>     let ss = zipWith (\s n -> s { sId = n }) (concatMap sessions ps) [0..]
>     print $ length ss
>     start <- getCPUTime
>     result <- simulate sched w rs dt dur int history ss
>     stop <- getCPUTime
>     putStrLn $ "Test Execution Speed: " ++ show (fromIntegral (stop-start) / 1.0e12) ++ " seconds"
>     return result
>   where
>     rs  = []
>     dt  = fromGregorian 2006 1 1 0 0 0
>     dur = 60 * 24 * 2
>     int = 60 * 24 * 1
>     history = []
  
> simulate :: Strategy -> Weather -> ReceiverSchedule -> DateTime -> Minutes -> Minutes -> [Period] -> [Session] -> IO [Period]
> simulate sched w rs dt dur int history sessions
>     | dur < int  = return []
>     | otherwise  = do
>         w' <- newWeather w $ Just (negate hint `addMinutes'` dt)
>         schedPeriods <- runScoring w' rs $ sched sf start int' history sessions
>         print "backups: "
>         print . length $ [s | s <- sessions, backup s]
>         putStrLn $ "schedPeriods: " ++ show (schedPeriods)
>         -- now see if all these new periods meet Min. Obs. Conditions         
>         obsPeriods <- runScoring w' rs $ scheduleBackups sf schedPeriods sessions
>         putStrLn $ "obsPeriods: " ++ show (obsPeriods)
>         let sessions' = updateSessions sessions obsPeriods
>         result <- simulate sched w' rs (hint `addMinutes'` dt) (dur - hint) int (reverse obsPeriods ++ history) sessions'
>         return $ obsPeriods ++ result
>   where
>     sf    = genScore sessions
>     hint  = int `div` 2
>     start = case history of
>         (h:_) -> duration h `addMinutes'` startTime h
>         _     -> dt
>     end   = int `addMinutes'` dt
>     int'  = end `diffMinutes'` start

> scheduleBackups :: ScoreFunc -> [Period] -> [Session] -> Scoring [Period]
> {-scheduleBackups sf ps ss | ps == []             = return $ ps
>                          | backupSessions == [] = return $ ps
>                          | otherwise            = return $ replacedPeriods
>   where
>     backupSessions  = [ s | s <- ss, backup s]
>     replacedPeriods = mapM (replacePeriod sf backupSessions) ps
> -}                                    
> scheduleBackups sf ps ss = mapM (scheduleBackup sf backupSessions) ps
>   where
>     backupSessions  = [ s | s <- ss, backup s]
>     

> scheduleBackup :: ScoreFunc -> [Session] -> Period -> Scoring Period 
> scheduleBackup sf ss p = do
>   moc        <- minimumObservingConditions (startTime p) (session p)
>   (s, score) <- best (averageScore sf (startTime p)) candidates
>   if fromMaybe False moc then return p else
>     if score > 0.0 
>     then return $ Period s (startTime p) (duration p) score
>     else return p
>   where
>     candidates = [s | s <- ss, between (duration p) (minDuration s) (maxDuration s)]

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
