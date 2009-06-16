> module Antioch.Simulate where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities    (between, rad2hr, showList', dt2semester)
> import Antioch.Weather      (Weather(..), getWeather)
> import Control.Monad.Writer
> import Data.List
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

> type SelectionCriteria = DateTime -> Session -> Bool

> timeLeft :: SelectionCriteria
> timeLeft dt s = (totalAvail s sem) >= (minDuration s)
>   where 
>     sem = dt2semester dt

> isTypeOpen :: SelectionCriteria
> isTypeOpen _ s = sType s == Open

We are explicitly ignoring grade here: it has been decided that a humna
should deal with closing old B projects, etc.

> isScheduableSemester :: SelectionCriteria 
> isScheduableSemester dt s = (semester $ project s) <= current_semester
>    where
>      current_semester = dt2semester dt

> filterSessions :: DateTime -> [SelectionCriteria] -> [Session] -> [Session]
> filterSessions dt []       ss = ss
> filterSessions dt (sc:scs) ss = filterSessions dt scs $ filter (sc dt) ss

TBF: this does not work properly in serveral ways:
   * each point in time should be 'scheduled' only once.  This works most of
     the time; for example, if a period is scheduled, but then canceled due 
     to MOC and not replaced w/ a backup, this 'gap' in the schedule will not
     be filled again by the scheduling strategy.  This example does not apply,
     however, when the period that is canceled, was the last (by date) to be
     scheduled.  This is because the head of the history is used to determine
     when we should start scheduling next.
   * when the 'history' contains not just periods from the past, but 
     pre-scheduled periods (ex: maintanence), it will start scheduling at the
     end of the latest history, even if that is far in the future!.
   * not all scheduling strategies even handle the 'history'!  Currently, only
     Pack seems too ...

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
>   let schedSessions = filterSessions dt [isTypeOpen, timeLeft, isScheduableSemester] sessions
>   sf <- genScore $ filterSessions dt [isScheduableSemester] sessions
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

simulateScheduling is a simplification of 'simulate', in that it does not
simulate scheduling *and* observing.  That is, it does not evaluate
scheduled periods' MOC's and attempt to find backups for cancelations.  It's
current purpose is for building a schedule for the 09B trimester.

We will take an approach as close as possible to what was done in production
for the 08B beta test: 
   * attempt to schedule a 48 hour time range
   * the first 24 hrs of this time range may have been scheduled already by
     preveious calls to simulate'
   * we will not attempt to do anything with the schedule as it is built up; 
     instead, we simply pass it along to the strategy, making it the strategy's
     responsibility for honoring pre-scheduled Periods.

This also exists because it attempts to solve the bugs involving the 'history':
that is, pre-scheduled periods that can appear any where in time.  

TBF: for the purposes of building a schedule, is this acceptable?
TBF: are we introducing an effect of cutting off periods at the simulation
boundary?  

> simulateScheduling :: StrategyName -> Weather -> ReceiverSchedule -> DateTime -> Minutes -> Minutes -> [Period] -> [Period] -> [Session] -> IO ([Period], [Trace])
> simulateScheduling sched w rs dt dur int history canceled sessions =
>     simulate' w dt dur history sessions [] []
>   where
>     simulate' w dt dur history sessions pAcc tAcc
>         | dur < int  = return (pAcc, tAcc)
>         | otherwise  = do
>             w' <- liftIO $ newWeather w $ Just dt
>             -- schedPeriods only includes those periods from the history that
>             -- are contained in or overlap (dt - (dt + int)).
>             (schedPeriods, t1) <- runScoring' w' rs $ runSimSchedStrategy sched dt int sessions history
>             -- take out the 'history' out of the result from the strategy
>             let newlyScheduledPeriods = schedPeriods \\ history
>             --liftIO $ print $ "newly scheduled periods: " ++ (show newlyScheduledPeriods)
>             let sessions' = updateSessions sessions newlyScheduledPeriods
>             liftIO $ putStrLn $ "Time: " ++ show (toGregorian' dt) ++ "\r"
>             -- This writeFile is a necessary hack to force evaluation of the pressure histories.
>             liftIO $ writeFile "/dev/null" (show t1)
>             -- We must always pass on the whole history, because Periods
>             -- not covered in the time range we are simulating would be
>             -- other wise lost.
>             -- Note: by using nub, we aren't catching geniune bugs involving 
>             -- the creation of identical Periods.
>             simulate' w' (hint `addMinutes'` dt) (dur - hint) (nub . sort $ schedPeriods ++ history) sessions' (nub . sort $ schedPeriods ++ history) $! (tAcc ++ t1)
>       where
>         -- move forward next simulation by half the sim. interval
>         hint   = int `div` 2 


The main diff between this and runSimStrategy is that we aren't simulating
observing: not checking MOC, not trying to replace cancelations w/ backups.

> runSimSchedStrategy :: StrategyName -> DateTime -> Minutes -> [Session] -> [Period] -> Scoring [Period]
> runSimSchedStrategy strategyName dt dur sessions history = do
>   tell [Timestamp dt]
>   let strategy = getStrategy strategyName 
>   let schedSessions = filterSessions dt [isTypeOpen, timeLeft, isScheduableSemester] sessions
>   sf <- genScore $ filterSessions dt [isScheduableSemester] sessions
>   schedPeriods <- strategy sf dt dur history schedSessions
>   return schedPeriods

Utilities:

> debugThisSessionHistory :: String -> [Period] -> String
> debugThisSessionHistory name ps = "Num periods in history for " ++ name ++ " : " ++ (show . length $ ps')
>   where
>     ps' = filter (\p-> (sName . session $ p) == name) ps

> debugThisSessionPeriod :: String -> [Period] -> String
> debugThisSessionPeriod name ps = report ps' 
>   where
>     ps' = filter (\p-> (sName . session $ p) == name) ps
>     report ps' = if length ps' > 0 then concatMap show ps' else "No Periods for: " ++ name

> debugThisSession :: String -> [Session] -> String
> debugThisSession name ss = report ss'
>   where
>     ss' = filter (\s-> (sName s) == name) ss
>     report ss' = if (length ss') == 1 then report' . head $ ss' else name ++ " is not present!!!!!!!!!!"
>     report' s = (sName s) ++ ": " ++ (show . totalTime $ s) ++ ", " ++ (show . totalUsed $ s) ++ ", " ++ (show $ (totalTime s) - (totalUsed s))

Scores the named session for the interval spanned.

> scoreThisSession :: String -> DateTime -> Minutes -> [Session] -> Scoring [Score] 
> scoreThisSession name dt dur ss = if (length ss') == 1 then scoreThisSession' (head ss') dt dur ss else return [] 
>   where
>     ss' = filter (\s-> (sName s) == name) ss

> scoreThisSession' :: Session -> DateTime -> Minutes -> [Session] -> Scoring [Score]
> scoreThisSession' s dt dur ss = do
>     sf <- genScore ss
>     let score' s dt = do
>         fs <- genScore ss 
>         sc <- fs dt s
>         return $ eval sc
>     scores <- mapM (score' s) times
>     return scores
>   where
>     times = [(15*q) `addMinutes'` dt | q <- [0..(dur `div` 15)]]
> 

