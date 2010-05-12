> module Antioch.Simulate where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities    (between, showList', overlie, printList)
> import Antioch.Weather      (Weather(..), getWeather)
> import Antioch.DailySchedule
> import Control.Monad.Writer
> import Data.List
> import Data.Maybe           (fromMaybe, mapMaybe, isJust, fromJust)
> import System.CPUTime


Here we leave the meta-strategy to do the work of scheduling, but inbetween,
we must do all the work that usually gets done in nell.

> simulateDailySchedule :: ReceiverSchedule -> DateTime -> Int -> Int -> [Period] -> [Session] -> Bool -> [Period] -> [Trace] -> IO ([Period], [Trace])
> simulateDailySchedule rs start packDays simDays history sessions quiet schedule trace
>     | packDays > simDays = return (schedule, trace)
>     | otherwise = do 
>         liftIO $ putStrLn $ "Time: " ++ show (toGregorian' start) ++ " " ++ (show simDays) ++ "\r"
>         w <- getWeather $ Just start
>         (newSched, newTrace) <- runScoring' w rs $ dailySchedule Pack start packDays history sessions quiet
>         -- This writeFile is a necessary hack to force evaluation of the pressure histories.
>         liftIO $ writeFile "/dev/null" (show newTrace)
>         -- newSched is a combination of the periods from history that overlap
>         -- the scheduling range, and the new periods prodcued by pack.
>         -- here's how we get the new periods:
>         -- ex: [1,2,3,4,5] \\ [1,2,3,5] -> [4]
>         let newlyScheduledPeriods' = newSched \\ history
>         -- move these periods to the scheduled state, etc. 
>         let newlyScheduledPeriods = map publishPeriod newlyScheduledPeriods'
>         -- here we need to take the periods that were created by pack
>         -- and add them to the list of periods for each session
>         -- this is necessary so that a session that has used up all it's
>         -- time is not scheduled in the next call to simulate.
>         let sessions'' = updateSessions sessions newlyScheduledPeriods
>         -- TBF: optional - simulate observing
>         simulateDailySchedule rs (nextDay start) packDays (simDays - 1) (nub . sort $ newSched ++ history) sessions'' quiet (nub . sort $ newSched ++ history) $! (trace ++ newTrace)
>   where
>     nextDay dt = addMinutes (1 * 24 * 60) dt 

This is vital for calculating pressures correctly.
TBF: once windows are introduced, here we will need to reconcile them.

> publishPeriod :: Period -> Period
> publishPeriod p = p { pState = Scheduled, pDuration = dur }
>   where
>     dur = duration p


> debugSimulation :: [Period] -> [Period] -> [Trace] -> String
> debugSimulation schdPs obsPs trace = concat [schd, obs, bcks, "\n"]
>   where
>     schd = "Scheduled: \n" ++ (showList' schdPs) ++ "\n"
>     obs = "Observed: \n" ++ (showList' obsPs) ++ "\n"
>     backups = [p | p <- obsPs, pBackup p]
>     bcks = if length backups == 0 then "" else  "Backups: \n" ++ (showList' backups) ++ "\n"

Assign the new periods to the appropriate Session.
For example, if the inputs looked like this:
sessions = 
   * Session A [(no periods)]
   * Session B [(Period for Session B)]
periods =
   * Period for Session A
   * Period for Session B

Then the result would be:
sessions =
   * Session A [(Period for Session A)]
   * Session B [(Period for Session B), (Period for Session B)]

> updateSessions :: [Session] -> [Period] -> [Session]
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
>     report' s = (sName s) ++ ": " ++ (show . sAllottedT $ s) ++ ", " ++ (show . sCommittedT $ s) ++ ", " ++ (show $ (sAllottedT s) - (sCommittedT s))

Scores the named session for the interval spanned.

> scoreThisSession :: String -> DateTime -> Minutes -> [Session] -> Scoring [Score] 
> scoreThisSession name dt dur ss = if (length ss') == 1 then scoreThisSession' (head ss') dt dur ss else return [] 
>   where
>     ss' = filter (\s-> (sName s) == name) ss

> scoreThisSession' :: Session -> DateTime -> Minutes -> [Session] -> Scoring [Score]
> scoreThisSession' s dt dur ss = do
>     sf <- genScore dt ss
>     let score' s dt = do
>         fs <- genScore dt ss 
>         sc <- fs dt s
>         return $ eval sc
>     scores <- mapM (score' s) times
>     return scores
>   where
>     times = [(15*q) `addMinutes'` dt | q <- [0..(dur `div` 15)]]
> 

