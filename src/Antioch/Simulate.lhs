> module Antioch.Simulate where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities    (between, showList', dt2semester, overlie)
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
>         (s, t) <- runScoring' w rs $ dailySchedule Pack start packDays history sessions quiet
>         -- This writeFile is a necessary hack to force evaluation of the pressure histories.
>         liftIO $ writeFile "/dev/null" (show t)
>         -- TBF: udpate sessions
>         -- TBF: publish
>         -- TBF: optional - simulate observing
>         simulateDailySchedule rs (nextDay start) packDays (simDays - 1) (sort . nub $ history ++ s) sessions quiet (sort . nub $ schedule ++ s) (trace ++ t)
>   where
>     nextDay dt = addMinutes (1 * 24 * 60) dt 

> debugSimulation :: [Period] -> [Period] -> [Trace] -> String
> debugSimulation schdPs obsPs trace = concat [schd, obs, bcks, "\n"]
>   where
>     schd = "Scheduled: \n" ++ (showList' schdPs) ++ "\n"
>     obs = "Observed: \n" ++ (showList' obsPs) ++ "\n"
>     backups = [p | p <- obsPs, pBackup p]
>     bcks = if length backups == 0 then "" else  "Backups: \n" ++ (showList' backups) ++ "\n"

> updateSessionPeriods :: [Session] -> [Period] -> [Session]
> updateSessionPeriods ss nps = map update ss
>   where
>     -- replace all the session's original periods from new periods (nps)
>     update s = makeSession s (windows s) rps
>       where
>         -- the session's original periods
>         sps = periods s
>         -- the session's replacement periods:
>         --     originals not in nps and equivalent in nps
>         rps = (sps \\ nps) ++ (filter (\p -> elem p sps) nps)

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

