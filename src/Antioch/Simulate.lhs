> module Antioch.Simulate where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Score
> import Antioch.Types
> import Antioch.Statistics
> import Antioch.TimeAccounting
> import Antioch.Utilities    (showList', printList, dt2semester, periodInWindow)
> import Antioch.Weather      (Weather(..), getWeather, getWeatherTest)
> import Antioch.Schedule
> import Antioch.DailySchedule
> import Antioch.SimulateObserving
> import Antioch.Filters
> import Antioch.Debug
> import Antioch.ReceiverTemperatures
> import Control.Monad.Writer
> import Control.Exception as Ex    (assert)
> import Data.List
> import Data.Maybe
> import System.CPUTime
> import Test.HUnit


Here we leave the meta-strategy to do the work of scheduling, but inbetween,
we must do all the work that usually gets done in nell.

> simulateDailySchedule :: ReceiverSchedule -> DateTime -> Int -> Int -> [Period] -> [Session] -> Bool -> Bool -> [Period] -> [Trace] -> IO ([Period], [Trace])
> simulateDailySchedule rs start packDays simDays history sessions quiet test schedule trace
>     | packDays > simDays = return (schedule, trace)
>     | otherwise = do 
>         liftIO $ putStrLn $ "Time: " ++ show (toGregorian' start) ++ " " ++ (show simDays) ++ "\r"
>         -- you MUST create the weather here, so that each iteration of 
>         -- the simulation has a new date for the weather origin - this
>         -- makes sure that the forecast types will be correct.
>         w <- if test then getWeatherTest $ Just start else getWeather $ Just start
>         rt <- getReceiverTemperatures
>         -- make sure sessions from future semesters are unauthorized
>         let sessions' = authorizeBySemester sessions start
>
>         -- now we pack, and look for backups
>         (newSchedPending, newTrace) <- runScoring' w rs rt $ do
>             -- it's important that we generate the score only once per
>             -- simulation step; otherwise we screw up the trace that
>             -- the plots depend on
>             sf <- genScore start . scoringSessions start $ sessions'
>             -- acutally schedule!!!
>             newSched' <- dailySchedule sf Pack start packDays history sessions' quiet
>             -- simulate observing
>             newSched'' <- scheduleBackups sf Pack sessions newSched' start (24 * 60 * 1)
>             let newSched''' = map publishPeriod newSched''
>             let newlyScheduledPeriods = newSched''' \\ history
>             let www = p_ps_2_trace . p_ps . p_ws . wps $ newlyScheduledPeriods
>             --let www' = map (\(p, mw) -> (fromJust mw, Nothing, p)) www
>             liftIO $ print $ "tell me!"
>             liftIO $ print $ www
>             mapM (\w -> tell [WindowPeriods w]) www
>             return $ newSched''
>
>         -- This writeFile is a necessary hack to force evaluation
>         -- of the pressure histories.
>         liftIO $ writeFile "/dev/null" (show newTrace)
>         -- publishing the periods is important for pressures, etc.
>         let newSched = map publishPeriod newSchedPending
>         -- newSched is a combination of the periods from history that overlap
>         -- the scheduling range, and the new periods prodcued by pack.
>         -- here's how we get the new periods:
>         -- ex: [1,2,3,4,5] \\ [1,2,3,5] -> [4]
>         let newlyScheduledPeriods = newSched \\ history
>         -- now get the canceled periods so we can make sure they aren't 
>         -- still in their sessions
>         let cs = getCanceledPeriods $ trace ++ newTrace 
>         -- find which default periods need to be deleted and
>         -- which windows got scheduled
>         let (wps, ws) = findScheduledWindowPeriods newlyScheduledPeriods
>         liftIO $ print $ "Scheduling windows: "
>         liftIO $ print (wps, ws)
>         -- here we need to take the periods that were created by pack
>         -- and add them to the list of periods for each session
>         -- this is necessary so that a session that has used up all it's
>         -- time is not scheduled in the next call to simulate.
>         let sessions'' = updateSessions sessions' newlyScheduledPeriods (cs ++ wps) ws
>         -- updating the history to be passed to the next sim. iteration
>         -- is actually non-trivial
>         let newHistory = updateHistory history newSched (cs ++ wps) 
>         -- run the below assert if you have doubts about bookeeping
>         -- make sure canceled periods have been removed from sessons
>         --let sessPeriods = concatMap periods sessions''
>         --let results = all (==True) $ map (\canceled -> (elem canceled sessPeriods) == False) cs
>         --assert results
>         -- move on to the next day in the simulation!
>         simulateDailySchedule rs (nextDay start) packDays (simDays - 1) newHistory sessions'' quiet test newHistory $! (trace ++ newTrace)
>   where
>     nextDay dt = addMinutes (1 * 24 * 60) dt 
>     wps = filter (\p -> Windowed == (sType . session $ p))
>     p_ws = map (\p -> (p, find (periodInWindow p) . windows . session $ p))
>     p_ps = map (\(p, w) -> (p, filter (flip periodInWindow (fromJust w)) (periods . session $ p), w))

Given a list newly scheduled periods, find which -- if any -- belong
to windowed sessions and return lists of resulting replaced periods
and satisfied windows.

> p_ps_2_trace :: [(Period, [Period], Maybe Window)] -> [(Window, Maybe Period, Period)]
> p_ps_2_trace xs = map p_ps_2_trace' xs

> p_ps_2_trace' :: (Period, [Period], Maybe Window) -> (Window, Maybe Period, Period)
> p_ps_2_trace' (p1 , (p2:[]), mw) | p1 == p2 = (fromJust mw, Nothing, p1)
>                                  | otherwise = (fromJust mw, Just p1, p2)

> findScheduledWindowPeriods :: [Period] -> ([Period], [Window])
> --findScheduledWindowPeriods ps = rps . p_ps . p_ws . wps $ ps
> findScheduledWindowPeriods ps = rps . isLegal . p_ps . p_ws . wps $ ps
>   where
>     -- periods from a windowed session:
>     -- wps ::[Period] -> [Period]
>     wps = filter (\p -> Windowed == (sType . session $ p))
>
>     -- periods with their associated window:
>     -- p_ws :: [Period] -> [(Period, Maybe Window)]
>     p_ws = map (\p -> (p, find (periodInWindow p) . windows . session $ p))
>
>     -- periods with all periods in their window:
>     --    [(Period, Maybe Window)] -> [(Period, [Period], Maybe Window)]
>     p_ps = map (\(p, w) -> (p, filter (flip periodInWindow (fromJust w)) (periods . session $ p), w))
>
>     -- asserts to find pathologial cases
>     isLegal = p_ps''' . p_ps'' . p_ps'
>     p_ps' =   (\x -> Ex.assert (oneDefault x) x)
>     p_ps''  = (\x -> Ex.assert (defaultNeqChosen x) x)
>     p_ps''' = (\x -> Ex.assert (chosenLtDefault x) x)
>
>     -- replaced periods and all windows
>     -- rps :: [(Period, [Period], Maybe Window)] -> ([Period], [Window])
>     rps ppws = (ps', ws')
>       where
>         (_, ps, ws) = unzip3 ppws
>         ps' = map head ps
>         --    TBF wHasChose protects against rescheduling, but does not
>         --        denote that the default period was replaced
>         ws' = map (\w -> w {wHasChosen = True}) $ map fromJust ws

> --   period list length == 1
> oneDefault, defaultNeqChosen, chosenLtDefault :: [(Period, [Period], Maybe Window)] -> Bool
> oneDefault = all (\(p, ps, _) -> length ps == 1)
> --   chosen period not in period list
> defaultNeqChosen = all (\(p, ps, _) -> not . elem p $ ps)
> --   chosen period before period in list
> chosenLtDefault = all (\(p, ps, _) -> head ps > p)

This is vital for calculating pressures correctly.
TBF: once windows are introduced, here we will need to reconcile them.

> publishPeriod :: Period -> Period
> publishPeriod p = p { pState = Scheduled, pDuration = dur }
>   where
>     dur = duration p

During simulations, we want to be realistic about sessions from projects
from future trimesters.  So, we will simply unauthorize any sessions beloning
to future trimesters.

> authorizeBySemester :: [Session] -> DateTime -> [Session]
> authorizeBySemester ss dt = map (authorizeBySemester' dt) ss

> authorizeBySemester' dt s = s { authorized = a }
>   where
>     a = (semester . project $ s) <= currentSemester 
>     currentSemester = dt2semester dt

We must combine the output of the scheduling algorithm with the history from
before the algorithm was called, but there's two complications:
   * the output from the algo. is a combination of parts of the history and
     the newly scheduled periods
   * this same output is then modified: cancellations and replacements
     (backups) may occur.
So, we need to intelligently combine the previous history and the algo. output.
What we do is we simply combine the history and the newly scheduled periods, 
remove any redundancies (periods that were in both lists), then remove any 
periods that we know just got canceled.

> updateHistory :: [Period] -> [Period] -> [Period] -> [Period]
> updateHistory history newSched condemned = filter notCondemned $ nub . sort $ history ++ newSched 
>   where
>     notCondemned p = not $ any (==p) condemned

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

> updateSessions :: [Session] -> [Period] -> [Period] -> [Window] -> [Session]
> updateSessions sessions periods condemned windows = map update sessions
>   where
>     pss      = partitionWith session periods
>     update s =
>         case find (\(p:_) -> session p == s) pss of
>           Nothing -> updateSession' s [] condemned windows -- condemned go anyways
>           Just ps -> updateSession' s ps condemned windows

> partitionWith            :: Eq b => (a -> b) -> [a] -> [[a]]
> partitionWith _ []       = []
> partitionWith f xs@(x:_) = as : partitionWith f bs
>   where
>     (as, bs) = partition (\t -> f t == f x) xs

Ties the knots between a session and it's periods & windows. 
But it also:
   * removes condemend periods
   * updates any scheduled windows

> updateSession' :: Session -> [Period] -> [Period] -> [Window] -> Session
> updateSession' s ps canceled ws = makeSession s ws' $ sort $ (removeCanceled s canceled) ++ ps
>   where
>     -- any windows that belong to this session need to be marked as 
>     -- scheduled
>     sessSchedWins = filter (\w -> (wSession w) == s) ws
>     sessNonSchedWins = filter (\w -> not . elem w $ sessSchedWins) $ windows s
>     ws' = sort $ sessSchedWins ++ sessNonSchedWins

> removeCanceled :: Session -> [Period] -> [Period]
> removeCanceled s canceled =  (periods s) \\ canceled

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

