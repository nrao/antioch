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


Here we leave the meta-strategy (dailySchedule, which is also used for the daily, production scheduling)
to do the work of scheduling, but inbetween, we must do all the work that usually gets done in nell.

What makes this function so complicated is that it is iterative, and between each iteration, we must 
update the parameters.  At first, this just involved the newly scheduled periods, but now we must also
keep track of canceled periods and reconciled windows.  Here's a brief outline:

   * prepare for scheduling:
      * get weather and receiver temparture resources
      * make sure sessions from future semesters are un-authorized
      * make sure windows in the range of this iteration get scheduled
   * schedule 
      * this is the same call to dailySchedule that we do in daily production scheduling.   
      * here we are scheduling a 2.5 day time range 
   * backups 
      * look for periods that have failing MOC's in the next day 
      * we only cover a day because we our step size in the iterations is one day - we want to make sure we check each periods' MOC only once.
      * if MOC fails, replace it with a backup if necessary
   * prepare for next iteration:
      * publish all newly scheduled periods
      * mark reconcile windows - find which default periods need to get deleted
         * ws that were scheduled with a chosen period must make sure their default periods are deleted
      * update parameters
         * update sessions
            * each sessions' list of periods must get the newly scheduled ones added, and the canceled ones removed
            * each sessions' list of windows must be marked as scheduled (when appropriate)
         * udpate history (the schedule)
            * all newly scheduled periods added, and canceled ones removed
   * call next iteration with updated parameters - go back to the top!

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
>         let sessions'' = authorizeBySemester sessions start
>
>         -- make sure default periods that are in this scheduling range
>         -- get scheduled; cast a large net: any windowed periods in this
>         -- schedule range mean those windows won't get scheduled here.
>         let h = filterHistory history start (packDays+1) 
>         let ws' = getWindows sessions'' h 
>         let ws = map (\w -> w {wComplete = True}) ws'
>         let sessions' = updateSessions sessions'' [] [] [] ws
>
>         -- now we pack, and look for backups
>         (newSchedPending, newTrace) <- runScoring' w rs rt $ do
>             -- it's important that we generate the score only once per
>             -- simulation step; otherwise we screw up the trace that
>             -- the plots depend on
>             sf <- genScore start . scoringSessions start undefined $ sessions'
>             -- acutally schedule!!!
>             newSched' <- dailySchedule sf Pack start packDays history sessions' quiet
>             -- simulate observing
>             newSched'' <- scheduleBackups sf Pack sessions newSched' start (24 * 60 * 1)
>             -- write any scheduled windows to the trace
>             let newWinInfo= getWindowInfo sessions newSched''
>             let oldWinInfo = getWindowPeriodsFromTrace trace
>             mapM (\w -> tell [WindowPeriods w]) (newWinInfo\\oldWinInfo)
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
>         -- now get the just published periods: that is, periods that
>         -- were already in the history, but have just gone from 
>         -- Pending to Scheduled
>         let newlyPublishedPeriods = filter (newlyPublished history) newSched
>         -- now get the canceled periods so we can make sure they aren't 
>         -- still in their sessions
>         let cs = getCanceledPeriods $ trace ++ newTrace 
>         -- find all windows that got scheduled this time around
>         let wps = filter (typeWindowed . session) newSched
>         let ws' = getWindows sessions wps 
>         let ws = map (\w -> w { wComplete = True }) ws'
>         --liftIO $ print $ "Compare periods to default periods: "
>         --liftIO $ printList $ getDefaultPeriods sessions ws
>         let dps = getDefaultPeriods sessions ws
>         let defaultsToDelete = dps \\ wps
>         let condemned = cs ++ defaultsToDelete
>         let sessions'' = updateSessions sessions' newlyScheduledPeriods newlyPublishedPeriods condemned ws
>         -- updating the history to be passed to the next sim. iteration
>         -- is actually non-trivial
>         let newHistory = updateHistory history newSched condemned 

>         -- move on to the next day in the simulation!
>         simulateDailySchedule rs (nextDay start) packDays (simDays - 1) newHistory sessions'' quiet test newHistory $! (trace ++ newTrace)
>   where
>     nextDay dt = addMinutes (1 * 24 * 60) dt 

For the given list of sessions and periods

> getWindowInfo :: [Session] -> [Period] -> [(Window, Maybe Period, Period)]
> getWindowInfo ss ps = zip3 wins chosen dps 
>   where
>     wps = filter (typeWindowed . session) ps
>     wins = getWindows ss wps
>     dps = getDefaultPeriods ss wins
>     chosen = zipWith (\d p -> if (d == p) then Nothing else Just p) dps wps

Retrieve all the windows belonging to the given periods' session.

> getWindows :: [Session] -> [Period] -> [Window]
> getWindows ss ps = map (getWindow wss) wps
>   where
>     wss = filter typeWindowed ss
>     wps = filter (typeWindowed . session) ps

Retrieve the window belonging to the given period's session.
Note we pass along all sessions so that we don't have to rely on the session tied to
the given period being up to date - this is the knot tying problem.

> getWindow :: [Session] -> Period -> Window
> getWindow ss p = fromJust $ find (periodInWindow p) (windows s)
>   where
>     s = fromJust $ find (\s -> (sId s) == (sId . session $ p)) ss 

> getDefaultPeriods ss ws = map (getDefaultPeriod ss) ws

Retrieves the default period for a given window given the full pool of sessoins.
Note that the session is identified using the sId and retrieved from the pool of sessions,
since the session from wSession may not be up to date - this is the knot typing problem.

> getDefaultPeriod :: [Session] -> Window -> Period
> getDefaultPeriod ss w = fromJust $ find (flip periodInWindow w) $ periods s 
>   where
>     s = fromJust $ find (\s -> (sId s) == (sId . wSession $ w)) ss 

> --   period list length == 1
> oneDefault, defaultNeqChosen, chosenLtDefault :: [(Period, [Period], Maybe Window)] -> Bool
> oneDefault = all (\(p, ps, _) -> length ps == 1)
> --   chosen period not in period list
> defaultNeqChosen = all (\(p, ps, _) -> not . elem p $ ps)
> --   chosen period before period in list
> chosenLtDefault = all (\(p, ps, _) -> head ps > p)

This is vital for calculating pressures correctly.
Note: window reconciliation does NOT happen here.

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

If the given period is found in the given list of periods, compare
their states.  Used for finding what periods just got published.
TBF: needs unit test

> newlyPublished :: [Period] -> Period -> Bool 
> newlyPublished history p = case find (==p) history of
>     Nothing -> False
>     Just ph -> (pState ph) == Pending && (pState p) == Scheduled

We must combine the output of the scheduling algorithm with the history from
before the algorithm was called, but there's some complications:
   * the output from the algo. is a combination of parts of the history and
     the newly scheduled periods
   * this same output is then modified: cancellations and replacements
     (backups) may occur.
   * Also, periods from the history may have been published.  We need
   to update the final history to reflect this: that amounts to replacing the unpublished periods in the history w/ their publised equivalents.
So, we need to intelligently combine the previous history and the algo. output.
What we do is we simply combine the history and the newly scheduled periods, 
remove any redundancies (keeping scheduled periods), then remove any 
periods that we know just got condemned.  Condemned periods include both canceled 
periods, and default periods whose windows got scheduled earlier.

> updateHistory :: [Period] -> [Period] -> [Period] -> [Period]
> updateHistory history newSched condemned = filter notCondemned schd 
>   where
>     notCondemned p = not $ any (==p) condemned
>     -- make sure published periods replace their unpulished originals
>     schd = filterDupUnpubPeriods . sort $ history ++ newSched

> debugSimulation :: [Period] -> [Period] -> [Trace] -> String
> debugSimulation schdPs obsPs trace = concat [schd, obs, bcks, "\n"]
>   where
>     schd = "Scheduled: \n" ++ (showList' schdPs) ++ "\n"
>     obs = "Observed: \n" ++ (showList' obsPs) ++ "\n"
>     backups = [p | p <- obsPs, pBackup p]
>     bcks = if length backups == 0 then "" else  "Backups: \n" ++ (showList' backups) ++ "\n"

Assign the new periods to the appropriate Session, and update windows.
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

Note that in this function, we appropriately group together periods that all belong
to the same session.  But this does not happen to the windows.

> updateSessions :: [Session] -> [Period] -> [Period] -> [Period] -> [Window] -> [Session]
> updateSessions sessions periods published condemned windows = map update sessions
>   where
>     pss      = partitionWith session periods
>     update s =
>         case find (\(p:_) -> session p == s) pss of
>           -- published & condemned get passed on anyways
>           Nothing -> updateSession' s [] published condemned windows 
>           Just ps -> updateSession' s ps published condemned windows

> partitionWith            :: Eq b => (a -> b) -> [a] -> [[a]]
> partitionWith _ []       = []
> partitionWith f xs@(x:_) = as : partitionWith f bs
>   where
>     (as, bs) = partition (\t -> f t == f x) xs

Ties the knots between a session and it's periods & windows. 
But it also:
   * removes condemend periods
   * updates any scheduled windows
   * updates any pre-existing periods that have been published
Note that the windows passed in may not all belong to this given session,
so must get filtered properly.
Note that this is also true of the published periods passed in.

> updateSession' :: Session -> [Period] -> [Period] -> [Period] -> [Window] -> Session
> updateSession' s newPs published canceled ws = makeSession s ws' $ updateSessionPeriods s published canceled newPs 
>   where
>     -- any windows that belong to this session need to be marked as 
>     -- scheduled
>     sessSchedWins = filter (\w -> (wSession w) == s) ws
>     sessNonSchedWins = filter (\w -> not . elem w $ sessSchedWins) $ windows s
>     ws' = sort $ sessSchedWins ++ sessNonSchedWins

> removeCanceled :: Session -> [Period] -> [Period]
> removeCanceled s canceled =  (periods s) \\ canceled

For a given session, and it's:
   * new periods to add
   * existing periods to publish
   * canceled periods to remove
Returns the sessions new periods (ready for makeSession).
TBF: needs unit test

> updateSessionPeriods :: Session -> [Period] -> [Period] -> [Period] -> [Period]
> updateSessionPeriods s published canceled newPs = sort $ updatePublishedPeriods published $ (removeCanceled s canceled) ++ newPs

Given a list of published periods, return the second list of given
periods, with periods that appear in the published list updated (published)
Note: the given list of published periods do not have to be for the same
session as the other periods, since identity for periods depends on 
the parent session.
TBF: needs unit test

> updatePublishedPeriods :: [Period] -> [Period] -> [Period]
> updatePublishedPeriods published ps = map (publish published) ps
>   where
>     -- if the period is in the list of published, update it
>     publish pub p = if p `elem` pub then publish' p else p
>     publish' p = p {pState = Scheduled, pDuration = duration p}

This filters out duplicate, unpublished periods.

> filterDupUnpubPeriods :: [Period] -> [Period]
> filterDupUnpubPeriods ps = fop ps []
>   where
>     isPub p q = if (pState p == Scheduled) then p else q
>     fop []       ls           = ls
>     fop (p:[])   ls           = p : fop [] ls
>     fop (p:q:ps) ls
>         | p == q              = isPub p q : fop ps ls
>         | otherwise           = p : fop (q:ps) ls

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
>     times = [(15*q) `addMinutes` dt | q <- [0..(dur `div` 15)]]
> 

