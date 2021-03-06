Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

> module Antioch.RunScores where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather (getWeather, getWeatherTest, Weather)
> import Antioch.ReceiverTemperatures (getReceiverTemperatures, ReceiverTemperatures) 
> import Antioch.Score
> import Antioch.Filters
> import Antioch.HardwareSchedule (getReceiverSchedule) 
> import Antioch.Utilities         -- debug
> import Antioch.DSSData 

> import Control.Monad.Trans (liftIO)
> import Data.Maybe
> import Data.List

This module is responsible for computing various types of scores for use in production scheduling (not simulations).
These are called from the modules in src/Server, which process
calls to the various urls of the Antioch Server.

In other words, here is the business logic that determines the results
of users' requests.  For example, in Nubbles, the Period Explorer
displays the current scores for displayed periods.  What those scores
actually are is determened here.

There are a few main variables that determine the
Scores final result:

   * Weather - when the weather origin is affects what weather data is used for any given time being scored.  For example, if the origin of the weather is the user's 'now', and a period is being scored that starts in one hour, the best forecast weather data will be used.  Fortunately, all of the functions in this file use the users 'now' for the weather origin (we aren't simulating here, afterall).

   * Scoring Factors - some types of scoring explicitly ignore certain factors.  For example, to get the current score for a pre-existing period, one must ignore the 'enoughTimeBetween' scoring factor.  The different methods that provide these are (all of type Scoring ScoreFunc):
      * genScore - uses scoringFactors, which is the complete list of factors (used in scheduling).
      * genPeriodScore - uses periodFactors, used for scores of pre-existing periods (ignores 'enoughTimeBetween').
      * genPartScore - uses a partial list of factors that may be added on later, depending on user specifications (in the UI). Used for finding Nominees.
      
   * Session Pool - the pool of sessions affects the pressure calculations, so whatever filtering is done on this pool can affect results.  For most scoring methods, we use the scoringSessions filter, which filters out session grades < B, and Maintenance.

Other outside factors that affect scoring, but are more or less invariant in the code:
   * Receiver Temperatures
   * Receiver Schedule

NOTE: to make these functions accessable to unit testing:
   * the projects are passed in - this makes the unit tests independent of the projects DB; unit tests create what they need and pass it in, while the Server simply gives the results from 'getProjects'
   * test parameter - if set to True, Weather is *not* NOW, but given the origin from something predictable (like earliest period start time), and receiver schedule is ignored.  Thus we can get consistent unit test results.

Now for the the indiviual functions:

----------------------------------------------------------

Computes:
   * the current scores for all the given Periods (identified by ID)
   * the historical score of any Period that needs it, and saves this to the DB
   * the MOC of any Period that needs it, and saves this to the DB

Example usage: Period Explorer

 Variables:
    * Weather - weather for NOW.
    * Scoring Factors - genPeriodScore, genScore
    * Session Pool - scoringSessions

> runUpdatePeriods :: [Int] -> [Project] -> Bool -> IO ([(Int, Score, Maybe Score, Maybe Bool)])
> runUpdatePeriods pids projs test = do
>
>     -- get all the sessions and periods for these projects
>     let ss = concatMap sessions projs
>     let ps = concatMap periods ss
>
>     -- target periods
>     let tps = filter (\p -> (peId p) `elem` pids) ps
>     -- associated target sessions
>     let tss = map (\p -> (find (\s -> (sId . session $ p) == (sId s)) ss)) tps
>     -- target (period, session) sets
>     let tsps = catMaybes . map raise . zip tps $ tss
>
>     -- get the earliest start time for rcvr schedule
>     let start = minimum $ map startTime tps
>
>     -- set up invariant part of the scoring environment
>     -- NOTE: if it's a test, use same weather and rcvr schedule.
>     w <- if test then getWeatherTest $ Just start else getWeather Nothing
>     rs <- if test then return [] else liftIO $ getReceiverSchedule . Just $ start
>     rt <- liftIO $ getReceiverTemperatures
>
>     -- compute current scores
>     scores <- sequence $ map (\(p, s) -> scorePeriod p s (scoringSessions (startTime p) undefined ss) w rs rt) tsps
>
>     -- compute historical scores, where needed
>     hscores <- sequence $ map (\(p, s) -> scoreSession' p s (scoringSessions (startTime p) undefined ss) w rs rt) tsps
>
>     -- compute MOCs, where needed
>     mocs <- sequence $ map (\(p, s) -> evalMOC p s ) tsps
>
>     -- match the scores backup w/ their period Ids
>     return $ zip4 (map (peId . fst) tsps) scores hscores mocs 

Scores the session, only if it's needed.

> scoreSession' :: Period -> Session -> [Session] -> Weather -> ReceiverSchedule -> ReceiverTemperatures -> IO (Maybe Score)
> scoreSession' p s ss w rs rt | (pScore p) == (-1.0) = do
>   hscore <- scoreSession dt dur s ss w rs rt
>   return $ Just hscore
>                              | otherwise = return $ Nothing
>   where
>     dt = startTime p
>     dur = duration p

Evaluates the Period's MOC, again, only if needed.

> evalMOC :: Period -> Session -> IO (Maybe Bool)
> evalMOC p s | isNothing . pMoc $ p = runMOC dt dur s False
>             | otherwise            = return Nothing 
>   where
>     dt = startTime p
>     dur = duration p


Computes the current score factors (and other, subfactors) of a given Session (identified by ID) over the given time range, to be found in the given list of Projects.  
Note how this uses the same variables as runScorePeriods, but the inputs and outputs are different.
Example usage: the Factors tab in Nubbles.

Variables:
   * Weather - weather for NOW.
   * Scoring Factors - genPeriodScore
   * Session Pool - scoringSessions

> runFactors :: Int -> DateTime -> Minutes -> [Project] -> Bool -> IO ((Session, [Factors]))
> runFactors id dt dur projs test = do
>     -- find the session in the projects
>     let ss = concatMap sessions projs
>     let sss = scoringSessions dt undefined ss
>     let s = head $ filter (\s -> (sId s) == id) ss
>     -- setup the environment; watch for tests
>     w <- liftIO $ if test then getWeatherTest $ Just dt else getWeather Nothing
>     rs <- if test then return [] else liftIO $ getReceiverSchedule $ Just dt
>     rt <- liftIO $ getReceiverTemperatures
>     -- compute and combine different factors
>     factors' <- liftIO $ scoreFactors s w sss dt dur rs
>     let scores = map (\x -> [x]) . zip (repeat "score") . map Just . map eval $ factors'
>     factors <- liftIO $ scoreElements s w rt sss dt dur rs
>     return $ (s, zipWith (++) scores factors)
>

This is probably the the outlier among this group of functions.  Used for finding candidate periods to fill in a given hole in the schedule.  The variables are different, but note that overhead is handled consistently with the way runScorePeriods and runScoreSessions does it.


Variables:
   * Weather - weather for NOW.
   * Scoring Factors - genPartScore
   * Session Pool - scoringSessions

Example params:
[("start",Just "2011-01-04 03A45A00"),("duration",Just "165"),("timeBetween",Just "false"),("minimum",Just "false"),("blackout",Just "false"),("backup",Just "false"),("completed",Just "false"),("rfi",Just "false"),("tz",Just "UTC"),("sortField",Just "null"),("sortDir",Just "NONE")]


> runNominees :: DateTime -> Maybe Minutes -> Maybe Minutes -> [(String, Maybe String)] -> [Project] -> Bool -> IO ([Nominee])
> runNominees dt lower upper params projs test = do
>     -- interpret all the different knobs for finding nominees
>     let rfi         = fromJust . fromJust . lookup "rfi" $ params
>     let timeBetween = fromJust . fromJust . lookup "timeBetween" $ params
>     let blackout    = fromJust . fromJust . lookup "blackout" $ params
>     -- Using the flags from above, what optional scoring factors should we include? 
>     let sfs = catMaybes [if rfi == "true" then Nothing else Just correctTimeOfDay
>                        , if timeBetween == "true" then Nothing else Just enoughTimeBetween
>                        , if blackout == "true" then Nothing else Just observerAvailable
>                         ]
>     -- use only backup sessions?
>     let backup = fromJust . fromJust . lookup "backup" $ params
>     -- include completed sessions?
>     let completed = fromJust . fromJust . lookup "completed" $ params
>     -- Using the above two flags, construct the list of filters to apply against our
>     -- pool of candidate sessions
>     let filter = catMaybes . concat $ [
>             if completed == "true" then [Nothing] else [Just hasTimeSchedulable, Just isNotComplete]
>           , [Just isNotMaintenance]
>           , [Just isNotTypeFixed]
>           , [Just isNotTypeElective]
>           , [Just isApproved]
>           , [Just hasObservers]
>           , if backup == "true" then [Just isBackup] else [Nothing]
>                                       ]
>     let schedSessions = filterSessions dt undefined filter
>
>     -- our original pool of sessions
>     let ss = concatMap sessions projs
>
>     -- setup the enviornment; watch for tests
>     w <- liftIO $ if test then getWeatherTest $ Just (addMinutes (-60) dt) else getWeather Nothing
>     rs <- if test then return [] else liftIO $ getReceiverSchedule $ Just dt
>     rt <- liftIO $ getReceiverTemperatures
> 
>     -- find the nominees
>     nominees <- liftIO $ runScoring w rs rt $ do
>         -- generate the scoring function, adding on whatever optional scoring functions were
>         -- selected from above
>         sf <- genPartScore dt sfs . scoringSessions dt undefined $ ss
>         -- apply the filter we constructed from above
>         durations <- bestDurations sf dt lower upper $ schedSessions ss
>         return durations
>     return nominees

MinimumObservingCondition (MOC) - here is the simplest case. What's
different (and simpler) is that MOC only uses a few scoring factors,
so the Scoring Factors and Session Pool variables don't apply here.

Variables:
   * Weather - weather for NOW.
   * Scoring Factors - N/A
   * Session Pool - N/A

> runMOC :: DateTime -> Minutes -> Session -> Bool -> IO (Maybe Bool)
> runMOC dt dur s test = do
>   w <- if test then getWeatherTest $ Just hrEarlyDt else getWeather Nothing
>   rs <- if test then return [] else getReceiverSchedule $ Just dt
>   rt <- getReceiverTemperatures
>   moc <- runScoring w rs rt $ minimumObservingConditions dt dur s
>   return moc
>     where
>   hrEarlyDt = addMinutes (-60) dt

> runPeriodMOC :: Period -> Bool -> IO (Maybe Bool)
> runPeriodMOC p test = do
>     runMOC (startTime p) (duration p) (session p) test

Here is the one exception: a function NOT called from src/Server, but
from the command line.  So, the outer layer will interact w/ the DB,
so that the inner part we can unit test, like we've done above.

> updateMOCs :: IO ([(Int, Maybe Bool)])
> updateMOCs = do
>     print "updateMOCs"
>     (start, dur) <- getMOCTimeRange
>     let end = addMinutes dur start
>     print $ "MOC Range from : " ++ (show . toSqlString $ start) ++ " to: " ++ (show . toSqlString $ end)
>     -- read from the DB
>     cnn <- connect
>     ps <- getDiscretionaryPeriods cnn  start dur
>     print "Periods: "
>     printList ps
>     -- calcualte what we need
>     mocs <- getCurrentMOCs ps False
>     print "MOCs: "
>     print mocs
>     -- write to the DB
>     mapM (updatePeriodMOC' cnn) mocs
>     return mocs
>   where
>     updatePeriodMOC' cnn (pid, moc) = updatePeriodMOC cnn pid moc

> getCurrentMOCs :: [Period] -> Bool -> IO ([(Int, Maybe Bool)])
> getCurrentMOCs ps test = do
>     mocs <- mapM (flip runPeriodMOC test) ps
>     return $ zip (map peId ps) (mocs)
> 

> getMOCTimeRange :: IO ((DateTime, Minutes))
> getMOCTimeRange = do
>   now <- getCurrentTime
>   let start = addMinutes (-30) now
>   let dur = (2*24*60) + 30 -- 2 days + 30 mins
>   return (start, dur)

Utilities:

> raise :: (Period, Maybe Session) -> Maybe (Period, Session)
> raise (p, ms)
>     | ms == Nothing   = Nothing
>     | otherwise       = Just (p, fromJust ms)
