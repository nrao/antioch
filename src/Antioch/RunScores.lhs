> module Antioch.RunScores where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Weather
> import Antioch.ReceiverTemperatures
> import Antioch.Score
> import Antioch.Filters
> import Antioch.HardwareSchedule

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

TBFs:
   * Score.scorePeriod & Score.scoreSession should be able to share code, and put the IO Monad part into this module.
   # Can Score.bestDuration also be sharing w/ scorePeriod & scoreSession better?

NOTE: to make these functions accessable to unit testing:
   * the projects are passed in - this makes the unit tests independent of the projects DB; unit tests create what they need and pass it in, while the Server simply gives the results from 'getProjects'
   * test parameter - if set to True, Weather is *not* NOW, but given the origin from something predictable (like earliest period start time), and receiver schedule is ignored.  Thus we can get consistent unit test results.

Now for the the indiviual functions:

----------------------------------------------------------

Computes the current scores for the given Periods (identified by ID), to
be found in the given list of Projects.  
Example usage: Period Explorer's current score column.

Variables:
   * Weather - weather for NOW.
   * Scoring Factors - genPeriodScore
   * Session Pool - scoringSessions

> runScorePeriods :: [Int] -> [Project] -> Bool -> IO ([(Int, Score)])
> runScorePeriods pids projs test = do
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

>     -- compute scores
>     --scores <- sequence $ map (liftIO . scorePeriod' ss w rt rs) tsps
>     scores <- sequence $ map (\(p, s) -> scorePeriod p s (scoringSessions (startTime p) undefined ss) w rs rt) tsps
>     -- match the scores backup w/ their period Ids
>     return $ zip (map (peId . fst) tsps) scores

> {-
> scorePeriod' :: [Session] -> Weather -> ReceiverTemperatures -> ReceiverSchedule -> (Period, Session) -> IO Score
> scorePeriod' ss w rt rs psp = do
>     let p = fst psp
>     let s = snd psp
>     let dt = startTime p
>     let sss = scoringSessions dt undefined ss
>     --rs <- liftIO $ getReceiverSchedule . Just $ dt
>     --let rs = []
>     print ("scorePeriod: ", p) --, rs)
>     scorePeriod p s sss w rs rt
> -}

Computes the current score of a given Session (identified by ID) over
the given time range, to be found in the given list of Projects.  Example usage: a period is changed or newly created (?) via the Nell server.

Variables:
   * Weather - weather for NOW.
   * Scoring Factors -genScore (makes me think this is only used when creating a new period, since the period doesn't exist yet).
   * Scoring Pool - scoringSessions
   
> runScoreSession :: Int -> DateTime -> Minutes -> [Project] -> Bool -> IO (Score)
> runScoreSession id dt dur projs test = do
>     -- get the session to score
>     let ss = concatMap sessions projs
>     let s = head $ filter (\s -> (sId s) == id) ss
>
>     -- setup the scoring enviornment (check for unit tests)
>     --w <- liftIO $ getWeatherTest $ if test then (Just dt) else Nothing
>     w <- if test then liftIO $ getWeatherTest $ Just dt else getWeather Nothing
>     rs <- if test then return [] else liftIO $ getReceiverSchedule $ Just dt
>     rt <- liftIO $ getReceiverTemperatures
>     let sss = scoringSessions dt undefined ss
>
>     -- score!
>     score <- liftIO $ scoreSession dt dur s sss w rs rt
>     return score


Computes the current score factors (and other, subfactors) of a given Session (identified by ID) over the given time range, to be found in the given list of Projects.  
Note how this uses the same variables as runScorePeriods, but the inputs and outputs are different.
Example usage: the Factors tab in Nubbles.

Variables:
   * Weather - weather for NOW.
   * Scoring Factors - genPeriodScore
   * Session Pool - scoringSessions

> --runFactors :: Int -> DateTime -> Minutes -> [Project] -> IO ([Factors])

This is probably the the outlier among this group of functions.  Used for finding candidate periods to fill in a given hole in the schedule.  The variables are different, but note that overhead is handled consistently with the way runScorePeriods and runScoreSessions does it.


Variables:
   * Weather - weather for NOW.
   * Scoring Factors - genPartScore
   * Session Pool - scoringSessions

> --runNominees :: DateTime -> Minutes -> [Project] -> IO ([Period])

MOC - this is done so simply, that maybe we don't need it here.


Utilities:

> raise :: (Period, Maybe Session) -> Maybe (Period, Session)
> raise (p, ms)
>     | ms == Nothing   = Nothing
>     | otherwise       = Just (p, fromJust ms)
