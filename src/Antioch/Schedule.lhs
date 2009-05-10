> {-# OPTIONS -XPatternGuards #-}

> module Antioch.Schedule where
> {- TBF: was there a good reason why we were hiding some things?
> module Antioch.Schedule (
>     Strategy
>   , pack
>   , scheduleFixedDuration
>   , scheduleFixedDuration'
>   , scheduleMinDuration
>   , scheduleLittleNell
>   , validPackScores
>   , validScores
>   , obeyDurations
>   , obeySchedDuration
>   , best
>   , constrain
>   , timeLeftHistory
>   , timeUsedHistory
>   ) where
> -}

> import Antioch.DateTime  (DateTime, addMinutes', fromGregorian, toSqlString)
> import Antioch.Utilities
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Generators
> import Control.Monad     (liftM)
> import Data.Foldable     (foldlM, foldr')
> import Data.List         (foldl', find, delete, nub, (\\))
> import Data.Maybe        (maybe, maybeToList, isJust)
> import qualified Antioch.Schedule.Pack as P
> import Test.QuickCheck hiding (frequency)
> import System.IO.Unsafe (unsafePerformIO)
> import Control.Monad.Trans (liftIO)

We need to be able to pass down knowledge of what strategy is being run.
One way to do this is through an enum that can, in turn, give us our
desired Strategy.

> data StrategyName = Pack | ScheduleMinDuration | ScheduleLittleNell deriving (Eq, Show, Read)

> getStrategy :: StrategyName -> Strategy
> getStrategy Pack = pack
> getStrategy ScheduleMinDuration = scheduleMinDuration
> getStrategy ScheduleLittleNell = scheduleLittleNell

> type Strategy = ScoreFunc -> DateTime -> Minutes -> [Period] -> [Session] -> Scoring [Period]

> pack             :: Strategy
> pack sf dt dur fixed = P.pack sf dt dur fixed 

Little Nell was Dana's original simulator, and it scheduled sessions
by simply scoring them at the begining of a Period.

> scheduleLittleNell :: Strategy
> scheduleLittleNell sf dt dur history sessions = 
>   scheduleMinDurationWorker sf dt dur history sessions firstScore

This is like 'Little Nell', but schedules sessions by scoring their average
over a Period's duration.

> scheduleMinDuration :: Strategy
> scheduleMinDuration sf dt dur history sessions = 
>   scheduleMinDurationWorker sf dt dur history sessions averageScore

Always schedules a session at its minimum duration, but choose the best
Session for scheduling w/ different methods.

> scheduleMinDurationWorker :: ScoreFunc -> DateTime -> Minutes -> [Period] -> [Session] -> BestScore -> Scoring [Period]
> scheduleMinDurationWorker sf dt dur history [] bestScorer = return []
> scheduleMinDurationWorker sf dt dur history sessions bestScorer
>     | [] <- candidates = return []
>     | otherwise        = do
>         (s, score) <- best (bestScorer sf dt) candidates
>         if score > 0.0
>           then do
>             w <- weather
>             let d = minDuration s
>             let p = Period s dt d score (forecast w) False
>             rest <- scheduleMinDurationWorker sf (d `addMinutes'` dt) (dur - d) (p : history) sessions bestScorer
>             return $ p : rest
>           else
>             scheduleMinDurationWorker sf (quarter `addMinutes'` dt) (dur - quarter) history sessions bestScorer
>   where
>     candidates = constrain history . filter (\s -> minDuration s <= dur) $ sessions

Always schedules a session at a given fixed duration.

> scheduleFixedDuration :: Minutes -> Strategy
> scheduleFixedDuration len sf dt dur history [] = return []
> scheduleFixedDuration len sf dt dur history sessions
>     | dur < len        = return []
>     | [] <- candidates = return []
>     | otherwise        = do
>         (s, score) <- best (totalScore sf dt len) sessions
>         if score > 0.0
>           then do
>             w <- weather
>             let p = Period s dt len score (forecast w) False
>             rest <- scheduleFixedDuration len sf (len `addMinutes'` dt) (dur - len) (p : history) sessions
>             return $ p : rest
>           else
>             scheduleFixedDuration len sf (len `addMinutes'` dt) (dur - len) history sessions
>   where
>     candidates = constrain history sessions

A really dumb scheduler that just looks at the first score for a session.
      
> scheduleFixedDuration' :: Minutes -> Strategy
> scheduleFixedDuration' len sf dt dur history [] = return []
> scheduleFixedDuration' len sf dt dur history sessions
>     | dur < len        = return []
>     | [] <- candidates = return []
>     | otherwise        = do
>         (s, score) <- best (liftM eval . sf dt) sessions
>         if score > 0.0
>           then do
>             w <- weather
>             let p = Period s dt len score (forecast w) False
>             rest <- scheduleFixedDuration len sf (len `addMinutes'` dt) (dur - len) (p : history) sessions
>             return $ p : rest
>           else
>             scheduleFixedDuration len sf (len `addMinutes'` dt) (dur - len) history sessions
>   where
>     candidates = constrain history sessions

During a scheduling strategy, certain filters must be applied to the list of
candidate sessions.  Some of  these filters may have been applied already
before the strategy is first called, but since the act of scheduling a session
may affect their candidacy, we need to run some of these filters within the 
strategy as well.  Examples are:
   * timeLeft - if a Session has been scheduled, and subsequently used up all its useful time, remove it in the filter.
   * timeBetween - TBF: if a Session has been scheduled recently, we may have to wait till we can do so again.
   * TBF: what else?

> constrain :: [Period] -> [Session] -> [Session]
> constrain = filter . timeLeftHistory 

Is there time for this session?  Taking into account both the session's periods
and the periods in the history?

> timeLeftHistory :: [Period] -> Session -> Bool
> timeLeftHistory history s = totalTime s - timeUsedHistory history s >= minDuration s

The list of periods (ps) may contain redundant versions of the
sessions' periods list.  

> timeUsedHistory :: [Period] -> Session -> Minutes
> timeUsedHistory ps s = sum [duration p | p <- uniquePeriods ps s] + totalUsed s 
>   where
>     uniquePeriods ps s = [p | p <- ps, s == session p] \\ periods s
> 

Select the highest scoring element of a list.

> best          :: (Monad m, Ord b) => (a -> m b) -> [a] -> m (a, b)
> best f (x:xs) = do
>     s <- f x
>     foldlM f' (x, s) xs
>   where
>     f' (x, s) y = do
>         s' <- f y
>         return $ if s' > s then (y, s') else (x, s)

QuickCheck Properties:

Make sure that the schedule produced by pack has no conlficts: no overlapping
periods. 
TBF: failing after n tests!

> prop_packValidSchedule = forAll genScheduleProjects $ \ps ->
>                      forAll genStartDate $ \starttime ->
>                      forAll genScheduleDuration $ \dur ->
>  let sched = runStrategy pack ps starttime dur [Nothing] in 
>      not (internalConflicts sched)  && 
>      obeyDurations sched && 
>      obeySchedDuration dur sched &&
>      --validPackScores sched
>      validScores sched &&
>      validSchdPositions ps sched

Same as above, but now insert some pre-schedule periods into the problem.
TBF: failing after 1 test!

> prop_packValidMixedSchedule = forAll genScheduleProjects $ \ps ->
>                      forAll genStartDate $ \starttime ->
>                      forAll genScheduleDuration $ \dur ->
>                      forAll (genSchedulePeriods starttime dur (concatMap sessions ps)) $ \fixed ->
>  let sched = runStrategy pack ps starttime dur fixed in 
>      not (internalConflicts sched)  && 
>      obeyDurations sched && 
>      obeySchedDuration dur sched &&
>      honorsFixed fixed sched &&
>      validSchdPositions' ps sched fixed
>      -- validScores sched -- TBF: periods getting neg. scores!

> prop_minDurValidSchedule = forAll genScheduleProjects $ \ps ->
>                      forAll genStartDate $ \starttime ->
>                      forAll genScheduleDuration $ \dur ->
>  let sched = runStrategy scheduleMinDuration ps starttime dur [Nothing] in
>      not (internalConflicts sched)  && 
>      obeyDurations sched && 
>      obeySchedDuration dur sched &&
>      validScores sched && -- TBF: allows scores of zero
>      validSchdPositions ps sched

> prop_minDurValidMixedSchedule = forAll genScheduleProjects $ \ps ->
>                      forAll genStartDate $ \starttime ->
>                      forAll genScheduleDuration $ \dur ->
>                      forAll (genSchedulePeriods starttime dur (concatMap sessions ps)) $ \fixed ->
>  let sched = runStrategy scheduleMinDuration ps starttime dur fixed in 
>      not (internalConflicts sched)  && 
>      obeyDurations sched && 
>      obeySchedDuration dur sched &&
>      --honorsFixed fixed sched && -- TBF: fails, by design?  
>      validScores sched && -- TBF: allows scores of zero
>      validSchdPositions ps sched

Framework for quick checking startegies

> runStrategy :: Strategy -> [Project] -> DateTime -> Minutes -> [Maybe Period] -> [Period]
> runStrategy strategy ps starttime dur fixed = unsafePerformIO $ do
>     let fixed' = concatMap maybeToList fixed
>     {- 
>     print "runStrategy: "
>     print . toSqlString $ starttime
>     print dur 
>     print "fixed:"
>     print fixed' 
>     -}
>     w <- theWeather -- TBF: is this right?
>     w' <- newWeather w (Just starttime)
>     let sess = concatMap sessions ps
>     ps <- runScoring w' [] $ do
>         fs <- genScore sess
>         strategy fs starttime dur fixed' sess
>     --print "schedule: "
>     --print ps
>     return ps


Make sure the pre-scheduled periods are in the final schedule.

> honorsFixed :: [Maybe Period] -> [Period] -> Bool
> honorsFixed []    _        = True
> honorsFixed fixed schedule = null fixed' || dropWhile (==True) (findFixed fixed' schedule) == [] 
>   where 
>     fixed' = concatMap maybeToList fixed
>     findFixed fs schedule = [isJust (find (==f) schedule) | f <- fs] 

Make sure no single period is longer or shorter then it's session's max and min duration.

> obeyDurations :: [Period] -> Bool
> obeyDurations ps = dropWhile obeysDurations ps == []
>   where
>     obeysDurations p = (minDuration . session $ p) <= duration p &&
>                        (maxDuration . session $ p) >= duration p

Make sure that no periods are scheduled where their project should be blacked
out.  TBF: this is only valid for 09B!!!

> obeyProjectBlackouts :: [Period] -> [(Period, DateRange)]  
> obeyProjectBlackouts ps = concatMap disobeysBlackout ps
>   where
>     disobeysBlackout p = filter (\(p, bo) -> (inBlackout p bo)) $ map (\bo-> (p, bo)) (blackouts p)
>     inBlackout p bo = (startTime p) < (snd bo) && (fst bo) < (endTime p)
>     blackouts p = pBlackouts . project . session $ p

Make sure that we don't have a schedule that has more periods scheduled then 
what we actually scheduled for.  TBF: right now we are scheduling an extra
15 minutes at the end.

> obeySchedDuration :: Int -> [Period] -> Bool
> obeySchedDuration dur ps = sum (map duration ps) <= dur -- + quarter -- TBF: qtr

All open sessions scheduled w/ by pack should have scores much greater then 0, 
given there are enough sessions and the weather doesn't absolutely suck.
Thus, if there are no pre-scheduled periods that pack has to work around, 
*all* the scores should be much greater then 0.

> validPackScores :: [Period] -> Bool
> validPackScores ps = dropWhile (>(1.0e-6*1000)) (map pScore ps) == []

> validScores :: [Period] -> Bool
> validScores ps = dropWhile (>=0.0) (map pScore ps) == []

Check to make sure positions, such as elevation, RA & Dec are valid

> validSchdPositions :: [Project] -> [Period] -> Bool
> validSchdPositions projs ps = validPositions projs && validElevs ps

Same as above, but because the fixed periods may have been created arbitrarily,
causing invalid elevations, don't check them

> validSchdPositions' :: [Project] -> [Period] -> [Maybe Period] -> Bool
> validSchdPositions' projs ps fixed = validSchdPositions projs ps'
>   where
>     fixed' = concatMap maybeToList fixed
>     ps' = ps \\ fixed'

> validPositions :: [Project] -> Bool
> validPositions projs = validRAs ss && validDecs ss
>   where 
>     ss = concatMap sessions projs

> validRAs :: [Session] -> Bool
> validRAs ss = dropWhile validRA ss == []

> validDecs :: [Session] -> Bool
> validDecs ss = dropWhile validDec ss == []

> validElevs :: [Period] -> Bool
> validElevs ps = dropWhile validElev ps == []

> validElev :: Period -> Bool
> validElev p = 5 <= el && el <= 90 
>   where
>     el = elevationFromZenith p
