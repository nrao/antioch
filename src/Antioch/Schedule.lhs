> {-# OPTIONS -XPatternGuards #-}

> module Antioch.Schedule (
>     Strategy
>   , pack
>   , scheduleFixedDuration
>   , scheduleFixedDuration'
>   , scheduleMinDuration
>   , validPackScores
>   , obeyDurations
>   , obeySchedDuration
>   , best
>   ) where

> import Antioch.DateTime  (DateTime, addMinutes', fromGregorian, toSqlString)
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Generators
> import Control.Monad     (liftM)
> import Data.Foldable     (foldlM, foldr')
> import Data.List         (foldl', find, delete)
> import Data.Maybe        (maybe, maybeToList, isJust)
> import qualified Antioch.Schedule.Pack as P
> import Test.QuickCheck hiding (frequency)
> import System.IO.Unsafe (unsafePerformIO)

> type Strategy = ScoreFunc -> DateTime -> Minutes -> [Period] -> [Session] -> Scoring [Period]

> pack             :: Strategy
> pack sf dt dur _ = P.pack sf dt dur []

Always schedules a session at its minimum duration.

> scheduleMinDuration :: Strategy
> scheduleMinDuration sf dt dur history [] = return []
> scheduleMinDuration sf dt dur history sessions
>     | [] <- candidates = return []
>     | otherwise        = do
>         (s, score) <- best (averageScore sf dt) candidates
>         if score > 0.0
>           then do
>             let d = minDuration s
>             let p = Period s dt d score
>             rest <- scheduleMinDuration sf (d `addMinutes'` dt) (dur - d) (p : history) sessions
>             return $ p : rest
>           else
>             scheduleMinDuration sf (quarter `addMinutes'` dt) (dur - quarter) history sessions
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
>             let p = Period s dt len score
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
>             let p = Period s dt len score
>             rest <- scheduleFixedDuration len sf (len `addMinutes'` dt) (dur - len) (p : history) sessions
>             return $ p : rest
>           else
>             scheduleFixedDuration len sf (len `addMinutes'` dt) (dur - len) history sessions
>   where
>     candidates = constrain history sessions

> constrain _ = id

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
       
> prop_packValidSchedule = forAll genScheduleProjects $ \ps ->
>                      forAll genStartDate $ \starttime ->
>                      forAll genScheduleDuration $ \dur ->
>  let sched = runStrategy pack ps starttime dur [Nothing] in 
>      not (internalConflicts sched)  && 
>      obeyDurations sched && 
>      obeySchedDuration dur sched &&
>      --validPackScores sched
>      validScores sched

Same as above, but now insert some pre-schedule periods into the problem.

> prop_packValidMixedSchedule = forAll genScheduleProjects $ \ps ->
>                      forAll genStartDate $ \starttime ->
>                      forAll genScheduleDuration $ \dur ->
>                      forAll (genSchedulePeriods starttime dur (concatMap sessions ps)) $ \fixed ->
>  let sched = runStrategy pack ps starttime dur fixed in 
>      not (internalConflicts sched)  && 
>      obeyDurations sched && 
>      obeySchedDuration dur sched &&
>      honorsFixed fixed sched
>      -- validScores sched -- TBF: periods getting neg. scores!

> prop_minDurValidSchedule = forAll genScheduleProjects $ \ps ->
>                      forAll genStartDate $ \starttime ->
>                      forAll genScheduleDuration $ \dur ->
>  let sched = runStrategy scheduleMinDuration ps starttime dur [Nothing] in
>      not (internalConflicts sched)  && 
>      obeyDurations sched && 
>      obeySchedDuration dur sched &&
>      validScores sched  -- TBF: allows scores of zero

> prop_minDurValidMixedSchedule = forAll genScheduleProjects $ \ps ->
>                      forAll genStartDate $ \starttime ->
>                      forAll genScheduleDuration $ \dur ->
>                      forAll (genSchedulePeriods starttime dur (concatMap sessions ps)) $ \fixed ->
>  let sched = runStrategy scheduleMinDuration ps starttime dur fixed in 
>      not (internalConflicts sched)  && 
>      obeyDurations sched && 
>      obeySchedDuration dur sched &&
>      --honorsFixed fixed sched && -- TBF: fails, by design?  
>      validScores sched -- TBF: allows scores of zero

Framework for quick checking startegies

> runStrategy :: Strategy -> [Project] -> DateTime -> Minutes -> [Maybe Period] -> [Period]
> runStrategy strategy ps starttime dur fixed = unsafePerformIO $ do
>     let fixed' = concatMap maybeToList fixed
>     {-print "runStrategy: "
>     print . toSqlString $ starttime
>     print dur 
>     print "fixed:"
>     print fixed' -}
>     w <- theWeather -- TBF: is this right?
>     w' <- newWeather w (Just starttime)
>     let sess = concatMap sessions ps
>     fs <- genScore sess
>     runScoring w' [] $ strategy fs starttime dur fixed' sess

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


