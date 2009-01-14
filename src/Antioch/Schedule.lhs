> {-# OPTIONS -XPatternGuards #-}

> module Antioch.Schedule (
>     Strategy
>   , scheduleFixedDuration
>   , scheduleMinDuration
>   ) where

> import Antioch.DateTime  (DateTime, addMinutes')
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Control.Monad     (liftM)
> import Data.Foldable     (foldlM)
> import Data.List         (foldl')
> import Data.Maybe        (maybe)

> stepSize = 15

> type Strategy = ScoreFunc -> DateTime -> Minutes -> [Period] -> [Session] -> Scoring [Period]

Always schedules a session at its minimum duration.

> scheduleMinDuration :: Strategy
> scheduleMinDuration sf dt dur history sessions
>     | [] <- candidates = return []
>     | otherwise        = do
>         s <- best (averageScore sf dt) candidates
>         let d = minDuration s
>         p <- liftM (Period s dt d) $ totalScore sf dt d s
>         rest <- scheduleMinDuration sf (d `addMinutes'` dt) (dur - d) (p : history) sessions
>         return $ p : rest
>   where
>     candidates = constrain history . filter (\s -> minDuration s <= dur) $ sessions

Always schedules a session at a given fixed duration.

> scheduleFixedDuration :: Minutes -> Strategy
> scheduleFixedDuration len sf dt dur history sessions
>     | dur < len        = return []
>     | [] <- candidates = return []
>     | otherwise        = do
>         s <- best (totalScore sf dt len) sessions
>         p <- liftM (Period s dt len) $ totalScore sf dt len s
>         rest <- scheduleFixedDuration len sf (len `addMinutes'` dt) (dur - len) (p : history) sessions
>         return $ p : rest
>   where
>     candidates = constrain history sessions

> constrain _ = id

Compute the average score for a given session over an interval.

> averageScore :: ScoreFunc -> DateTime -> Session -> Scoring Float
> averageScore sf dt s = do
>     score <- totalScore sf dt dur s
>     return $ score / fromIntegral (dur `div` stepSize + 1)
>   where
>     dur = minDuration s

Compute the total score for a given session over an interval.

> totalScore :: ScoreFunc -> DateTime -> Minutes -> Session -> Scoring Float
> totalScore sf dt dur s = do
>     scores <- mapM (liftM eval . flip sf s) $ times
>     return $ addScores scores
>   where
>     times  = map (`addMinutes'` dt) [0, stepSize .. dur-1]

Add a set of scores, with the added complication that if any
individual score is zero then the end result must also be zero.

> addScores :: [Score] -> Score
> addScores = maybe 0.0 id . foldr step (Just 0.0)
>   where
>     step s Nothing   = Nothing
>     step s (Just x)
>         | s < 1.0e-6 = Nothing
>         | otherwise  = Just $ x + s

Select the highest scoring element of a list.

> best          :: (Monad m, Ord b) => (a -> m b) -> [a] -> m a
> best f (x:xs) = do
>     s <- f x
>     liftM fst . foldlM f' (x, s) $ xs
>   where
>     f' (x, s) y = do
>         s' <- f y
>         return $ if s' > s then (y, s') else (x, s)
