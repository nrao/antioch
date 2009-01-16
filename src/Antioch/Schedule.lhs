> {-# OPTIONS -XPatternGuards #-}

> module Antioch.Schedule (
>     Strategy
>   , scheduleFixedDuration
>   , scheduleFixedDuration'
>   , scheduleMinDuration
>   ) where

> import Antioch.DateTime  (DateTime, addMinutes')
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Control.Monad     (liftM)
> import Data.Foldable     (foldlM, foldr')
> import Data.List         (foldl')
> import Data.Maybe        (maybe)

> type Strategy = ScoreFunc -> DateTime -> Minutes -> [Period] -> [Session] -> Scoring [Period]

  Always schedules a session at its minimum duration.

> scheduleMinDuration :: Strategy
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
>           else do
>             scheduleMinDuration sf (stepSize `addMinutes'` dt) (dur - stepSize) history sessions
>   where
>     candidates = constrain history . filter (\s -> minDuration s <= dur) $ sessions

Always schedules a session at a given fixed duration.

> scheduleFixedDuration :: Minutes -> Strategy
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
>           else do
>             scheduleFixedDuration len sf (len `addMinutes'` dt) (dur - len) history sessions
>   where
>     candidates = constrain history sessions

A really dumb scheduler that just looks at the first score for a session.
      
> scheduleFixedDuration' :: Minutes -> Strategy
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
>           else do
>             scheduleFixedDuration len sf (len `addMinutes'` dt) (dur - len) history sessions
>   where
>     candidates = constrain history sessions

> constrain _ = id

Select the highest scoring element of a list.

> best          :: (Monad m, Ord b) => (a -> m b) -> [a] -> m (a, b)
> best f (x:xs) = do
>     s <- f x
>     foldlM f' (x, s) $ xs
>   where
>     f' (x, s) y = do
>         s' <- f y
>         return $ if s' > s then (y, s') else (x, s)
