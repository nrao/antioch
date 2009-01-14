> module Antioch.Simulate (simulate) where

> import Antioch.DateTime  (DateTime, addMinutes', diffMinutes')
> import Antioch.Schedule  (Strategy)
> import Antioch.Score     (ReceiverSchedule, genScore, runScoring)
> import Antioch.Types
> import Antioch.Weather   (getWeather)
> import Data.List         (find, partition)

> simulate :: Strategy -> ReceiverSchedule -> DateTime -> Minutes -> Minutes -> [Period] -> [Session] -> IO [Period]
> simulate sched rs dt dur int history sessions
>     | dur < hint = return []
>     | otherwise  = do
>         w <- getWeather $ Just (negate hint `addMinutes'` dt)
>         let periods   = runScoring w rs $ sched sf start int' history sessions
>         let sessions' = updateSessions sessions periods
>         result <- simulate sched rs (hint `addMinutes'` dt) (dur - hint) int (reverse periods ++ history) sessions'
>         return $ periods ++ result
>   where
>     sf    = genScore sessions
>     hint  = int `div` 2
>     start = case history of
>         (h:_) -> duration h `addMinutes'` startTime h
>         _     -> dt
>     end   = int `addMinutes'` dt
>     int'  = end `diffMinutes'` start

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
