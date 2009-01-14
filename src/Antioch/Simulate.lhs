> module Antioch.Simulate where

> import Antioch.DateTime
> import Antioch.Generators
> import Antioch.Schedule
> import Antioch.Score     (ReceiverSchedule, genScore, runScoring)
> import Antioch.Types
> import Antioch.Weather   (getWeather)
> import Data.List         (find, partition)

> simulate06 :: Strategy -> IO [Period]
> simulate06 sched = do
>     ps <- generateVec 10
>     let ps' = zipWith (\p n -> p { pId = n }) ps [0..]
>     let ps'' = [makeProject p (sessions p) | p <- ps'']
>     let ss  = zipWith (\s n -> s { sId = n }) (concatMap sessions ps'') [0..]
>     simulate sched rs dt dur int history ss
>   where
>     rs  = []
>     dt  = fromGregorian 2006 1 1 0 0 0
>     dur = 60 * 24 * 7
>     int = 60 * 24 * 2
>     history = []
  
> simulate :: Strategy -> ReceiverSchedule -> DateTime -> Minutes -> Minutes -> [Period] -> [Session] -> IO [Period]
> simulate sched rs dt dur int history sessions
>     | dur < int  = return []
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
