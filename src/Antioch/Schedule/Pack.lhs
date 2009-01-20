> {-# OPTIONS -XParallelListComp #-}

> module Antioch.Schedule.Pack where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Data.List   (foldl', sort)
> import Data.Maybe  (fromMaybe, isNothing)

> epsilon  =  1.0e-6 :: Score

> numSteps :: Minutes -> Int
> numSteps = (`div` quarter)

`Pack` is the top-level driver function.  It is primarily concerned
with converting between our internal data structures and the external
representation of sessions and telescope periods.  Given a start time
`dt` and duration `dur` in minutes, `pack` first constructs a regular
grid `dts` of datetime values `quarter` minutes apart.  It then uses
the list of fixed and pre-scheduled `periods` to construct a partial
schedule and `mask` out datetime values that can't be scheduled.  Then
all of the open `sessions` are scored at the unmasked datetime values
to generate `items` for input to the `packWorker` function.

> pack :: ScoreFunc -> DateTime -> Minutes -> [Period] -> [Session] -> Scoring [Period]
> pack sf dt dur periods sessions = do
>     items <- mapM (toItem sf mask) sessions
>     return $! map (toPeriod dt) . packWorker sched $ items
>   where
>     dts   = scanl (flip addMinutes') dt [quarter * m | m <- [0 .. numSteps dur]]
>     sched = toSchedule dts . sort $ periods
>     mask  = zipWith (\dt s -> maybe (Just dt) (const Nothing) s) dts sched

A schedule is a list of time slots.  A given slot is either `Nothing`
if it is free and available to be scheduled, or `Just x` where x is a
fixed or pre-scheduled session that needs to be respected.  This
function assumes its inputs are sorted.

> toSchedule         :: [DateTime] -> [Period] -> [Maybe (Candidate Session)]
> toSchedule []  _   = []
> toSchedule dts []  = map (const Nothing) dts
> toSchedule dts@(dt:dts') ps@(p:ps')
>     | dt  <= begin = Nothing : toSchedule dts' ps
>     | end <= dt    = toSchedule dts ps'
>     | otherwise    =
>         Just (Candidate (session p) 0 (numSteps $ dt `diffMinutes'` begin) (pScore p)) : toSchedule dts' ps
>   where
>     begin = startTime p
>     end   = addMinutes' (duration p) begin

Convert an open session `s` into a schedulable item by scoring it with
`sf` at each of the open time slots in a mask `dts`.

> toItem          :: ScoreFunc -> [Maybe DateTime] -> Session -> Scoring (Item Session)
> toItem sf dts s = do
>     scores <- mapM (fromMaybe (return 0.0) . fmap (fmap eval . flip sf s)) dts
>     let force = if sum scores >= 0.0 then True else False
>     return $! force `seq` Item {
>         iId      = s
>       , iMinDur  = numSteps . minDuration $ s
>       , iMaxDur  = numSteps . maxDuration $ s
>       , iFuture  = scores
>       , iPast    = []
>       }

Convert candidates to telescope periods relative to a given startime.

> toPeriod              :: DateTime -> Candidate Session -> Period
> toPeriod dt candidate = defaultPeriod {
>     session   = cId candidate
>   , startTime = cStart candidate `addMinutes'` dt
>   , duration  = quarter * cDuration candidate
>   , pScore    = cScore candidate
>   }

Candidates, importantly, don't care what unit of time we're working
with.  Both `cStart` and `cDuration` are simply in "units."

> data Candidate a = Candidate {
>     cId       :: !a
>   , cStart    :: !Int
>   , cDuration :: !Int
>   , cScore    :: !Score
>   } deriving (Eq, Show)

> defaultCandidate = Candidate {
>     cId       = 0
>   , cStart    = 0
>   , cDuration = 0
>   , cScore    = 0.0
>   }

Given a filled list representing all partial solutions of size 1..N,
return a list of only those solutions contributing to the solution of
size N.  Assumes the existence of a sentinel at the end of the input
list.

> unwind    :: [Maybe (Candidate a)] -> [Candidate a]
> unwind xs = [ x { cScore = y } | x <- xs' | y <- ys' ]
>   where
>     xs' = unwind' [] xs
>     ys  = map cScore xs'
>     ys' = [ y' - y | y' <- ys | y <- 0 : ys]
>     unwind' acc []             = acc
>     unwind' acc (Nothing : xs) = unwind' acc xs
>     unwind' acc (Just x  : xs) = unwind' (x { cStart = length rest - 1} : acc) rest
>       where
>         rest = drop (cDuration x - 1) xs

> data Item a = Item {
>     iId      :: !a
>   , iMinDur  :: !Int
>   , iMaxDur  :: !Int
>   , iFuture  :: ![Score]
>   , iPast    :: ![Score]
>   } deriving (Eq, Show)

Generate a series of candidates representing the possibilities for
scheduling an item at each of a sequence of durations: 15 minutes, 30
minutes, etc.

> candidates               :: Item a -> [Maybe (Candidate a)]
> candidates Item { iId = id, iMinDur = min, iMaxDur = max, iPast = past }
>     | length past' < min = []
>     | otherwise          = toCandidate id $ replicate (min-1) Nothing ++ (map Just . drop (min-1) $ past')
>   where
>     past' = acc . takeWhile (>= epsilon) . take max $ past
>     acc   = scanl1 (+)

> toCandidate           :: a -> [Maybe Score] -> [Maybe (Candidate a)]
> toCandidate id scores = [fmap (Candidate id 0 d) s | s <- scores | d <- [1..]]

Move a score from the future to the past, so that it can now be
scheduled.

> step :: Item a -> Item a
> step item@(Item { iFuture = [],     iPast = past }) = item {               iPast = 0 : past }
> step item@(Item { iFuture = (f:fs), iPast = past }) = item { iFuture = fs, iPast = f : past }

Forget the past.  Used when we've just encountered a pre-scheduled
block that we know we won't be scheduling across.

> forget      :: Item a -> Item a
> forget item = item { iPast = [] }

> packWorker        :: [Maybe (Candidate a)] -> [Item a] -> [Candidate a]
> packWorker future = unwind . packWorker' future [Nothing] . map step

> packWorker' :: [Maybe (Candidate a)] -> [Maybe (Candidate a)] -> [Item a] -> [Maybe (Candidate a)]
> packWorker' []                 past _        = past
> packWorker' (Just b  : future) past sessions =
>     packWorker' future (Just b:past) $! map (step . forget) sessions
> packWorker' (Nothing : future) past sessions =
>     f `seq` packWorker' future (b:past) $! map step sessions
>   where
>     b = best . map (\s -> best . zipWith madd (candidates s) $ past) $ sessions
>     f = if maybe 0.0 cScore b >= 0.0 then True else False

Find the best of a collection of candidates.

> best :: [Maybe (Candidate a)] -> Maybe (Candidate a)
> best = foldl' better Nothing

Find the better of two candidates.

> better                      :: Maybe (Candidate a) -> Maybe (Candidate a) -> Maybe (Candidate a)
> better Nothing   c2         = c2
> better c1        Nothing    = c1
> better (Just c1) (Just c2)
>     | cScore c1 > cScore c2 = Just c1
>     | otherwise             = Just c2

Add a candidate to a historical value to produce a new candidate.

> madd                     :: Maybe (Candidate a) -> Maybe (Candidate a) -> Maybe (Candidate a)
> madd Nothing   _         = Nothing
> madd c1        Nothing   = c1
> madd (Just c1) (Just c2) = Just $! c1 { cScore = cScore c1 + cScore c2 }
