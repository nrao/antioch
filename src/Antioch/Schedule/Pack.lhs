> {-# OPTIONS -XParallelListComp #-}

> module Antioch.Schedule.Pack where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Generators
> import Data.List   (foldl', sort, delete)
> import Data.Maybe  (fromMaybe, isNothing)
> import Test.QuickCheck hiding (frequency)
> import System.IO.Unsafe (unsafePerformIO)

> epsilon  =  1.0e-4 :: Score

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
> pack sf dt dur fixed sessions = do
>     let dts = quarterDateTimes dt dur
>     -- TBF: the call to sort below is causing an infinite loop 
>     -- (or stack overflow).  For now, just assume that fixed is sorted.
>     -- let sched = toSchedule dts . sort $ fixed
>     let sched = toSchedule dts fixed
>     items <- mapM (toItem sf (mask dts sched)) sessions
>     return $! map (toPeriod dt) . packWorker sched $ items

Construct a list of datetimes using quarter intervals

> quarterDateTimes :: DateTime -> Minutes -> [DateTime]
> quarterDateTimes dt dur = [(quarter * m) `addMinutes` dt | m <- [0 .. numSteps dur - 1]]

Mask out datetimes that are covered by a session already.

> mask :: [DateTime] -> [Maybe (Candidate Session)] -> [Maybe DateTime]
> mask dts sched  = zipWith (\dt s -> maybe (Just dt) (const Nothing) s) dts sched

A schedule is a list of time slots.  A given slot is either `Nothing`
if it is free and available to be scheduled, or `Just x` where x is a
fixed or pre-scheduled session that needs to be respected.  This
function assumes its inputs are sorted.

> toSchedule         :: [DateTime] -> [Period] -> [Maybe (Candidate Session)]
> toSchedule []  _   = []
> toSchedule dts []  = map (const Nothing) dts
> toSchedule dts@(dt:dts') ps@(p:ps')
>     | dt  < begin = Nothing : toSchedule dts' ps
>     | end <= dt    = toSchedule dts ps'
>     | otherwise    =
>         Just (Candidate (session p) 0 (1 + (numSteps $ dt `diffMinutes'` begin)) (pScore p)) : toSchedule dts' ps
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
>   , startTime = (quarter * cStart candidate) `addMinutes` dt
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

Given the schedule (showing free and pre-scheduled time slots) and the list
of sessions to pack (items have scores), returns when the session and pre-
schedule time slots should occur (a list of *only* candidates)

> packWorker        :: [Maybe (Candidate a)] -> [Item a] -> [Candidate a]
> packWorker future items = unwind . packWorker' future [Nothing] . map step $ items

Returns a list representing each time slot, with each element either being 
Nothing or a Candidate.  Note that this list is 'Nothing' terminated.
This function is initially called from 'packWorker' as:
packWorker' future [Nothing] (map step items)
Note that the 'past' param is seeded w/ [Nothing], and that the Items
have their first future score moved out their past scores ('step').
The basic recursive pattern here is that each element of the future list
is inspected, and the past list is constructed, until we reach the end of the 
future list, where we terminate and return our constructed past list.

> packWorker' :: [Maybe (Candidate a)] -> [Maybe (Candidate a)] -> [Item a] -> [Maybe (Candidate a)]
> packWorker' []                 past _        = past  -- termination pattern
> -- if there is something pre-scheduled, it wins: put it in the 'past' list
> -- and remove the past scores from all sessions, then step to next time slot
> packWorker' (Just b  : future) past sessions =
>     packWorker' future (Just b:past) $! map (step . forget) sessions
> -- if there is nothing pre-scheduled, find the best candiate for this period,
> -- then step to the next time slot
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
>     | cScore c1 - cScore c2 > epsilon = Just c1
>     | otherwise                       = Just c2

Add a candidate to a historical value to produce a new candidate.

> madd                     :: Maybe (Candidate a) -> Maybe (Candidate a) -> Maybe (Candidate a)
> madd Nothing   _         = Nothing
> madd c1        Nothing   = c1
> madd (Just c1) (Just c2) = Just $! c1 { cScore = cScore c1 + cScore c2 }

Quick Check properties:

Make sure that the schedule produced by pack has no conlficts: no overlapping
periods. 
TBF: this does not work when using genScheduleProjects, genScheduleDuration, 
and genStartDate.  why not?
       
> prop_validSchedule = forAll (genProjects 20) $ \ps ->
>  --                    forAll genScheduleProjects $ \duration ->
>  --                    forAll genStartDate $ \starttime ->
>  --                    forAll genScheduleDuration $ \duration ->
>  let sched = runPacking ps in 
>      conflicts sched sched == False  && 
>      obeyDurations sched && 
>      obeySchedDuration duration sched
>  where
>    runPacking ps  = unsafePerformIO $ do
>       let starttime = fromGregorian 2006 10 15 12 0 0
>       w <- theWeather -- TBF: is this right?
>       w' <- newWeather w (Just $ starttime)
>       let sess = concatMap sessions ps
>       let fs = genScore sess
>       let sched = pack fs starttime duration [] sess
>       sched' <- runScoring w' [] $ sched
>       putStrLn . show $ sched'
>       return $ sched'
>    duration = 12 * 60

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
> obeySchedDuration dur ps = sum (map duration ps) <= dur + quarter -- TBF: qtr

TBF: the sudoku.lhs from the python beta test has lots of tested code for 
finding conflicts.  we should probably tap into that eventually.  For now
we'll reproduce some of the basic conflict detection stuff:

instance Ord t => Span (Interval t) where
    (Interval s1 e1) `conflict` (Interval s2 e2) = s1 < e2 && s2 < e1

> overlap :: Period -> Period -> Bool
> overlap p1 p2 = s1 < e2 && s2 < e1
>   where
>     s1 = startTime p1
>     s2 = startTime p2
>     e1 = (duration p1) `addMinutes'` s1
>     e2 = (duration p2) `addMinutes'` s2

TBF: can't seem to implement this with something like dropWhile! arrgh...

> overlaps :: Period -> [Period] -> Bool
> overlaps y [] = False
> overlaps y (x:xs) | overlap y x = True
>                   | otherwise = overlaps y xs

TBF: this sucks.  learn to program dude.

> conflicts :: [Period] -> [Period] -> Bool
> conflicts [] ys = False
> conflicts (x:xs) ys | overlaps x (delete x ys) = True
>                     | otherwise = conflicts xs ys

> testPeriods = [p1, p2, p3]
> s = defaultSession
> st1 = fromGregorian 2006 1 1 0 0 0
> st2 = 60 `addMinutes` st1
> st3 = 60 `addMinutes` st2
> p1 = Period s st1 60 0
> p2 = Period s st2 60 0
> p3 = Period s st3 60 0
