> {-# OPTIONS -XParallelListComp #-}

> module Antioch.Schedule.Pack where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Generators
> import Antioch.Utilities
> import Data.List   (foldl', sort, delete, find)
> import Data.Maybe  (fromMaybe, isNothing, maybeToList, isJust, fromJust)
> import Test.QuickCheck hiding (frequency)
> import System.IO.Unsafe (unsafePerformIO)
> import Control.Monad.Trans (liftIO)

> epsilon  =  1.0e-4 :: Score

> numSteps :: Minutes -> Int
> numSteps = (`div` quarter)

TBF: we are not honoring the following session attributes as we pack:
   * time between
   * schedule time <= allotted time

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
>     let sched = toSchedule dts . sort $ fixed
>     items <- mapM (toItem sf (mask dts sched)) sessions
>     return $! restoreFixed fixed dt dur $ map (toPeriod dt) . packWorker sched $ items

Some things have to be corrected before this 'schedule' can be returned:
   * pre-scheduled Periods on the time boundraries have been cut off
   * pre-schedule Periods w/ in the time boundraries have had their scores 
     mangled up.

> restoreFixed :: [Period] -> DateTime -> Minutes -> [Period] -> [Period] 
> restoreFixed fs dt dur ps = restoreFixedBoundraies fs dt dur $ map (restoreFixedScore fs) ps

Fixed periods that overlap the start and end of the time range over which
we are packing must be restored.

> restoreFixedBoundraies :: [Period] -> DateTime -> Minutes -> [Period] -> [Period]
> restoreFixedBoundraies fs dt dur ps = restoreBnd end False fs $ restoreBnd start True fs ps
>   where
>     start = dt
>     end = dur `addMinutes'` dt

Checks the boundry of the time range that we are packing for 
('hd' == True is for the start of the packing time range) to see if one of the
fixed periods initially passed in overlaps it.  If so, this original fixed
period is used to replaced the truncated period in the schedule.

> restoreBnd :: DateTime -> Bool -> [Period] -> [Period] -> [Period]
> restoreBnd dt hd fs ps | inFixed dt fs == [] = ps
> restoreBnd dt hd fs ps | otherwise           = if hd then [head $ inFixed dt fs] ++ tail ps else init ps ++ [head $ inFixed dt fs]

Does the given datetime fall within the bounds of one of the telescope periods?
If so, which ones?

> inFixed :: DateTime -> [Period] -> [Period]
> inFixed dt fs = filter (\f -> startTime f < dt && dt < periodEndTime f ) fs 

Using a list 'fs' of pre-scheduled (fixed) periods, return a period 'p'
with its original pre-packing score.  Only works for those periods that
were not on the time range boundraries.

> restoreFixedScore :: [Period] -> Period -> Period
> restoreFixedScore fs p = case find (\f -> f == p) fs of
>     Nothing -> p
>     Just f  -> p {pScore = pScore f}

Construct a list of datetimes (in minutes) at quarter intervals
starting at 'dt' for 'dur' minutes.

> quarterDateTimes :: DateTime -> Minutes -> [DateTime]
> quarterDateTimes dt dur = [(quarter * m) `addMinutes` dt | m <- [0 .. numSteps dur - 1]]

Translate a list of datetimes 'dts' into a list of possible datetimes
(using Maybe), according to the previously scheduled times found in the
schedule 'sched'.
[Mask out datetimes 'dts' that are already covered by a session in 'sched'.]
A datetime is previously scheduled if its associated value in the
schedule is Nothing.

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
>     | dt  <  begin = Nothing : toSchedule dts' ps
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
>       , iMaxDur  = numSteps $ min (maxDuration s) (totalAvail s)
>       , iTimeAv  = totalAvail s
>       , iTimeBt  = timeBetween s
>       , iFuture  = scores
>       , iPast    = []
>       }

Convert candidates to telescope periods relative to a given startime.
Remember: Candidates have unitless times, and their scores are cumulative.
Our Periods need to have Minutes, and average scores.


> toPeriod              :: DateTime -> Candidate Session -> Period
> toPeriod dt candidate = defaultPeriod {
>     session   = cId candidate
>   , startTime = (quarter * cStart candidate) `addMinutes` dt
>   , duration  = quarter * cDuration candidate
>   , pScore    = (cScore candidate)/(fromIntegral $ cDuration candidate)
>   , pForecast = dt
>   , pBackup   = False
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
size N.  Assumes the existence of a sentinel at the end of the input list.
What's going on here is the following: first the list of partial solutions
is correctly unwound to the list of Candidates (w/ correct start's and durs).
Then the scores deltas are applied to these Candidates, since these scores
were built up from multiple Candidates in the first place.
Note that this does not honor for pre-scheduled periods, this value
is restored later in pack.

TBF: when scheduling is sparse, and there are lots of Nothing's in the input to this
function, the optimal schedule is not being picked because we are always picking
the last candidate, which may NOT necessarily have the largest accumulated score!

> unwind :: [Maybe (Candidate a)] -> [Candidate a]
> unwind = unwind' []
>   where
>     unwind' acc []              = acc
>     unwind' acc (Nothing : xs)  = unwind' acc xs
>     unwind' acc xs@(Just x : xs'@(Just x' : _))
>         | cScore x' > cScore x  = unwind' acc xs'
>         | otherwise             = step acc xs
>     unwind' acc xs@(Just x : _) = step acc xs
>     step acc (Just x : xs)      =
>         unwind' (x { cStart = length rest - 1, cScore = cScore x - rscore} : acc) rest
>       where
>         rest   = drop (cDuration x - 1) xs
>         rscore = case rest of
>             []            -> 0.0
>             (Nothing : _) -> 0.0
>             (Just y  : _) -> cScore y

An Item is a "value-added" session where the additional information
is used for scheduling. Namely: min and max durations in units of
"scoring periods", e.g., quarters, and two lists of scores for
available scoring periods: ?? (iFuture) and ?? (iPast) scores.

> data Item a = Item {
>     iId      :: !a
>   , iMinDur  :: !Int
>   , iMaxDur  :: !Int
>   , iTimeAv  :: !Int
>   , iTimeBt  :: !Int
>   , iFuture  :: ![Score]
>   , iPast    :: ![Score]
>   } deriving (Eq, Show)

Generate a series of candidates representing the possibilities for
scheduling an item at each of a sequence of durations: 15 minutes, 30
minutes, etc.
Example: pass in Item {sId = 1, min = 2, max = 4, past = [1,1,0,0]
past' becomes only [1,2] because: 1) take just the first 4 (max) 2) take 
only the first two because of the zeros being < epsilon, then 3) do a
running accumulate (scanl1 (+)).
Next in our example, we call toCandidate 1 [Nothing, Just 2].

> candidates               :: Item a -> [Maybe (Candidate a)]
> candidates Item { iId = id, iMinDur = min, iMaxDur = max, iPast = past }
>     | length past' < min = []
>     | otherwise          = toCandidate id $ replicate (min-1) Nothing ++ (map Just . drop (min-1) $ past')
>   where
>     -- TBF OVERHEAD: This computes the period's score ignoring
>     -- the first quarter of the period, i.e., assumes a score of 0.0
>     --past' = case takeWhile (>= epsilon) . take max $ past of
>     --    []     -> []
>     --    (_:ss) -> scanl (+) 0.0 ss
>     -- These functions score all quarters, i.e., no overhead
>     past' = acc . takeWhile (>= epsilon) . take max $ past
>     acc   = scanl1 (+)

Given a proposed 'item' for a slot and a list of candidates representing
all the previous slots ('past'), return the count of previously 'used' time
for the item and the 'separate'ion between the current item's period
and any previous periods.  Note the returned values only apply
if a previous candidate using item was found, i.e., 'used' > 0.

> queryPast :: (Eq a) => Item a -> [Maybe (Candidate a)] -> Int -> (Int, Int, [Int])
> queryPast item past dur = queryPast' item . drop (dur - 1) $ past

> queryPast' :: (Eq a) => Item a -> [Maybe (Candidate a)] -> (Int, Int, [Int])
> --queryPast' item   past     -> (used, separate)
> queryPast'   item   [Nothing] = (0, 0, [])
> queryPast'   item (c:cs)
>     | isNothing c             = (used, 1 + separate, step:vs)
>     | itemId == candidateId   = (dur + used, 0, step:vs)
>     | otherwise               = if used > 0
>                                 then (used, dur + separate, step:vs)
>                                 else (used, separate, step:vs)
>   where
>     itemId = iId item
>     candidateId = cId (fromJust c)
>     step = min (length cs) dur - 1
>     dur = cDuration (fromMaybe (defaultCandidate {cId = (iId item), cDuration = 1}) c)
>     (used, separate, vs) = queryPast' item (drop step cs)

Given possible scores (using Maybe) over a range of a session's durations,
generate a list of possible candidates corresponding to the scores.
Note the use of fmap: here we are transforming Scores into Candidates 
inside the Maybe Monad.
Example (continued from above): toCandidate 1 [Nothing, Just 2] gives:
[ Nothing, Just (Candidate 1 0 2 2) ]

> toCandidate           :: a -> [Maybe Score] -> [Maybe (Candidate a)]
> toCandidate id scores = [fmap (Candidate id 0 d) s | s <- scores | d <- [1..]]
> -- add filter to remove candidates which break time_between, i.e.,
> -- cDuration < last.cStart (not defined!) + last.cDuration?

Move a score from the future to the past, so that it can now be
scheduled. Note that the order of the scores are reversed as they
are passed from future to past.

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

> packWorker        :: Eq a => [Maybe (Candidate a)] -> [Item a] -> [Candidate a]
> packWorker future items = unwind . packWorker' future [Nothing] . map step $ items

Returns a list representing each time slot, with each element either being 
Nothing or a Candidate.  Note that this list is 'Nothing' terminated.
This function is initially called from 'packWorker' as:
packWorker' future [Nothing] (map step items)
Note that the 'past' param is seeded w/ [Nothing], and that the Items
have their first future score moved out their past scores ('step').
The basic recursive pattern here is that each element of the future list
is inspected, and the past list is constructed, until we reach the end of
the future list, where we terminate and return our constructed past list.
Remember, the pack algorithm works by breaking down the problem into sub
problems.  For our case, that means first solving the 15-min schedule, then 
using this solution to solve for the 30-min schedule, and so on.  The
solutions to our sub-problems are represented by the 'past' param.
Note that cStart in the result is not defined, this occurs in unwind.

> packWorker' :: Eq a => [Maybe (Candidate a)] -> [Maybe (Candidate a)] -> [Item a] -> [Maybe (Candidate a)]
> -- packWorker' future             past sessions
> packWorker'    []                 past _        = past 
> packWorker'    (Just b  : future) past sessions =
>     packWorker' future (Just b:past) $! map (step . forget) sessions
> packWorker'    (Nothing : future) past sessions =
>     let b = getBest past sessions in
>     (if maybe 0.0 cScore b >= 0.0 then True else False) `seq` packWorker' future (b:past) $! map step sessions

Given the sessions (items) to pack, and the 'past', which is the step n
in our N step packing algorithm (15 minute steps):
   * from each item, create a set of candidates starting with duration 0 up
     to the max duration of the session
   * increase the scores of each candidate using the score from the 'past'
     candidate (if any).  Eventually, the 'past' will contain the best
     candidates from the previous sub-problems.
   * find the best of each of these lists of candidates found from a
     single item
   * now find the best from the collection of the best candidates for each
     item

> getBest ::  Eq a => [Maybe (Candidate a)] -> [Item a] -> Maybe (Candidate a)
> getBest past sessions = best $ map (bestCandidateOfASession past) sessions    

TBF: Start using filterCandidates once we're sure it's working and it doesn't 
seem to have a huge impact on performance.

> bestCandidateOfASession :: Eq a => [Maybe (Candidate a)] -> Item a -> Maybe (Candidate a)
> --bestCandidateOfASession past sess = best . zipWith madd (filterCandidates sess past $ candidates sess) $ past
> bestCandidateOfASession past sess = best . zipWith madd (candidates sess) $ past

We need apply certain constraints inside the packing algorithm.  For example,
time remaining, and time between must be obeyed as the candidates for packing
are constructed.  This function will apply these types of contraints to
a list of candidates, such that candidates that violate constraints are 
replaced by Nothing.

> filterCandidates :: Eq a => Item a -> [Maybe (Candidate a)] -> [Maybe (Candidate a)] -> [Maybe (Candidate a)]
> filterCandidates item past cs = map (filterCandidate item past) cs 

> filterCandidate :: Eq a => Item a -> [Maybe (Candidate a)] -> Maybe (Candidate a) -> Maybe (Candidate a)
> filterCandidate item past Nothing  = Nothing
> filterCandidate item past (Just c) | iId item == cId c = acceptCandidate item used sep c 
>                                    | otherwise         = Just c
>   where
>     (used, sep, hist) = queryPast item past (cDuration c)
>     acceptCandidate item used sep c = if (used > (iTimeAv item)) || (sep < (iTimeBt item)) then Nothing else Just c

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

Quick Check properties: see Schedule.lhs


