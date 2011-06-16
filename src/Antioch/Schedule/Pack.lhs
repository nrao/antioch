Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

> {-# OPTIONS -XParallelListComp #-}

See https://safe.nrao.edu/wiki/bin/view/GB/Software/DSSPackingAlgorithm

> module Antioch.Schedule.Pack where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Generators
> import Antioch.Utilities
> import Antioch.Weather
> import Data.List   (foldl', sort, delete, find)
> import Data.Maybe  (fromMaybe, isNothing, maybeToList, isJust, fromJust)
> import Test.QuickCheck hiding (frequency)
> import System.IO.Unsafe (unsafePerformIO)
> import Control.Monad.Trans (liftIO)

> epsilon = 1.0e-4 :: Score

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
>     -- make sure the resultant periods record the proper weather origin
>     w <- weather
>     let dtw = forecast w
>     let dts = quarterDateTimes dt dur
>     let sched = toSchedule dts . sort $ fixed
>     items <- mapM (toItem dt dur sf (mask dts sched)) sessions
>     return $! restoreFixed fixed dt dur $ map (toPeriod dt dtw) . packWorker sched $ items

Some things have to be corrected before this 'schedule' can be returned:
   * pre-scheduled Periods on the time boundraries have been cut off
   * pre-schedule Periods w/ in the time boundraries have had their scores 
     mangled up (and other attributes overriden!).

> restoreFixed :: [Period] -> DateTime -> Minutes -> [Period] -> [Period] 
> restoreFixed fs dt dur ps = restoreFixedBoundraies fs dt dur $ map (restoreFixedScore fs) ps

Fixed periods that overlap the start and end of the time range over which
we are packing must be restored.

> restoreFixedBoundraies :: [Period] -> DateTime -> Minutes -> [Period] -> [Period]
> restoreFixedBoundraies fs dt dur ps = restoreBnd end False fs $ restoreBnd start True fs ps
>   where
>     start = dt
>     end = dur `addMinutes` dt

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
Also need to reset other attributes here too!

> restoreFixedScore :: [Period] -> Period -> Period
> restoreFixedScore fs p = case find (\f -> f == p) fs of
>     Nothing -> p
>     Just f  -> p {pScore = pScore f
>                 , pBackup = pBackup f
>                 , pState = pState f
>                 , pForecast = pForecast f}

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
>         Just (Candidate (session p) (pId . project . session $ p) 0 (1 + (numSteps $ dt `diffMinutes` begin)) (pScore p)) : toSchedule dts' ps
>   where
>     begin = startTime p
>     end   = addMinutes (duration p) begin
>     id   = pId . project . session $ p

Convert an open session `s` into a schedulable item by scoring it with
`sf` at each of the open time slots in a mask `dts`.

> toItem          :: DateTime -> Minutes -> ScoreFunc -> [Maybe DateTime] -> Session -> Scoring (Item Session)
> toItem dt dur sf dts s = do
>     scores <- mapM (fromMaybe (return 0.0) . fmap (fmap eval . flip sf s)) dts
>     let force = if sum scores > 0.0 then True else False
>     return $! force `seq` Item {
>         iId      = s
>       , iSessId  = sId s
>       , iProj    = pId . project $ s
>       , iMinDur  = numSteps . minDuration $ s
>       , iMaxDur  = numSteps $ min (maxDuration s) (sAvailT s)
>       , iOvhdDur = getOverhead s
>       , iSTimAv  = numSteps $ sAvailT s
>       , iPTimAv  = numSteps $ pAvailT (project s)
>       , iTimeBt  = numSteps $ timeBetween s
>       , iTrType  = transit s
>       , iTrnsts  = if (transit s) == Optional
>                    then []
>                    else deriveTransits dt dur s
>       , iFuture  = scores
>       , iPast    = []
>       }

Generate a session's transit times over the time slice dt/dur

> deriveTransits :: DateTime -> Minutes -> Session -> [Int]
> deriveTransits dt dur sess = map (\t -> numSteps $ t `diffMinutes` dt) $ deriveTransits' dt dur sess

> deriveTransits' :: DateTime -> Minutes -> Session -> [DateTime]
> deriveTransits' dt dur sess
>     | dur <= 0    = []
>     | otherwise   = lstHours2utc dt (rad2hrs . ra $ sess) :  deriveTransits' (1440 `addMinutes` dt) (dur - 1440) sess

Convert candidates to telescope periods relative to a given startime.
Remember: Candidates have unitless times, and their scores are cumulative.
Our Periods need to have Minutes, and average scores.

> toPeriod              :: DateTime -> DateTime -> Candidate Session -> Period
> toPeriod dt dtw candidate = defaultPeriod {
>     session      = cId candidate
>   , startTime    = (quarter * cStart candidate) `addMinutes` dt
>   , duration     = quarter * cDuration candidate
>   , pScore       = (cScore candidate)/(fromIntegral $ cDuration candidate)
>   , pForecast    = dtw -- the weather origin used to create this period
>   , pBackup      = False
>   , pDuration    = quarter * cDuration candidate
>   }

Candidates, importantly, don't care what unit of time we're working
with.  Both `cStart` and `cDuration` are simply in "units."

> data Candidate a = Candidate {
>     cId       :: !a
>   , cProj     :: !Int
>   , cStart    :: !Int
>   , cDuration :: !Int 
>   , cScore    :: !Score
>   } deriving (Eq, Show)

> defaultCandidate = Candidate {
>     cId       = 0
>   , cProj     = 0
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

Story: https://www.pivotaltracker.com/story/show/14219719
When scheduling is sparse, and there are lots of Nothing's in the input to this
function, the optimal schedule is not being picked because we are always picking
the last candidate, which may NOT necessarily have the largest accumulated score!

This function must traverse the list of candiates exactly as queryPast does.

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
>   , iSessId  :: !Int
>   , iProj    :: !Int
>   , iMinDur  :: !Int
>   , iMaxDur  :: !Int
>   , iOvhdDur :: !Int
>   , iSTimAv  :: !Int
>   , iPTimAv  :: !Int
>   , iTimeBt  :: !Int
>   , iTrType  :: !TransitType
>   , iTrnsts  :: ![Int]
>   , iFuture  :: ![Score]
>   , iPast    :: ![Score]
>   } deriving (Eq, Show)

> defaultItem = Item {
>     iId      = 0
>   , iSessId  = 0
>   , iProj    = 0 
>   , iMinDur  = 0 
>   , iMaxDur  = 0 
>   , iOvhdDur = 0 
>   , iSTimAv  = 0 
>   , iPTimAv  = 0 
>   , iTimeBt  = 0 
>   , iTrType  = Optional 
>   , iTrnsts   = []
>   , iFuture   = []
>   , iPast     = []
>   }

A convenient way of displaying Items, since we often don't want to see the entire
session.  However, this string can be used to cut and past into unit tests to 
create new Items.  Note: trying to create an instance of Show Item causes complications
since it's a parameterized data structure.

> showItem :: Item Session -> String
> showItem i = "Item {iId = defaultSession {sId = " ++ ssId ++ "}, iProj = " ++ ssId ++ ", iMinDur = " ++ sMin ++", iMaxDur = " ++ sMax ++ ", iOvhdDur = 1, iSTimAv = " ++ sS ++ ", iPTimAv = " ++ sP ++ ", iTimeBt = " ++ sTB ++ ", iTrType = Optional, iTrnsts = [], iFuture = " ++ futures ++ ", iPast = []}"
>     where
>       ssId = show . sId . iId $ i
>       sMin = show . iMinDur $ i
>       sMax = show . iMaxDur $ i
>       sS  = show . iSTimAv $ i
>       sP  = show . iPTimAv $ i
>       sTB = show . iTimeBt $ i
>       futures = show . iFuture $ i

The function *candidates*, defined below, generates a series of candidates
representing all the possible periods which end at a specified time
(t(n)) and start at incrementally times earlier, i.e., if the scheduling
time starts at t(0) then we need the accumulated scores for periods
covering times:
[t(n), t(n-1) to t(n), t(n-2) to t(n), ..., t(0) to t(n)]
Actually, because sessions have a maximum length, the problem is
simpler. So in the above case, if the session has a maximum length of 4
quarters then we need the accumulated scores for periods covering times:
[t(n), t(n-1) to t(n), t(n-2) to t(n), t(n-3) to t(n)]

The value past in *candidates* contains the individual quarterly scores
reversed, i.e., the score for t(n), then the score for t(n-1), the score
for t(n-2), ..., finally the score for t(0).

So, let past = [a, b, c, d .. x, y, z] where z is the score for the first
15 minutes after t(0), y is the score the second 15 minutes, ..., and
a is the score for t(n).  Also, let the max/min be 4/8,
then past' containing the accumulated scores from past is:
              [a,        sum(a,b), sum(a,c), sum(a,d),
               sum(a,e), sum(a,f), sum(a,g), sum(a,h)]
where, for example, sum(a,c) = a + b + c

The first accumulation, a, represents the raw score at t(n).  Likewise,
the last accumulation, sum(a,h), represents the earliest accumulation,
i.e., 2:00 hours before t(n) and 4:30 after t(0).  Therefore, these
accumulations represent the last 15 minutes, the last 30 minutes,
etc. before t(n) respectively.

These accumulations are backward because we need to find the score for
possible periods from a specified time to an earlier times to allow the
dynamic scheduling needed by the Knapsack Algorithm.

To accomodate overhead, we can force the accumulations to act as if
the first quarter or two of each possible period is zero by dropping
the last accumulation(s) and prefixing zero(s) to past' causing the
accumulations to be offset.  If the overhead offset is one then the
accumulation used for the last 15 minutes (earliest chronologically)
is zero, the accumulation previously used for the last 15 minutes get
used for the last 30 minutes, 30 minutes get used for 45 minutes, etc.;
past'' is:
             [0,        a,        sum(a,b), sum(a,c),
              sum(a,d), sum(a,e), sum(a,f), sum(a,g)]

Now we need to tag each accumulation as to whether or not it is viable
or not using the Maybe monad.  The first min - 1 accumulations are
represented by Nothing since they represent periods of length less than
minimum allowed, and the remaining are represented by Just values, i.e.,

[Nothing,       Nothing,       Nothing,       Just sum(a,c),
 Just sum(a,d), Just sum(a,e), Just sum(a,f), Just sum(a,g)]

The is list contains only 8 items because any following items would
be Nothing since the maximum length is 8.

Analyzing the fourth score, Just sum(a,c): it represents 4 quarters
starting at t(n) - 1:00 whose raw quarterly scores were
d, c, b, and a.  But we need to score the first quarter as 0,
so essentially the quarterly scores are c, b, a which are indeed
the scores being summed in Just sum(a,c).

This result is passed to toCandidate to create a list of candidates
with the appropriate scores and durations.  (Note the code itself is
a bit shorter.)

> candidates               :: Item a -> [Maybe (Candidate a)]
> candidates Item { iId = id, iMinDur = min, iMaxDur = max
>                 , iOvhdDur = ovhd, iPast = past }
>     | max == 0           = []
>     | lpast' < min       = []
>     | otherwise          = toCandidate id $ replicate min' Nothing ++ (map Just $ drop min' past'')
>   where
>     min'   = min - 1
>     past'  = accScores ovhd . take max $ past
>     lpast' = length past'
>     past'' = (replicate ovhd 0.0) ++ (take (lpast'-ovhd) past')

> accScores :: Int -> [Score] -> [Score]
> accScores ovhd = scanl1 (+) . takeWhilen ovhd (>= epsilon)

Take from a list while the predicate is true and then include n more

> takeWhilen :: Int -> (a -> Bool) -> [a] -> [a]
> takeWhilen _ _ []       =  []
> takeWhilen n p (x:xs) 
>             | p x       =  if n == 0
>                            then []
>                            else x : takeWhilen n p xs
>             | otherwise = if n == 2
>                           then x : takeWhilen 0 p xs
>                           else [x]

Given a proposed 'item' for a slot and a list of candidates representing
all the previous slots ('past'), return the count of previously 'sUsed' time
for the item, 'pUsed' time for its project, and the 'separate'ion between
the current item's period and any previous periods.  Note the returned
separate value only applies if a previous candidate of item was found, i.e.,
'sUsed' > 0, otherwise it is set to (-1). This function must traverse the
list of candiates exactly as unwind does.

> queryPast :: (Eq a) => Item a -> [Maybe (Candidate a)] -> Int -> (Int, Int, Int, [Int])
> queryPast item past dur = queryPast' (0,     0,     (-1),       []) item . drop (dur - 1) $ past

> queryPast' :: (Eq a) => (Int, Int, Int, [Int]) -> Item a -> [Maybe (Candidate a)] -> (Int, Int, Int, [Int])
> -- Sentinel signals termination, already got the answer, so just return
> -- the tuple.
> queryPast' (sUsed, pUsed, separate, steps) item [Nothing]       = (sUsed, pUsed, separate, steps)
> -- No candidate, skip it noting the hole in the steps field of the return tuple.
> queryPast' (sUsed, pUsed, separate, steps) item (Nothing:cs)    = queryPast' (sUsed, pUsed, separate, 1:steps) item cs
> -- Two candiates in a row, use the one with the larger score, i.e., update
> -- the return tuple and continue.
> queryPast' (sUsed, pUsed, separate, steps) item cs@(Just c : cs'@(Just c' : _))
>         | cScore c' > cScore c  = queryPast' (sUsed,  pUsed,  separate,     1:steps) item cs' 
>         | otherwise             = queryPast' (sUsed', pUsed', separate', step:steps) item (drop step cs)
>   where
>     step = min ((length cs) - 1) dur
>     dur  = cDuration c
>     itemSId = iId item
>     candidateSId = cId c
>     itemPId = iProj item
>     candidatePId = cProj c
>     pUsed' | itemPId == candidatePId   = dur + pUsed
>            | otherwise                 = pUsed
>     sUsed' | itemSId == candidateSId   = dur + sUsed
>            | otherwise                 = sUsed
>     separate' | itemSId == candidateSId   = if separate == (-1) then sum steps else separate
>               | otherwise                 = separate
> -- Default case: have a candidate followed by whatever, update the
> -- return tuple and continue.
> queryPast' (sUsed, pUsed, separate, steps) item cs@(Just c : _) =
>                                   queryPast' (sUsed', pUsed', separate', step:steps) item (drop step cs)
>   where
>     step = min ((length cs) - 1) dur
>     dur  = cDuration c
>     itemSId = iId item
>     candidateSId = cId c
>     itemPId = iProj item
>     candidatePId = cProj c
>     pUsed' | itemPId == candidatePId   = dur + pUsed
>            | otherwise                 = pUsed
>     sUsed' | itemSId == candidateSId   = dur + sUsed
>            | otherwise                 = sUsed
>     separate' | itemSId == candidateSId   = if separate == (-1) then sum steps else separate
>               | otherwise                 = separate

Given possible scores (using Maybe) over a range of a session's durations,
generate a list of possible candidates corresponding to the scores.
Note the use of fmap: here we are transforming Scores into Candidates 
inside the Maybe Monad.
Example (continued from above): toCandidate 1 [Nothing, Just 2] gives:
[ Nothing, Just (Candidate 1 0 2 2) ]

> toCandidate           :: a -> [Maybe Score] -> [Maybe (Candidate a)]
> toCandidate id scores = [fmap (Candidate id 0 0 d) s | s <- scores | d <- [1..]]

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

> packWorker :: Eq a => [Maybe (Candidate a)] -> [Item a] -> [Candidate a]
> packWorker future items = unwind . packWorker' 0 future [Nothing] . map step $ items

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
problems.  For our case, that means first solving the 15-min schedule,
then using this solution to solve for the 30-min schedule, and so on.
The solutions to our sub-problems are represented by the 'past' param.
Note that cStart in the result is not defined, this occurs in unwind.

> packWorker' :: Eq a => Int -> [Maybe (Candidate a)] -> [Maybe (Candidate a)] -> [Item a] -> [Maybe (Candidate a)]
> -- packWorker' dur  future             past sessions
> packWorker'    _    []                 past _        = past 
> packWorker'    dur  (Just b  : future) past sessions =
>     packWorker' (dur + 1) future (Just b:past) $! map (step . forget) sessions
> packWorker'    dur (Nothing : future) past sessions =
>     let b = getBest dur past sessions in
>     (if maybe 0.0 cScore b >= 0.0 then True else False) `seq` packWorker' (dur + 1) future (b:past) $! map step sessions

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

> getBest ::  Eq a => Int -> [Maybe (Candidate a)] -> [Item a] -> Maybe (Candidate a)
> getBest dur past sessions = best $ map (bestCandidateOfASession dur past) sessions    

> bestCandidateOfASession :: Eq a => Int -> [Maybe (Candidate a)] -> Item a -> Maybe (Candidate a)
> bestCandidateOfASession dur past sess = best . transitChecks sess dur . zipWith madd (filterCandidates sess past $ candidates sess) $ past

We need apply certain constraints inside the packing algorithm.  For example,
time remaining, and time between must be obeyed as the candidates for packing
are constructed.  This function will apply these types of contraints to
a list of candidates, such that those candidates that violate the constraints
are replaced by Nothing.

> filterCandidates :: Eq a => Item a -> [Maybe (Candidate a)] -> [Maybe (Candidate a)] -> [Maybe (Candidate a)]
> filterCandidates item past cs = map (filterCandidate item past) cs 

A candidate gets replaced with Nothing if:
   * the time in the past, plus the time the candidate is testing to put on 
   the schedule, is greater than it's available time
   * the duration of the candidate puts it in close (too close) proximity to another period of the same session (time between constraint).

> filterCandidate :: Eq a => Item a -> [Maybe (Candidate a)] -> Maybe (Candidate a) -> Maybe (Candidate a)
> filterCandidate    _    _ Nothing                      = Nothing
> filterCandidate item past (Just c) | iId item == cId c = rejectCandidate
>                                    | otherwise         = Just c
>   where
>     (sUsed, pUsed, sep, hist) = queryPast item past (cDuration c)
>     dur = cDuration c
>     rejectCandidate = if ((sUsed + dur) > (iSTimAv item)) ||
>                          (sUsed > 0 && sep < (iTimeBt item)) ||
>                          ((pUsed + dur) > (iPTimAv item))
>                       then Nothing
>                       else Just c

Given an item and a list of its possible candidates (cs) for a given scheduling
duration (dur), eliminate any candidate in the list by substituting Nothing
if it does not provide sufficient coverage of the item's transit.

> transitChecks :: Eq a => Item a -> Int -> [Maybe (Candidate a)] -> [Maybe (Candidate a)]
> transitChecks item dur cs = map (transitCheck item dur) cs

> transitCheck :: Eq a => Item a -> Int -> Maybe (Candidate a) -> Maybe (Candidate a)
> transitCheck    _   _  Nothing                 = Nothing
> transitCheck item dur (Just c)
>     | (iTrType item) == Optional || inTransit  = Just c
>     | otherwise                                = Nothing
>   where
>     factor = if (iTrType item) == Partial
>              then 4 -- 25% criterion
>              else 2 -- 50% criterion
>     cDur = cDuration c
>     span = flip div factor cDur
>     lower t = t + span
>     upper' t = t - span + cDur
>     -- fudge to handle Center candidates with even number of quarters
>     upper t = if (iTrType item) == Center && (even cDur)
>               then (upper' t) + 1
>               else (upper' t)
>     inTransit = any (\t -> (lower t) <= dur && dur < (upper t)) (iTrnsts item)

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


