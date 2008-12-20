> {-# OPTIONS -XParallelListComp #-}

> module Antioch.Schedule.Pack where

> import Antioch.DateTime
> import Antioch.Score
> import Antioch.Types
> import Data.List  (foldl', sort)
> import Data.Maybe (fromMaybe)

> stepSize = 15      :: Minutes
> epsilon  =  1.0e-6 :: Score

> numSteps :: Minutes -> Int
> numSteps = (`div` stepSize)

> pack :: ScoreFunc -> DateTime -> Minutes -> [Period] -> [Session] -> Scoring [Period]
> pack sf dt dur periods sessions = do
>     items <- mapM (toItem sf mask) sessions
>     return . map toPeriod . packWorker (toSchedule dts periods) $ items
>   where
>     dts   = scanl (flip addMinutes') dt [stepSize * m | m <- [0 .. numSteps dur]]
>     sched = toSchedule dts . sort $ periods
>     mask  = zipWith (\dt s -> fmap (const dt) s) dts sched

> toSchedule         :: [DateTime] -> [Period] -> [Maybe (Candidate Session)]
> toSchedule []  _   = []
> toSchedule dts []  = map (const Nothing) dts
> toSchedule dts@(dt:dts') ps@(p:ps')
>     | dt  <  begin = Nothing : toSchedule dts' ps
>     | end <= dt    = toSchedule dts ps'
>     | otherwise    = Just (Candidate (session p) (numSteps . duration $ p) (pScore p)) : toSchedule dts' ps
>   where
>     begin = startTime p
>     end   = addMinutes' (duration p) begin

> toItem          :: ScoreFunc -> [Maybe DateTime] -> Session -> Scoring (Item Session)
> toItem sf dts s = do
>     scores <- mapM (fromMaybe (return 0.0) . fmap (fmap eval . flip sf s)) dts
>     return $ Item {
>         iId      = s
>       , iMinDur  = numSteps . minDuration $ s
>       , iMaxDur  = numSteps . maxDuration $ s
>       , iFuture  = scores
>       , iPast    = []
>       }

> toPeriod           :: Candidate Session -> Period
> toPeriod candidate = defaultPeriod {
>     session = cId candidate
>   }

> data Candidate a = Candidate {
>     cId       :: a
>   , cDuration :: Int
>   , cScore    :: Score
>   } deriving (Eq, Show)

> defaultCandidate = Candidate {
>     cId       = 1
>   , cDuration = 1
>   , cScore    = 0.0
>   }

> unwind    :: [Maybe (Candidate a)] -> [Candidate a]
> unwind xs = [ x { cScore = y } | x <- xs' | y <- ys' ]
>   where
>     xs' = unwind' [] xs
>     ys  = map cScore xs'
>     ys' = [ y' - y | y <- 0 : ys | y' <- ys ]
>     unwind' acc []             = acc
>     unwind' acc (Nothing : xs) = unwind' acc xs
>     unwind' acc (Just x  : xs) = unwind' (x:acc) . drop (cDuration x - 1) $ xs

> data Item a = Item {
>     iId      :: a
>   , iMinDur  :: Int
>   , iMaxDur  :: Int
>   , iFuture  :: [Score]
>   , iPast    :: [Score]
>   }

> candidates               :: Item a -> [Maybe (Candidate a)]
> candidates Item { iId = id, iMinDur = min, iMaxDur = max, iPast = past }
>     | length past' < min = []
>     | otherwise          = toCandidate id $ replicate (min-1) Nothing ++ (map Just . drop (min-1) $ past')
>   where
>     past' = acc . takeWhile (>= epsilon) . take max $ past
>     acc   = scanl1 (+)

> toCandidate           :: a -> [Maybe Score] -> [Maybe (Candidate a)]
> toCandidate id scores = [fmap (Candidate id d) s | s <- scores | d <- [1..]]

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
>     packWorker' future (Just b:past) . map (step . forget) $ sessions
> packWorker' (Nothing : future) past sessions =
>     packWorker' future (b:past) . map step $ sessions
>   where
>     b = best . map (\s -> best . zipWith madd (candidates s) $ past) $ sessions

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
> madd (Just c1) (Just c2) = Just $ c1 { cScore = cScore c1 + cScore c2 }
