> module Antioch.Schedule.Pack where

> import Antioch.DateTime
> import Antioch.Types

> stepSize = 15

> pack :: ScoreFunc -> DateTime -> Minutes -> [Period] -> [Session] -> Scoring [Period]
> pack sf dt dur periods sessions = do
>     items <- mapM (toItem sf dts) sessions
>     return $ toPeriod . packWorker (toSchedule dts periods) $ items
>   where
>     dts = scanl (flip addMinutes) dt [stepSize * m | m <- [0 .. dur `div` stepSize]

> toItem          :: ScoreFunc -> [DateTime] -> Session -> Scoring Item
> toItem sf dts s = do
>     scores = mapM (flip sf s) dts
>     return $ Item {
>         iId     = id s
>       , iMinDur = minDuration s `div` stepSize
>       , iMaxDur = maxDuration s `div` stepSize
>       , iFuture = scores
>       , iPast   = []
>       }

> toPeriod :: Candidate -> Period

> toSchedule :: [DateTime] -> [Period] -> [Maybe Candidate]

> data Candidate = Candidate {
>     cId       :: Int
>   , cDuration :: Int
>   , cScore    :: Score
>   }

> unwind :: [Maybe Candidate] -> [Candidate]

> data Item = Item {
>     iId     :: Int
>   , iMinDur :: Int
>   , iMaxDur :: Int
>   , iFuture :: [Score]
>   , iPast   :: [Score]
>   }

> candidates               :: Item -> [Maybe Candidate]
> candidates Item { iId = id, iMinDur = min, iMaxDur = max, iPast = past }
>     | length past' < min = []
>     | otherwise          = toCandidate id $ replicate min Nothing ++ (map Just . drop min $ past')
>   where
>     past' = acc . takeWhile (>= epsilon) . take max $ past
>     acc   = scanl1' (+)

> epsilon :: Score
> epsilon = 1.0e-6

> toCandidate           :: Int -> [Maybe Score] -> [Maybe Candidate]
> toCandidate id scores = [fmap (Candidate id d) s | s <- scores | d <- [1..]]

Move a score from the future to the past, so that it can now be
scheduled.

> step :: Item -> Item
> step item@(Item { iFuture = [],     iPast = past }) = item {               iPast = 0 : past }
> step item@(Item { iFuture = (f:fs), iPast = past }) = item { iFuture = fs, iPast = f : past }

Forget the past.  Used when we've just encountered a pre-scheduled
block that we know we won't be scheduling across.

> forget      :: Item -> Item
> forget item = item { iPast = [] }

> packWorker        :: [Maybe Candidate] -> [Item] -> [Candidate]
> packWorker future = unwind . packWork' future [Nothing] . map step

> packWorker' :: [Maybe Candidate] -> [Maybe Candidate] -> [Item] -> [Maybe Candidate]
> packWorker' []                 past _        = past
> packWorker' (Just b  : future) past sessions =
>     packWork' future (b:past) . map (step . forget) $ sessions
> packWorker' (Nothing : future) past sessions =
>     packWork' future (b:past) . map step $ sessions
>   where
>     b = best . map (\s -> best . zipWith madd (candidates s) $ past) $ sessions

Find the best of a collection of candidates.

> best    :: [Maybe Candidate] -> Maybe Candidate
> best [] = Nothing
> best xs = foldl' better xs

Find the better of two candidates.

> better                      :: Maybe Candidate -> Maybe Candidate -> Maybe Candidate
> better Nothing   c2         = c2
> better c1        Nothing    = c1
> better (Just c1) (Just c2)
>     | cScore c1 > cScore c2 = Just c1
>     | otherwise             = Just c2

Add a candidate to a historical value to produce a new candidate.

> madd                     :: Maybe Candidate -> Maybe Candidate -> Maybe Candidate
> madd Nothing   _         = Nothing
> madd c1        Nothing   = c1
> madd (Just c1) (Just c2) = Just $ c1 { cScore = cScore c1 + cScore c2 }
