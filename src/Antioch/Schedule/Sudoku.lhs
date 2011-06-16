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

> module Antioch.Schedule.Sudoku where

> import Antioch.DateTime
> import Antioch.Types   hiding (Session)
> import Control.Monad.ST
> import Data.Array.ST
> import Data.Array.Unboxed
> import Data.Function  (on)
> import Data.List
> import Data.Maybe     (mapMaybe)
> import GHC.Exts       (groupWith, sortWith)
> import System         (getArgs)
> import Test.QuickCheck
> import qualified Antioch.Types as T

This top-level driver function is responsible for converting between Antioch's
`Session` and `Period` types and the `Allocation` and `Interval` types used
internally by the sudoku code.

> schedule                 :: DateTime -> Minutes -> [T.Session] -> [Period]
> schedule dt dur sessions = toPeriods dt (dur `addMinutes` dt) . sort $ allocs3
>   where
>     allocs1 = concatMap toAllocation sessions
>     allocs2 = ofInterest dt dur allocs1
>     allocs3 = schedule' allocs2

> ofInterest dt dur allocs = allocs

> toPeriods _     _   []         = []
> toPeriods start end (a:as)
>     | mustSchedule start end a = toPeriod a x : (toPeriods start end . nuke' x $ as)
>     | otherwise                = toPeriods start end as
>   where
>     x = head . intervals $ a

We `mustSchedule` an allocation within a certain time period if it does not
contain any intervals beyond the end of that time period.

> mustSchedule  start end       = mustSchedule' start end . intervals
> mustSchedule' _     _   []    = True
> mustSchedule' start end ((Interval s e):xs)
>     | end <= s                = False
>     | otherwise               = mustSchedule' start end xs

> schedule'     :: (Ord a, Num t, Ord t) => [Allocation a t] -> [Allocation a t]
> schedule' [x] = [x]
> schedule' xs  = case sudoku xs of
>     Nothing  -> []
>     Just xss -> aggregate . concat $ xss

> data (Eq a, Ord t) => Allocation a t =
>     Allocation { akey :: a, intervals :: [Interval t] } deriving Eq

Effectively, the default sort on a set of allocations sorts on the start time
of their first intervals.

> instance (Eq a, Ord t) => Ord (Allocation a t) where
>     compare = compare `on` (head . intervals)

> toAllocation s@(Open     { }) = []
> toAllocation s@(Fixed    { }) = [Allocation s [toInterval . period $ s]]
> toAllocation s@(Windowed { }) = [Allocation s (map toInterval . opportunities $ s)]

> data Ord t => Interval t = Interval t t deriving (Eq, Ord)

> toInterval Period { startTime = dt, duration = dur } =
>     Interval dt (dur `addMinutes` dt)

> toPeriod (Allocation id _) (Interval s e) =
>     Period id s (e `diffMinutes` s) 0.0

> spanIntervals    :: (Num t, Ord t) => [Interval t] -> t
> spanIntervals xs = e - s
>   where
>     Interval s _ = head xs
>     Interval _ e = last xs

> mergeIntervals :: Ord t => [Interval t] -> [Interval t]
> mergeIntervals = reverse . foldl' merge []
>   where
>     merge [] x = [x]
>     merge as@(a@(Interval s1 e1):as') x@(Interval s2 e2)
>       | a `conflict` x = Interval (min s1 s2) (max e1 e2) : as'
>       | otherwise      = x : as

> mkAllocation i = Allocation i . nub . sort

> spanAllocation                   :: (Eq a, Num t, Ord t) => Allocation a t -> t
> spanAllocation (Allocation _ xs) = spanIntervals xs

> data (Eq a, Ord t) => PowerSet a t =
>     PowerSet { used :: [Interval t], allocs :: [Allocation a t] }

> raise                     :: (Eq a, Ord t) => Allocation a t -> PowerSet a t
> raise a@(Allocation i ts) = PowerSet (mergeIntervals ts) [a]

> combine    :: (Eq a, Ord t) => [PowerSet a t] -> PowerSet a t
> combine xs = PowerSet newUsed newAllocs
>   where
>     newUsed   = mergeIntervals . sort . concatMap used $ xs
>     newAllocs = concatMap allocs xs
    
> partitionP :: (Eq a, Ord t) => [PowerSet a t] -> [PowerSet a t]
> partitionP = foldl' merge []
>   where
>     merge [] x = [x]
>     merge ps x =
>         if n > 5 then ps else combine (x:as) : bs
>       where
>         (as, bs) = partition (x `conflict`) ps
>         n        = (1 +) . sum . map (\(PowerSet _ as) -> length as) $ as

> data (Eq a, Ord t) => Session a t =
>     Session { skey :: a, sint :: Interval t } deriving (Eq, Ord)
  
> type Schedule a t = [Session a t]

> flatten                   :: (Eq a, Ord t) => Allocation a t -> [Session a t]
> flatten (Allocation i ts) = [Session i t | t <- ts]

> aggregate :: (Ord a, Ord t) => [Session a t] -> [Allocation a t]
> aggregate = map collect . groupWith skey . sortWith skey
>   where
>     collect                        :: (Eq a, Ord t) => [Session a t] -> Allocation a t
>     collect xs@((Session i _) : _) = mkAllocation i . map sint $ xs

> prop_Flatten = forAll genAllocation $ \x ->
>    (not . null . intervals $ x) ==> aggregate (flatten x) == [x]

> prop_Aggregate = forAll genSessions $ \xs ->
>     let ys = nub xs in sort (concatMap flatten . aggregate $ ys) == sort ys

> genInterval = sized $ \n -> do
>     start    <- choose (0, n)
>     duration <- choose (1, n)
>     return $ Interval start (start + duration)

> genIntervals = plural genInterval

> plural g = sized $ \n -> choose (0, n) >>= sequence . flip replicate g

> genAllocation = do
>     name <- arbitrary :: Gen String
>     fmap (mkAllocation name) genIntervals
    
> instance Arbitrary Char where
>     coarbitrary _ = variant 0
>     arbitrary     = choose ('a', 'z')

> genSessions = fmap (concatMap flatten) $ plural genAllocation

> genAllocations = fmap aggregate genSessions

> class Span a where
>     conflict :: a -> a -> Bool

> instance Ord t => Span (Interval t) where
>     (Interval s1 e1) `conflict` (Interval s2 e2) = s1 < e2 && s2 < e1

> prop_Overlap =
>     forAll genInterval $ \x@(Interval s1 e1) ->
>     forAll genInterval $ \y@(Interval s2 e2) ->
>       not (x `conflict` y) ==> e1 <= s2 || e2 <= s1
        
> instance (Eq a, Ord t) => Span (Allocation a t) where
>     (Allocation _ xs) `conflict` (Allocation _ ys) = xs `conflictIntervals` ys

> instance (Eq a, Ord t) => Span (Session a t) where
>     (Session _ x) `conflict` (Session _ y) = x `conflict` y

> instance (Eq a, Ord t) => Span (PowerSet a t) where
>     (PowerSet xs _) `conflict` (PowerSet ys _) = xs `conflictIntervals` ys

> conflictIntervals       :: Ord t => [Interval t] -> [Interval t] -> Bool
> conflictIntervals [] _  = False
> conflictIntervals _  [] = False
> conflictIntervals xs@((Interval s1 e1):xs') ys@((Interval s2 e2):ys')
>   | s1 < e2 && s2 < e1 = True
>   | e1 <= s2           = conflictIntervals xs' ys
>   | e2 <= s1           = conflictIntervals xs  ys'

> sudoku'                          :: (Eq a, Num t, Ord t) => [Allocation a t] -> Maybe [Schedule a t]
> sudoku' []                       = Just [[]]
> sudoku' ((Allocation i []) : _ ) = Nothing
> sudoku' ((Allocation i ts) : xs) =
>     case flip mapMaybe ts $ \t -> fmap (map (Session i t :)) . sudoku' . nuke t $ xs of
>       [] -> Nothing
>       rs -> Just $! concat rs

> nuke       :: (Eq a, Num t, Ord t) => Interval t -> [Allocation a t] -> [Allocation a t]
> nuke  t    = fewestFirst . biggestFirst . nuke' t

> nuke' t xs = [Allocation i . nukeInterval t $ x | Allocation i x <- xs]

> nukeInterval       :: (Num t, Ord t) => Interval t -> [Interval t] -> [Interval t]
> nukeInterval _ []  = []
> nukeInterval t@(Interval _ e) xs@(x@(Interval s _):xs')
>   | e <= s         = xs
>   | t `conflict` x = nukeInterval t xs'
>   | otherwise      = x : nukeInterval t xs'

> fewestFirst :: (Eq a, Ord t) => [Allocation a t] -> [Allocation a t]
> fewestFirst = sortByKey lengthCmp intervals

> biggestFirst :: (Eq a, Num t, Ord t) => [Allocation a t] -> [Allocation a t]
> biggestFirst = reverse . sortByKey intervalCmp intervals
>   where
>     intervalCmp []                   []                   = EQ
>     intervalCmp []                   _                    = LT
>     intervalCmp _                    []                   = GT
>     intervalCmp ((Interval s1 e1):_) ((Interval s2 e2):_) = (e1 - s1) `compare` (e2 - s2)

> sudoku :: (Eq a, Num t, Ord t) => [Allocation a t] -> Maybe [Schedule a t]
> sudoku = fmap concat . mapM sudoku' . sortWith length . partition'

> prop_Valid = forAll genAllocations $ \allocations ->
>     flip all (partition' allocations) $ \xs ->
>       case sudoku' xs of
>         Nothing        -> True
>         Just schedules -> all (isValid . length $ xs) schedules
>   where
>     isValid len schedule =
>       len == length schedule                   &&
>       len == length (groupWith skey schedule)  &&
>       len == length (transitiveClosure conflict schedule)

> --partition' :: Span a => [a] -> [[a]]            
> --partition' = transitiveClosure conflict
> partition' :: (Eq a, Ord t) => [Allocation a t] -> [[Allocation a t]]
> partition' = map allocs . partitionP . map raise

> transitiveClosure      :: (a -> a -> Bool) -> [a] -> [[a]]
> transitiveClosure p xs = subsets `seq` [[xs!!i | i <- subset] | subset <- subsets]
>   where
>     n         = length xs
>     connected = floydWarshall p xs
>     subsets   = connected `seq` closure [0..n-1]
>     closure []     = []
>     closure (i:js) = (i:hits) : closure misses
>       where
>         (hits, misses) = partition (\j -> connected!(i, j)) js

> prop_Cover    :: [Int] -> Bool
> prop_Cover xs = all (\x -> any (x `elem`) xss) xs
>   where
>     xss = transitiveClosure (==) xs

> prop_Disjoint    :: [Int] -> Bool
> prop_Disjoint xs = and [not . any (x `elem`) $ xss \\ [xs] | xs <- xss, x <- xs]
>   where
>     xss = transitiveClosure (==) xs

> prop_Disjoint' = forAll genIntervals $ \xs ->
>     let xss = transitiveClosure conflict xs in
>     and [not . any (x `conflict`) $ ys | xs <- xss, ys <- xss \\ [xs], x <- xs]

> floydWarshall       :: (a -> a -> Bool) -> [a] -> UArray (Int, Int) Bool
> floydWarshall  p xs = runSTUArray $ floydWarshall' p xs

> floydWarshall'      :: (a -> a -> Bool) -> [a] -> ST s (STUArray s (Int, Int) Bool)
> floydWarshall' p xs = do
>     arr <- newArray ((0, 0), (n-1, n-1)) False
>     for idx $ \i ->
>       for idx $ \j ->
>         writeArray arr (i, j) $ p (xs!!i) (xs!!j)
>     for idx $ \k ->
>       for idx $ \i ->
>         for idx $ \j -> do
>           z <- readArray arr (i, j)
>           x <- readArray arr (i, k)
>           y <- readArray arr (k, j)
>           writeArray arr (i, j) $! z || (x && y)
>     return arr
>   where
>     for xs f = foldr ((>>) . f) (return ()) xs
>     n   = length xs
>     idx = [0..n-1]

> prop_Idempotent = forAll genAllocations $ \xs ->
>     schedule' xs == schedule' (schedule' xs)

> prop_Schedulable = forAll genAllocations $ \xs ->
>     canSchedule (fewestFirst . schedule' $ xs)
>   where
>     canSchedule []                           = True
>     canSchedule ((Allocation _ []    ) : _ ) = False
>     canSchedule ((Allocation _ (t:ts)) : xs) = canSchedule . nuke t $ xs

> sortByKey cmp key = sortBy $ \x y -> key x `cmp` key y

> split _ []      = [[]]
> split d (c:cs)
>     | d == c    = []    : rs
>     | otherwise = (c:r) : rs'
>   where
>     rs@(r:rs') = split d cs

> []     `lengthCmp` []     = EQ
> []     `lengthCmp` _      = LT
> _      `lengthCmp` []     = GT
> (x:xs) `lengthCmp` (y:ys) = xs `lengthCmp` ys

> instance (Ord t, Show t) => Show (Interval t) where
>     show (Interval s e) = show s ++ '-' : show e

> instance (Eq a, Show a, Ord t, Show t) => Show (Session a t) where
>     show (Session name interval) = show name ++ ' ' : show interval

> instance (Eq a, Show a, Ord t, Show t) => Show (Allocation a t) where
>     show (Allocation name intervals) =
>         trim (show name) ++ ' ' : (concat . intersperse "," . map show $ intervals)
>       where
>         trim = t . t where t = drop 1 . reverse

> instance (Eq a, Show a, Ord t, Show t) => Show (PowerSet a t) where
>     show (PowerSet _ as) = unlines . map show $ as
