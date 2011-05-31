> {-# OPTIONS -XMultiParamTypeClasses -XScopedTypeVariables #-}

> module Antioch.Utilities where

> import Antioch.DateTime
> import Antioch.Types
> import qualified Antioch.SLALib as SLA
> import Test.QuickCheck hiding (frequency)
> import Data.Convertible
> import Data.Maybe             (isJust, fromJust)
> import Data.List              (elemIndex)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Text.Printf            (printf)

                              GBT data
--------------------------------------------------------------------

Based on analysis of a "TIES" VLBI run with the GBT and GB 20-meter
(December 2002)
see http://www.gb.nrao.edu/~fghigo/gbtdoc/vlbinfo.html#position

> gbtLat, gbtLatD, gbtLong, gbtLongD, gbtAlt :: Double
> gbtLat   = 0.67078465 -- radians
> gbtLat' = (realToFrac gbtLat)::Radians
> gbtLatD  = 38.433129  -- degrees
> gbtLong  = -1.3934683 -- radians
> gbtLongD = -79.839840 -- degrees
> gbtAlt   = 855.46     -- meters

In the weather DB, the forecasts table frequency values (Forecast)
range from 2 - 120 GHz.
The t_sys and stringency tables frequency values (History)
are in MHz and are a subset of the set specified in freqIndices
as partitioned by the frequency ranges of the receivers
(rcvrRanges).

> freqIndices :: [Int]
> freqIndices = [100, 200 .. 900] ++ [1000, 2000 .. 51000] ++ [52000, 54000 .. 120000]

Ranges for all receivers: copied from the DSS database.

> rcvrRanges :: [(Receiver, (Frequency, Frequency))]
> rcvrRanges = [
>     (NoiseSource,    ( 0.0,  0.0)),
>     (Rcvr_RRI,       ( 0.1,  1.6)),
>     (Rcvr_342,       ( 0.29, 0.395)),
>     (Rcvr_450,       ( 0.385,0.52)),
>     (Rcvr_600,       ( 0.51, 0.69)),
>     (Rcvr_800,       ( 0.68, 0.92)),
>     (Rcvr_1070,      ( 0.91, 1.23)),
>     (Rcvr1_2,        ( 1.15, 1.73)),
>     (Rcvr2_3,        ( 1.73, 2.6)),
>     (Rcvr4_6,        ( 3.95, 6.1)),
>     (Rcvr8_10,       ( 8.0, 10.0)),
>     (Rcvr12_18,      (12.0, 15.4)),
>     (Rcvr18_26,      (18.0, 26.5)),
>     (Rcvr26_40,      (26.0, 39.5)),
>     (Rcvr40_52,      (38.2, 49.8)),
>     (Rcvr_PAR,       (80.0,100.0)),
>     (Zpectrometer,   ( 0.0,  0.0)),
>     (Holography,     (11.7, 12.2)),
>     (RcvrArray18_26, (17.0, 27.5)),
>     (Rcvr68_92,      (66.0, 94.0))]

--------------------------------------------------------------------

> deg2rad :: Float -> Float
> deg2rad deg = deg * pi / 180.0

> deg2rad' :: Double -> Double
> deg2rad' deg = deg * pi / 180.0

> rad2deg :: Float -> Float
> rad2deg rad = rad * 180.0 / pi

> rad2deg' :: Double -> Double
> rad2deg' rad = rad * 180.0 / pi

> rad2hrs :: Float -> Float
> rad2hrs x = 12 * x / pi

> deg2hrs :: Double -> Double
> deg2hrs d = d/15.0

> hrs2deg :: Float -> Float
> hrs2deg = (* 15.0)

> hrs2deg' :: Double -> Double
> hrs2deg' = (* 15.0)

> hrs2rad :: Float -> Float
> hrs2rad = deg2rad . hrs2deg

> hrs2rad' :: Double -> Double
> hrs2rad' = deg2rad' . hrs2deg'

> utc2lstHours    :: DateTime -> Float
> utc2lstHours = realToFrac . utc2lstHours'

> utc2lstHours'    :: DateTime -> Double
> utc2lstHours' dt = let
>     gmst = rad2deg' . SLA.gmst' . secondsToMJD' $ dt
>     gbls = deg2hrs $ gmst + gbtLongD
>     in if gbls < 0.0 then gbls + 24.0 else gbls

Translates a relative sidereal time (lst) at the given absolute solar time
(utc) to the equivalent absolute solar time.

> lstHours2utc :: DateTime -> Float -> DateTime
> lstHours2utc utc lst = lstHours2utc' utc (fromRational . toRational $ lst)

> lstHours2utc' :: DateTime -> Double -> DateTime
> lstHours2utc' utc lst
>     | lst < now = lstHours2utc' utc $ lst + 24.0
>     | otherwise = solarOffset `addSeconds` utc
>   where
>     now      = utc2lstHours' utc
>     solarTransform = 365.25 / 366.25
>     secondsPerHour = 60 * 60
>     solarOffset = floor ((lst - now) * solarTransform * secondsPerHour)

Frequency in GHz (float) to GHz (integer) 2 to 120

> freq2ForecastIndex' :: Frequency -> Int
> freq2ForecastIndex' = min 120 . max 2 . round

> freq2ForecastIndex :: Frequency -> Int
> freq2ForecastIndex f = if f' > 52 && odd f' then f' + 1 else f'
>   where
>     f' = freq2ForecastIndex' f

Frequency in GHz (float) to MHz (integer) 100 to 120,000 (sparsely)

> freq2HistoryIndex :: Receiver -> Frequency -> Int
> freq2HistoryIndex r ghz = approximate closest mhz rr
>   where
>     mhz = round . (*1000.0) $ ghz
>     rr  = getRcvrFreqIndices r

Find the y in yys closest, lowest, or highest relative to x,
note no guard against yys == [].

> approximate, approximate' :: (Ord a, Num a) => (a -> a -> a -> a) -> a -> [a] -> a
> approximate f x yys@(y:ys)
>     | x <= y                = y
>     | otherwise             = approximate' f x yys

> approximate' f x (y:ys)
>     | ys == []              = y
>     | x == y                = x
>     | x < hys               = f x y hys
>     | otherwise             = approximate' f x ys
>       where
>         hys = head ys

> closest, lowest, highest :: (Num a, Ord a) => a -> a -> a -> a
> closest x ylo yhi
>     | (yhi - x) > (x - ylo) = ylo
>     | otherwise             = yhi
> lowest x ylo yhi = ylo
> highest x ylo yhi = yhi

> getRcvrFreqIndices :: Receiver -> [Int]
> getRcvrFreqIndices rcvr = takeWhile (<=hi) . dropWhile (<lo) $ freqIndices
>   where
>     (lo', hi') = getRcvrRange rcvr
>     lo = approximate lowest (round . (*1000.0) $ lo') freqIndices
>     hi = approximate highest (round . (*1000.0) $ hi') freqIndices

Return a receiver's frequency range as a tuple

> getRcvrRange :: Receiver -> (Frequency, Frequency)
> getRcvrRange rcvr = if rngs == []
>                     then (0.0, 0.0)
>                     else snd . head $ rngs
>   where
>     rngs = filter (\r -> (fst r) == rcvr) rcvrRanges

The elevation range of the weather DB is from 5 - 90 degrees.

> elev2Index :: Radians -> Int
> elev2Index =  min 90 . max 5 . round . rad2deg

> between :: Ord a => a -> a -> a -> Bool
> between v min max = min <= v && v <= max

Is the datetime with a datetime/range?

> inTimeRange :: DateTime -> DateTime -> Minutes -> Bool
> inTimeRange dt start dur = start <= dt && dt < end
>   where
>     end = dur `addMinutes` start  

Does a period intersect a datetime/range?

> overlie :: DateTime -> Minutes -> Period -> Bool
> overlie start dur p = ps < e && s < pe
>   where
>     ps = startTime p
>     pe = periodEndTime p
>     s = start
>     e = dur `addMinutes` start  

Does a period lie within a datetime/range?

> within :: DateTime -> Minutes -> Period -> Bool
> within s d p = s <= ps && pe <= e
>   where
>     ps = startTime p
>     pe = addMinutes (duration p) ps
>     e = d `addMinutes` s  

Does a period lie within a window?

> periodInWindow :: Period -> Window -> Bool
> periodInWindow p w = within ws wd p
>   where
>     ws = wStart w
>     wd = wDuration w

Self-test: only works for real-data (see wPeriod)

> validWindow :: Window -> Bool
> validWindow w = (isJust . wPeriod $ w) && (periodInWindow (fromJust . wPeriod $ w) w)

Get all the session periods that are within the given window

> wPeriods :: Session -> Window -> [Period]
> wPeriods s w = filter (flip periodInWindow w) $ periods s

> printList :: Show a => [a] -> IO ()
> printList = putStrLn . showList'

> showList' :: Show a => [a] -> String
> showList' = unlines . map show

TBF: this is called *trimester2startDT* because it only handles dates
before 2011, before we switched to *semesters*, so don't change the
name till we can get this right.

Here we make any year > 2010 return Nothing as a reminder that this is
NOT semester2startDT - that still needs to be coded.

> trimester2startDT :: SemesterName -> Maybe DateTime
> trimester2startDT sn | sn == "" = Nothing 
>                      | 2000 + (read $ take 2 sn) > 2010 = Nothing 
>                      | otherwise = Just $ fromGregorian year month 1 0 0 0
>   where
>     year' = 2000 + (read $ take 2 sn)
>     sem = [last sn] -- [Char]
>     --         jan feb mar apr may jun jul aug sep oct nov dec
>     tms = ["?","?","A","A","A","A","B","B","B","B","C","C","C"]
>     month = fromJust $ elemIndex sem tms
>     year = if month == 1
>            then year' - 1
>            else year'

Warning this code produces a 3.1YK problem!

> dt2semester :: DateTime -> SemesterName
> dt2semester dt
>     | dt >= sem_bndry = printf "%02d%s" (year - 2000) (sms !! month)
>     | dt >= tr1_bndry = "11B"
>     | dt >= tr2_bndry = "11A"
>     | otherwise       = printf "%02d%s" (year - 2000) (tms !! month)
>   where
>     --         jan feb mar apr may jun jul aug sep oct nov dec
>     tms = ["?","C","A","A","A","A","B","B","B","B","C","C","C"]
>     sms = ["?","B","A","A","A","A","A","A","B","B","B","B","B"]
>     sem_bndry = fromGregorian 2011  8  1  0  0  0
>     tr1_bndry = fromGregorian 2011  7  1  0  0  0
>     tr2_bndry = fromGregorian 2011  6  1  0  0  0
>     (year', month, _, _, _, _) = toGregorian dt
>     year = if month == 1
>            then year' - 1
>            else year'

> readMinutes :: String -> Minutes
> readMinutes = read

> zipWith9                :: (a->b->c->d->e->f->g->h->i->j) -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i]->[j]
> zipWith9 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is)
>                    =  z a b c d e f g h i : zipWith9 z as bs cs ds es fs gs hs is
> zipWith9 _ _ _ _ _ _ _ _ _ _ = []

> instance Convertible Float SqlValue where
>     safeConvert x = return $ SqlDouble ((realToFrac x) :: Double)

> instance Convertible SqlValue Float where
>     safeConvert x = do
>         val :: Double <- safeConvert x
>         return $ realToFrac val

> concatMapM   :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
> concatMapM f = fmap concat . mapM f

QuickCheck Properties:

> genDate :: Gen DateTime
> genDate = do
>     mon <- choose (1, 12)
>     day <- choose (1, 30) 
>     hr  <- choose (0, 23)
>     return $ fromGregorian 2006 mon day hr 0 0

> prop_utc2lstHours = forAll genDate $
>     \a -> let lst = utc2lstHours a in 0.0 <= lst && lst <= 24.0
