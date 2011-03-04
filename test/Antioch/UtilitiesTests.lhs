> module Antioch.UtilitiesTests where

> import Antioch.DateTime
> import Antioch.Utilities
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score
> import Control.Monad (unless)
> --import Text.Printf (printf)

> type UtcFieldType = (Int, Int, Int, Int, Int, Int)
> type LstFieldType = (Int, Int, Int)

> tests = TestList [ 
>             test_utc2lstHours
>           , test_dt2semester
>           , test_trimester2startDT
>           , test_LST1
>           , test_LST2
>           , test_LST3
>           , test_LST3'
>           , test_inTimeRange
>           , test_overlie
>           , test_within
>           , test_periodInWindow
>           , test_freq2ForecastIndex
>           , test_freq2HistoryIndex
>           , test_approximate
>                  ]

> test_trimester2startDT = TestCase $ do
>   assertEqual "test_trimester2startDT_1" dt06A (trimester2startDT "06A")
>   assertEqual "test_trimester2startDT_2" dt06B (trimester2startDT "06B")
>   assertEqual "test_trimester2startDT_3" dt06C (trimester2startDT "06C")
>   assertEqual "test_trimester2startDT_4" dt07A (trimester2startDT "07A")
>   assertEqual "test_trimester2startDT_5" dt10A (trimester2startDT "10A")
>   assertEqual "test_trimester2startDT_5" Nothing (trimester2startDT "")
>   assertEqual "test_trimester2startDT_5" Nothing (trimester2startDT "11A")
>     where
>   dt06A = Just $ fromGregorian 2006 2  1 0 0 0 
>   dt06B = Just $ fromGregorian 2006 6  1 0 0 0 
>   dt06C = Just $ fromGregorian 2006 10 1 0 0 0 
>   dt07A = Just $ fromGregorian 2007 2  1 0 0 0 
>   dt10A = Just $ fromGregorian 2010 2  1 0 0 0 

> test_dt2semester = TestCase $ do
>   assertEqual "test_dt2semester_10a1" "10A" (dt2semester dt10a1)
>   assertEqual "test_dt2semester_10b1" "10B" (dt2semester dt10b1)
>   assertEqual "test_dt2semester_10b2" "10B" (dt2semester dt10b2)
>   assertEqual "test_dt2semester_10c1" "10C" (dt2semester dt10c1)
>   assertEqual "test_dt2semester_10c2" "10C" (dt2semester dt10c2)
>   assertEqual "test_dt2semester_10c3" "10C" (dt2semester dt10c3)
>   assertEqual "test_dt2semester_11a1" "11A" (dt2semester dt11a1)
>   assertEqual "test_dt2semester_11a2" "11A" (dt2semester dt11a2)
>   assertEqual "test_dt2semester_11b1" "11B" (dt2semester dt11b1)
>   assertEqual "test_dt2semester_11b2" "11B" (dt2semester dt11b2)
>   assertEqual "test_dt2semester_11b3" "11B" (dt2semester dt11b3)
>   assertEqual "test_dt2semester_12a1" "12A" (dt2semester dt12a1)
>   assertEqual "test_dt2semester_12a2" "12A" (dt2semester dt12a2)
>   assertEqual "test_dt2semester_12a3" "12A" (dt2semester dt12a3)
>   assertEqual "test_dt2semester_12b1" "12B" (dt2semester dt12b1)
>   assertEqual "test_dt2semester_12b2" "12B" (dt2semester dt12b2)
>   assertEqual "test_dt2semester_12b3" "12B" (dt2semester dt12b3)
>   assertEqual "test_dt2semester_13a1" "13A" (dt2semester dt13a1)
>     where
>   dt10a1 = fromGregorian 2010  5 31 23 59 59
>   dt10b1 = fromGregorian 2010  6  1  0  0  0
>   dt10b2 = fromGregorian 2010  9 30 23 59 59
>   dt10c1 = fromGregorian 2010 10  1  0  0  0
>   dt10c2 = fromGregorian 2010 12 15 12  0  0
>   dt10c3 = fromGregorian 2011  1 31 23 59 59
>   dt11a1 = fromGregorian 2011  2  1  0  0  0
>   dt11a2 = fromGregorian 2011  6 30 23 59 59
>   dt11b1 = fromGregorian 2011  7  1  0  0  0
>   dt11b2 = fromGregorian 2011 12 15 12  0  0
>   dt11b3 = fromGregorian 2012  1 31 23 59 59
>   dt12a1 = fromGregorian 2012  2  1  0  0  0
>   dt12a2 = fromGregorian 2012  3  1  0  0  0
>   dt12a3 = fromGregorian 2012  7 31 23 59 59
>   dt12b1 = fromGregorian 2012  8  1  0  0  0
>   dt12b2 = fromGregorian 2012 12 15 12  0  0
>   dt12b3 = fromGregorian 2013  1 31 23 59 59
>   dt13a1 = fromGregorian 2013  2  1  0  0  0

BETA: compare against TimeAgent.Absolute2RelativeLST
from antioch.util import TimeAgent
from datetime import datetime
dt = datetime(2006, 10, 15, 11)
TimeAgent.hr2rad(TimeAgent.Absolute2RelativeLST(dt))

> test_utc2lstHours = TestCase $ do
>   assertEqual "test_utc2lstHours_1" 1.9024007260691838 lst1
>   assertEqual "test_utc2lstHours_2" expLsts lsts
>     where
>   dt = fromGregorian 2006 10 15 11 0 0
>   lst1 = hrs2rad' $ utc2lstHours' dt
>   start = fromGregorian 2006 10 16 0 0 0
>   dts = [(i*60) `addMinutes` start | i <- [0..23]]
>   lsts = map (hrs2rad' . utc2lstHours') dts
>   expLsts = [5.315110946351022,5.577627117127827,5.840143287950469,6.102659458727275,8.199032232449549e-2,0.3445064931471453,0.6070226639239512,0.8695388347007567,1.1320550055233998,1.3945711763002058,1.657087347077011,1.9196035178996615,2.182119688676474,2.4446358594532875,2.7071520302759304,2.9696682010527287,3.2321843718295353,3.4947005426521778,3.7572167134289827,4.01973288420579,4.282249055028439,4.544765225805245,4.8072813965820504,5.069797567404693]

> test_LST1 = TestCase $ do
>   mapM_ (runUtc2LstTest "test_LST1") times
>   mapM_ (runLst2UtcTest "test_LST1" now) times
>     where
>   now = (2006, 5, 5, 14, 0, 0)
>   times = [
>       --   LST              UTC
>       ((23, 30, 57),  (2006, 5, 6, 13, 53, 10)),
>       ((23, 35, 58),  (2006, 5, 5, 14, 02, 7)),
>       ((23, 40, 59),  (2006, 5, 5, 14, 07, 7)),
>       ((23, 46, 00),  (2006, 5, 5, 14, 12, 7)),
>       ((23, 51, 01),  (2006, 5, 5, 14, 17, 7)),
>       ((23, 56, 02),  (2006, 5, 5, 14, 22, 7)),
>       ((00, 01, 02),  (2006, 5, 5, 14, 27, 7)),
>       ((00, 06, 03),  (2006, 5, 5, 14, 32, 7)),
>       ((00, 11, 04),  (2006, 5, 5, 14, 37, 7)),
>       ((00, 16, 05),  (2006, 5, 5, 14, 42, 7))
>       ]

> test_LST2 = TestCase $ do
>   mapM_ (runUtc2LstTest "test_LST2") times
>   mapM_ (runLst2UtcTest "test_LST2" now) times
>     where
>   now = (2006, 5, 5, 14, 0, 0)
>   times = [
>       --   LST              UTC
>       ((23, 59, 52), (2006, 5, 5, 14, 25, 57)),
>       ((23, 59, 53), (2006, 5, 5, 14, 25, 58)),
>       ((23, 59, 54), (2006, 5, 5, 14, 25, 59)),
>       ((23, 59, 55), (2006, 5, 5, 14, 26, 0)),
>       ((23, 59, 56), (2006, 5, 5, 14, 26, 01)),
>       ((23, 59, 57), (2006, 5, 5, 14, 26, 02)),
>       ((23, 59, 58), (2006, 5, 5, 14, 26, 03)),
>       ((23, 59, 59), (2006, 5, 5, 14, 26, 04)),
>       ((00, 00, 00), (2006, 5, 5, 14, 26, 05)),
>       ((00, 00, 01), (2006, 5, 5, 14, 26, 06)),
>       ((00, 00, 02), (2006, 5, 5, 14, 26, 07)),
>       ((00, 00, 04), (2006, 5, 5, 14, 26, 09)),
>       ((00, 00, 05), (2006, 5, 5, 14, 26, 10)),
>       ((00, 00, 06), (2006, 5, 5, 14, 26, 11)),
>       ((00, 00, 07), (2006, 5, 5, 14, 26, 12)),
>       ((00, 00, 08), (2006, 5, 5, 14, 26, 13))
>       ]

> test_LST3' = TestCase $ do
>   mapM_ (runUtc2LstTest "test_LST3'") times
>   mapM_ (runLst2UtcTest "test_LST3'" now) times
>     where
>   now = (2006, 5, 5, 22, 0, 0)
>   times = [
>       --   LST              UTC
>       ((09, 35, 30), (2006, 5, 6, 00, 00, 01)),
>       ((09, 35, 31), (2006, 5, 6, 00, 00, 02)),
>       ((09, 35, 32), (2006, 5, 6, 00, 00, 03)),
>       ((09, 35, 33), (2006, 5, 6, 00, 00, 03)),
>       ((09, 35, 34), (2006, 5, 6, 00, 00, 04)),
>       ((09, 35, 35), (2006, 5, 6, 00, 00, 05)),
>       ((09, 35, 36), (2006, 5, 6, 00, 00, 06)),
>       ((09, 35, 37), (2006, 5, 6, 00, 00, 07))
>       ]

> test_inTimeRange = TestCase $ do
>   assertEqual "test_inTimeRange_1"  False (inTimeRange dt1 start dur)
>   assertEqual "test_inTimeRange_2"  True  (inTimeRange dt2 start dur)
>   assertEqual "test_inTimeRange_3"  True  (inTimeRange dt3 start dur)
>   assertEqual "test_inTimeRange_4"  False (inTimeRange dt4 start dur)
>   assertEqual "test_inTimeRange_5"  False (inTimeRange dt5 start dur)
>     where
>   start = fromGregorian 2006 6 1 12 0 0
>   dur = 4*60
>   dt1 = (-15) `addMinutes` start
>   dt2 = start
>   dt3 = (2*60) `addMinutes` start
>   dt4 = dur `addMinutes` start
>   dt5 = (dur+15) `addMinutes` start

> test_overlie = TestCase $ do
>   assertEqual "test_overlie_1"  True  (overlie dt1 dur1 p1)
>   assertEqual "test_overlie_2"  True  (overlie dt1 dur2 p1)
>   assertEqual "test_overlie_3"  True  (overlie dt1 dur1 p2)
>   assertEqual "test_overlie_4"  False (overlie dt2 dur1 p1)
>   assertEqual "test_overlie_5"  False (overlie dt3 dur1 p1)
>   assertEqual "test_overlie_6"  True  (overlie dt3 dur2 p1)
>   assertEqual "test_overlie_7"  True  (overlie dt4 dur1 p2)
>   assertEqual "test_overlie_8"  True  (overlie dt4 dur2 p2)
>   assertEqual "test_overlie_9"  False (overlie dt2 dur2 p1)
>   assertEqual "test_overlie_10" False (overlie dt5 dur1 p1)
>   assertEqual "test_overlie_11" True  (overlie dt5 dur1 p2)
>     where
>   dt1 = fromGregorian 2006 6 1 12 0 0
>   dur1 = 4*60
>   p1 = defaultPeriod {startTime = dt1, duration = dur1}
>   dur2 = 6*60
>   p2 = defaultPeriod {startTime = dt1, duration = dur2}
>   dt2 = fromGregorian 2006 6 1 20 0 0
>   dt3 = fromGregorian 2006 6 1 8 0 0
>   dt4 = fromGregorian 2006 6 1 13 0 0
>   dt5 = fromGregorian 2006 6 1 16 0 0

> test_within = TestCase $ do
>   assertEqual "test_within_1"  True  (within dt1 dur1 p1)
>   assertEqual "test_within_2"  True  (within dt1 dur2 p1)
>   assertEqual "test_within_3"  False (within dt1 dur1 p2)
>   assertEqual "test_within_4"  False (within dt2 dur1 p1)
>   assertEqual "test_within_5"  False (within dt3 dur1 p1)
>   assertEqual "test_within_6"  False (within dt3 dur2 p1)
>   assertEqual "test_within_7"  False (within dt4 dur1 p2)
>   assertEqual "test_within_8"  False (within dt4 dur2 p2)
>   assertEqual "test_within_9"  False (within dt2 dur2 p1)
>   assertEqual "test_within_10" False (within dt5 dur1 p1)
>   assertEqual "test_within_11" False (within dt5 dur1 p2)
>   assertEqual "test_within_12" True  (within dt3 dur3 p1)
>   assertEqual "test_within_13" True  (within dt3 dur3 p2)
>     where
>   dt1 = fromGregorian 2006 6 1 12 0 0
>   dur1 = 4*60
>   p1 = defaultPeriod {startTime = dt1, duration = dur1}
>   dur2 = 6*60
>   dur3 = 10*60
>   p2 = defaultPeriod {startTime = dt1, duration = dur2}
>   dt2 = fromGregorian 2006 6 1 20 0 0
>   dt3 = fromGregorian 2006 6 1 8 0 0
>   dt4 = fromGregorian 2006 6 1 13 0 0
>   dt5 = fromGregorian 2006 6 1 16 0 0

> test_periodInWindow = TestCase $ do
>   assertEqual "test_periodInWindow_1"  True  (periodInWindow p1 w)
>   assertEqual "test_periodInWindow_2"  True  (periodInWindow p2 w)
>   assertEqual "test_periodInWindow_3"  False (periodInWindow p3 w)
>   assertEqual "test_periodInWindow_4"  False (periodInWindow p4 w)
>   assertEqual "test_periodInWindow_5"  True  (periodInWindow p5 w)
>     where
>   wr = [(defaultStartTime, addMinutes (6*60) defaultStartTime)]
>   w = defaultWindow { wRanges = wr }
>   p1 = defaultPeriod {duration = 6*60-1}
>   p2 = defaultPeriod {duration = 6*60}
>   p3 = defaultPeriod {duration = 6*60+1}
>   p4 = defaultPeriod {startTime = (-1) `addMinutes` defaultStartTime, duration = 6*60+1}
>   p5 = defaultPeriod {startTime = (1) `addMinutes` defaultStartTime, duration = 6*60-1}

> test_LST3 = TestCase $ do
>   mapM_ (runUtc2LstTest "test_LST3") times
>   mapM_ (runLst2UtcTest "test_LST3" now) times
>     where
>   now = (2006, 5, 5, 22, 0, 0)
>   times = [
>       --   LST              UTC
>       ((09, 35, 19), (2006, 5, 5, 23, 59, 50)),
>       ((09, 35, 21), (2006, 5, 5, 23, 59, 52)),
>       ((09, 35, 22), (2006, 5, 5, 23, 59, 53)),
>       ((09, 35, 23), (2006, 5, 5, 23, 59, 54)),
>       ((09, 35, 24), (2006, 5, 5, 23, 59, 55)),
>       ((09, 35, 25), (2006, 5, 5, 23, 59, 56)),
>       ((09, 35, 26), (2006, 5, 5, 23, 59, 57)),
>       ((09, 35, 27), (2006, 5, 5, 23, 59, 58)),
>       ((09, 35, 28), (2006, 5, 5, 23, 59, 59)),
>       ((09, 35, 29), (2006, 5, 5, 23, 59, 59))
>       ]

> test_freq2ForecastIndex = TestCase $ do
>   -- freq2ForecastIndex'
>   assertEqual "test_freq2ForecastIndex 1" 120  (freq2ForecastIndex' 140.0)
>   assertEqual "test_freq2ForecastIndex 2" 2    (freq2ForecastIndex' 1.0)
>   assertEqual "test_freq2ForecastIndex 3" 2    (freq2ForecastIndex' 0.4)
>   assertEqual "test_freq2ForecastIndex 4" 5    (freq2ForecastIndex' 5.2)
>   assertEqual "test_freq2ForecastIndex 5" 6    (freq2ForecastIndex' 5.5)
>   -- freq2ForecastIndex
>   assertEqual "test_freq2ForecastIndex 6" 120  (freq2ForecastIndex 140.0)
>   assertEqual "test_freq2ForecastIndex 7" 2    (freq2ForecastIndex 1.0)
>   assertEqual "test_freq2ForecastIndex 8" 2    (freq2ForecastIndex 0.4)
>   assertEqual "test_freq2ForecastIndex 9" 5    (freq2ForecastIndex 5.2)
>   assertEqual "test_freq2ForecastIndex 10" 6   (freq2ForecastIndex 5.5)
>   assertEqual "test_freq2ForecastIndex 11" 52  (freq2ForecastIndex 52.3)
>   assertEqual "test_freq2ForecastIndex 12" 58  (freq2ForecastIndex 57.1)
>   assertEqual "test_freq2ForecastIndex 13" 120 (freq2ForecastIndex 140.0)

> test_freq2HistoryIndex = TestCase $ do
>   assertEqual "test_freq2HistoryIndex_1" 40000    (freq2HistoryIndex Rcvr26_40 52.0)
>   assertEqual "test_freqHistory2Index_2" 100      (freq2HistoryIndex Rcvr_RRI 0.0)
>   assertEqual "test_freqHistory2Index_3" 700      (freq2HistoryIndex Rcvr_600 0.65)
>   assertEqual "test_freqHistory2Index_4" 900      (freq2HistoryIndex Rcvr_1070 0.85)
>   assertEqual "test_freq2HistoryIndex_5" 1000     (freq2HistoryIndex Rcvr_1070 1.24)
>   assertEqual "test_freq2HistoryIndex_6" 100      (freq2HistoryIndex Rcvr_RRI 0.02)
>   assertEqual "test_freq2HistoryIndex_7" 5000     (freq2HistoryIndex Rcvr4_6 5.23)
>   assertEqual "test_freq2HistoryIndex_8" 6000     (freq2HistoryIndex Rcvr4_6 5.5)
>   assertEqual "test_freq2HistoryIndex_9" 50000    (freq2HistoryIndex Rcvr40_52 52.3)
>   assertEqual "test_freq2HistoryIndex_10" 50000   (freq2HistoryIndex Rcvr40_52 57.1)
>   assertEqual "test_freq2HistoryIndex_11" 100000  (freq2HistoryIndex Rcvr_PAR 101.5)

> test_approximate = TestCase $ do
>     let ys = [4, 8, 11, 15, 16]
>     assertEqual "test_approximate 1"   4  (approximate closest  2 ys)
>     assertEqual "test_approximate 2"   4  (approximate closest  4 ys)
>     assertEqual "test_approximate 3"   4  (approximate closest  5 ys)
>     assertEqual "test_approximate 4"   8  (approximate closest  6 ys)
>     assertEqual "test_approximate 5"   8  (approximate closest  7 ys)
>     assertEqual "test_approximate 6"   8  (approximate closest  8 ys)
>     assertEqual "test_approximate 7"   8  (approximate closest  9 ys)
>     assertEqual "test_approximate 8"  11  (approximate closest 10 ys)
>     assertEqual "test_approximate 9"  11  (approximate closest 11 ys)
>     assertEqual "test_approximate 10" 11  (approximate closest 12 ys)
>     assertEqual "test_approximate 11" 16  (approximate closest 16 ys)
>     assertEqual "test_approximate 12" 16  (approximate closest 17 ys)
>     assertEqual "test_approximate 13"  4  (approximate lowest  2 ys)
>     assertEqual "test_approximate 14"  4  (approximate lowest  4 ys)
>     assertEqual "test_approximate 15"  4  (approximate lowest  5 ys)
>     assertEqual "test_approximate 16"  4  (approximate lowest  6 ys)
>     assertEqual "test_approximate 17"  4  (approximate lowest  7 ys)
>     assertEqual "test_approximate 18"  8  (approximate lowest  8 ys)
>     assertEqual "test_approximate 19"  8  (approximate lowest  9 ys)
>     assertEqual "test_approximate 20"  8  (approximate lowest 10 ys)
>     assertEqual "test_approximate 21" 11  (approximate lowest 11 ys)
>     assertEqual "test_approximate 22" 11  (approximate lowest 12 ys)
>     assertEqual "test_approximate 23" 16  (approximate lowest 16 ys)
>     assertEqual "test_approximate 24" 16  (approximate lowest 17 ys)
>     assertEqual "test_approximate 25"  4  (approximate highest  2 ys)
>     assertEqual "test_approximate 26"  4  (approximate highest  4 ys)
>     assertEqual "test_approximate 27"  8  (approximate highest  5 ys)
>     assertEqual "test_approximate 28"  8  (approximate highest  6 ys)
>     assertEqual "test_approximate 29"  8  (approximate highest  7 ys)
>     assertEqual "test_approximate 30"  8  (approximate highest  8 ys)
>     assertEqual "test_approximate 31" 11  (approximate highest  9 ys)
>     assertEqual "test_approximate 32" 11  (approximate highest 10 ys)
>     assertEqual "test_approximate 33" 11  (approximate highest 11 ys)
>     assertEqual "test_approximate 34" 15  (approximate highest 12 ys)
>     assertEqual "test_approximate 35" 16  (approximate highest 16 ys)
>     assertEqual "test_approximate 36" 16  (approximate highest 17 ys)

Utilities

> hms2hrs :: LstFieldType -> Double
> hms2hrs (h, m, s) = (fromIntegral h) + (fromIntegral m)/60.0 + (fromIntegral s)/3600.0

> runUtc2LstTest :: String -> (LstFieldType, UtcFieldType) -> IO ()
> runUtc2LstTest preface time =
>   assertAlmostEqual (preface ++ " (UTC to LST)") 3 ilst glst
>     where
>   ilst = hms2hrs (fst time)
>   iutc = let (y, mo, d, h, mi, s) = (snd time) in fromGregorian y mo d h mi s
>   glst = utc2lstHours' iutc

> runLst2UtcTest :: String -> UtcFieldType -> (LstFieldType, UtcFieldType) -> IO ()
> runLst2UtcTest preface now time = do
>   --printf "\niutc %d-%d-%d %02d:%02d:%02d\n" iy imo id ih imi is
>   --printf "gutc %d-%d-%d %02d:%02d:%02d\n" gy gmo gd gh gmi gs
>   --print inow
>   --print ilst
>   assertSecondsEqual (preface ++ " (LST to UTC)") 2 iutc gutc
>     where
>   inow = let (y, mo, d, h, mi, s) = now in fromGregorian y mo d h mi s
>   ilst = hms2hrs (fst time)
>   iutc = let (y, mo, d, h, mi, s) = (snd time) in fromGregorian y mo d h mi s
>   gutc = lstHours2utc' inow ilst
>   (iy, imo, id, ih, imi, is) = toGregorian iutc
>   (gy, gmo, gd, gh, gmi, gs) = toGregorian gutc

> assertAlmostEqual :: String -> Int -> Double -> Double -> Assertion
> assertAlmostEqual preface places expected actual =
>   unless (abs (actual - expected) < epsilon) (assertFailure msg)
>     where
>   msg = (if null preface then "" else preface ++ "\n") ++
>            "expected: " ++ show expected ++ "\n but got: " ++ show actual
>   epsilon = 1.0 / 10.0 ** fromIntegral places

> assertSecondsEqual :: String -> Int -> DateTime -> DateTime -> Assertion
> assertSecondsEqual preface seconds expected actual =
>   unless (abs (actual `diffSeconds` expected) < seconds) (assertFailure msg)
>     where
>   msg = (if null preface then "" else preface ++ "\n") ++
>            "expected: " ++ show expected ++ "\n but got: " ++ show actual

