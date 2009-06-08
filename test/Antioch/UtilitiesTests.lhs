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
>           , test_LST1
>           , test_LST2
>           , test_LST3
>           , test_LST3'
>                  ]

> test_dt2semester = TestCase $ do
>   assertEqual "test_dt2semester_1" "06B"  (dt2semester dt06B)
>   assertEqual "test_dt2semester_1" "09A"  (dt2semester dt09A)
>   assertEqual "test_dt2semester_1" "05C"  (dt2semester dt05C)
>   assertEqual "test_dt2semester_1" "06C"  (dt2semester dt06C)
>     where
>   dt06B = fromGregorian 2006 6  10 0 0 0
>   dt06C = fromGregorian 2006 10 10 0 0 0
>   dt05C = fromGregorian 2006 1  10 0 0 0
>   dt09A = fromGregorian 2009 2  10 0 0 0

BETA: compare against TimeAgent.Absolute2RelativeLST
from antioch.util import TimeAgent
from datetime import datetime
dt = datetime(2006, 10, 15, 11)
TimeAgent.hr2rad(TimeAgent.Absolute2RelativeLST(dt))

> test_utc2lstHours = TestCase $ do
>   assertAlmostEqual "test_utc2lstHours_1" 11 1.90240073092 lst1
>   assertEqual "test_utc2lstHours_2" expLsts lsts
>     where
>   dt = fromGregorian 2006 10 15 11 0 0
>   lst1 = hrs2rad' $ utc2lstHours' dt
>   start = fromGregorian 2006 10 16 0 0 0
>   dts = [(i*60) `addMinutes'` start | i <- [0..23]]
>   lsts = map (hrs2rad' . utc2lstHours') dts
>   expLsts = [5.315110951199158, 5.577627121975965, 5.840143292798607,
>              6.102659463575412, 8.19903271726322e-2, 0.34450649799528205,
>              0.6070226687720879, 0.8695388395488933, 1.1320550103715363,
>              1.3945711811483423, 1.6570873519251479, 1.9196035227477983,
>              2.1821196935246108, 2.444635864301424, 2.7071520351240674,
>              2.9696682059008657, 3.2321843766776714, 3.494700547500314,
>              3.7572167182771192, 4.019732889053926, 4.282249059876575,
>              4.544765230653381, 4.8072814014301875, 5.06979757225283]

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

