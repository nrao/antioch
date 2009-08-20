> module Antioch.Utilities where

> import Antioch.DateTime
> import Antioch.Types
> import qualified Antioch.SLALib as SLA
> import Test.QuickCheck hiding (frequency)

> gbtLat, gbtLong, gbtAlt :: Double
> -- TBF Why isn't it?  Need a reference here!
> --gbtLat should be:
> --        0.67078447611528835
> gbtLat  = 0.67078465065073467 -- radians
> -- gbtLong should be:
> --        -79.8398333 ...
> gbtLong = -79.839839722222223 -- degrees
> gbtAlt  = 855.0               -- meters

> deg2rad :: Float -> Float
> deg2rad deg = deg * pi / 180.0

> deg2rad' :: Double -> Double
> deg2rad' deg = deg * pi / 180.0

> rad2deg :: Float -> Float
> rad2deg rad = rad * 180.0 / pi

> rad2deg' :: Double -> Double
> rad2deg' rad = rad * 180.0 / pi

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
>     gbls = deg2hrs $ gmst + gbtLong
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

> between :: Ord a => a -> a -> a -> Bool
> between v min max = min <= v && v <= max

> printList :: Show a => [a] -> IO ()
> printList = putStrLn . showList'

> showList' :: Show a => [a] -> String
> showList' = unlines . map show

> dt2semester :: DateTime -> String
> dt2semester dt = yearStr ++ (drop 1 sem)
>   where
>     (year, month, _) = toGregorian' dt
>     sem   | month <   2 = "0C"
>           | month <   6 = "1A"
>           | month <  10 = "1B"
>           | month <= 12 = "1C"
>     year' = if (take 1 sem) == "0" then year - 1 else year
>     yearStr = drop 2 $ show year'

> readMinutes :: String -> Minutes
> readMinutes = read

QuickCheck Properties:

> genDate :: Gen DateTime
> genDate = do
>     mon <- choose (1, 12)
>     day <- choose (1, 30) 
>     hr  <- choose (0, 23)
>     return $ fromGregorian 2006 mon day hr 0 0

> prop_utc2lstHours = forAll genDate $
>     \a -> let lst = utc2lstHours a in 0.0 <= lst && lst <= 24.0
