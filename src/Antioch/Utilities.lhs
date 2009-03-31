> module Antioch.Utilities where

> import Antioch.DateTime
> import Antioch.Types
> import qualified Antioch.SLALib as SLA
> import Test.QuickCheck hiding (frequency)

> gbtLat, gbtLong, gbtAlt :: Float
> --gbtLat should be:
> --        0.67078447611528835
> gbtLat  = 0.67078465065073467 -- radians
> -- gbtLong should be:
> --        -79.8398333 ...
> gbtLong = -79.839839722222223 -- degrees
> gbtAlt  = 855.0               -- meters

> deg2rad :: Float -> Float
> deg2rad deg = deg * pi / 180.0

> rad2deg :: Float -> Float
> rad2deg rad = rad * 180.0 / pi

> rad2hr x = 12 * x / pi

> deg2hrs :: Float -> Float
> deg2hrs d = d/15.0

> hrs2deg = (* 15.0)

> hrs2rad = deg2rad . hrs2deg

> utc2lstHours    :: DateTime -> Float
> utc2lstHours dt = let
>     gmst = rad2deg . SLA.gmst . secondsToMJD $ dt
>     gbls = deg2hrs $ gmst + gbtLong
>     in if gbls < 0.0 then gbls + 24.0 else gbls

> between :: Ord a => a -> a -> a -> Bool
> between v min max = min <= v && v <= max

> printList :: Show a => [a] -> IO ()
> printList = putStrLn . showList'

> showList' :: Show a => [a] -> String
> showList' = unlines . map show

QuickCheck Properties:

> genDate :: Gen DateTime
> genDate = do
>     mon <- choose (1, 12)
>     day <- choose (1, 30) 
>     hr  <- choose (0, 23)
>     return $ fromGregorian 2006 mon day hr 0 0

> prop_utc2lstHours = forAll genDate $
>     \a -> let lst = utc2lstHours a in 0.0 <= lst && lst <= 24.0
