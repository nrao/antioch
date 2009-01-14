> module Antioch.Utilities where

> import Antioch.DateTime
> import Antioch.Types
> import qualified Antioch.SLALib as SLA

> gbtLat, gbtLong, gbtAlt :: Float
> gbtLat  = 0.67078465065073467
> gbtLong = -79.839839722222223
> gbtAlt  = 855.0

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
>     gmst = rad2deg . SLA.gmst . toMJD' . fromSeconds $ dt
>     gbls = deg2hrs $ gmst + gbtLong
>     in if gbls < 0.0 then gbls + 24.0 else gbls
