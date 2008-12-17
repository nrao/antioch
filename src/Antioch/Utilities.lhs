> module Antioch.Utilities where

> import Antioch.SLALib  (gmst)
> import Antioch.Types
> import Data.Time.Clock

> gbtLat, gbtLong, gbtAlt :: Float
> gbtLat  = 0.67078465065073467
> gbtLong = -79.839839722222223
> gbtAlt  = 855.0

> deg2rad deg = deg * pi / 180.0
> rad2deg rad = rad * 180.0 / pi
> rad2hr x = 12 * x / pi

