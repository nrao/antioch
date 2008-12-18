> module Antioch.Weather where

> import Database.HDBC
> import Database.HDBC.ODBC
> import Antioch.DateTime
> import System.IO.Unsafe (unsafePerformIO)
> import Data.List (elemIndex)
> import Data.Maybe (fromJust, isNothing)
> import Data.Time.Clock (getCurrentTime)

> data Weather = Weather {
>     wind    :: DateTime -> (Maybe DateTime) -> (Maybe Float) -- m/s
>   , tatm    :: DateTime -> (Maybe DateTime) -> (Maybe Float) -- Kelvin
>   , opacity :: DateTime -> (Maybe DateTime) -> Float -> (Maybe Float)
>   , tsys    :: DateTime -> (Maybe DateTime) -> Float -> (Maybe Float)
>   }

> getWeather :: IO Weather
> getWeather = do
>     return Weather {
>         wind    = getWind
>       , tatm    = getTAtm
>       , opacity = getOpacity
>       , tsys    = getTSys
>       }

> instance SqlType Float where
>     toSql x   = SqlDouble ((realToFrac x) :: Double)
>     fromSql x = realToFrac ((fromSql x) :: Double) :: Float

Both wind speed and atmospheric temperature are values forecast independently
of frequency.

> getWind            :: DateTime -> (Maybe DateTime) -> (Maybe Float)
> getWind target now = getWind' target (determineFType target now)

> getWind'          :: DateTime -> Int -> (Maybe Float)
> getWind' dt ftype = getFloat query [toSql . toSqlString $ dt, toSql ftype]
>   where query = "SELECT wind_speed FROM forecasts\n\
>                  \WHERE date = ? AND forecast_type_id = ?"

> getTAtm            :: DateTime -> (Maybe DateTime) -> (Maybe Float)
> getTAtm target now = getTAtm' target (determineFType target now)

> getTAtm'          :: DateTime -> Int -> (Maybe Float)
> getTAtm' dt ftype = getFloat query [toSql . toSqlString $ dt, toSql ftype]
>   where query = "SELECT tatm FROM forecasts\n\
>                  \WHERE date = ? AND forecast_type_id = ?"

However, opacity and system temperature (tsys) are values forecast dependent
on frequency.

> getOpacity            :: DateTime -> (Maybe DateTime) -> Float -> (Maybe Float)
> getOpacity target now frequency = 
>     getOpacity' target (determineFType target now) frequency

> getOpacity'                    :: DateTime -> Int -> Float -> (Maybe Float)
> getOpacity' dt ftype frequency = 
>     getFloat query [toSql . toSqlString $ dt, toSql frequency, toSql ftype]
>   where query = "SELECT opacity\n\
>                  \FROM forecasts, forecast_by_frequency\n\
>                  \WHERE date = ? AND\n\
>                  \frequency = ? AND\n\
>                  \forecast_type_id = ? AND\n\
>                  \forecasts.id = forecast_by_frequency.forecast_id"

> getTSys target now frequency = 
>     getTSys' target (determineFType target now) frequency

> getTSys'                    :: DateTime -> Int -> Float -> (Maybe Float)
> getTSys' dt ftype frequency = 
>     getFloat query [toSql . toSqlString $ dt, toSql frequency, toSql ftype]
>   where query = "SELECT tsys\n\
>                  \FROM forecasts, forecast_by_frequency\n\
>                  \WHERE date = ? AND frequency = ? AND\n\
>                  \forecast_type_id = ? AND\n\
>                  \forecasts.id = forecast_by_frequency.forecast_id"

Creates a connection to the weather forecast database.

> connect :: IO Connection
> connect = handleSqlError $ do
>     conn <- connectODBC "dsn=DSS;password=asdf5!"
>     return conn

Helper function to determine the desired forecast type given two DateTimes.

> determineFType :: DateTime -> (Maybe DateTime) -> Int
> determineFType target now = 
>     determineFType' target (getCurrentTime' now)
>   where getCurrentTime' dt = if isNothing dt
>                              then unsafePerformIO getCurrentTime
>                              else fromJust dt

> determineFType' :: DateTime -> DateTime -> Int
> determineFType' target now = 
>     case dropWhile (< difference) $ forecast_types of
>         []     -> length forecast_types
>         (x:xs) -> fromJust (elemIndex x forecast_types) + 1
>   where difference = (toSeconds target - toSeconds now) `div` 3600
>         forecast_types = [12, 24, 36, 48, 60]

Helper function to get singular Float values out of the database.

> getFloat          :: String -> [SqlValue] -> (Maybe Float)
> getFloat query xs = unsafePerformIO . handleSqlError $ do
>     conn   <- connect
>     result <- quickQuery' conn query xs
>     case result of
>         [[SqlNull]] -> return Nothing
>         [[x]] -> return $ Just (fromSql x)
>         [[]]  -> return Nothing
>         []    -> return Nothing
>         x     -> fail "There is more than one forecast with that time stamp."

Just some test functions to make sure things are working.

> target      = fromGregorian 2004 05 03 12 00 00
> now         = Just (fromGregorian 2004 05 03 12 00 00)
> testWind    = getWind target now
> testTAtm    = getTAtm target now
> testOpacity = getOpacity target now 2.0
> testTSys    = getTSys target now 2.0
