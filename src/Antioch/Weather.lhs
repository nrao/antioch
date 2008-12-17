> module Antioch.Weather where

> import Database.HDBC
> import Database.HDBC.ODBC
> import Antioch.Types

> instance SqlType Float where
>     toSql x   = SqlDouble ((realToFrac x) :: Double)
>     fromSql x = realToFrac ((fromSql x) :: Double) :: Float

> data Weather = Weather {
>     opacity :: DateTime -> Float -> Float
>   , tsys    :: DateTime -> Float -> Float
>   , wind    :: DateTime -> Float  -- m/s
>   , tatm    :: DateTime -> Float  -- Kelvin
>   }

> getWeather :: IO Weather
> getWeather = do
>     return Weather {
>         opacity = \_ _ -> 0.2
>       , tsys    = \_ _ -> 255.0
>       , wind    = const   2.0
>       , tatm    = const   255.0
>       }

> connect :: IO Connection
> connect = handleSqlError $ do
>     conn <- connectODBC "dsn=DSS;password=asdf5!"
>     return conn

Both wind speed and atmospheric temperature are values forecast independently
of frequency.

> getWind    :: String -> Int -> IO (Maybe Float)
> getWind dt ftype = getFloat query [toSql dt, toSql ftype]
>     where query = "SELECT wind_speed FROM forecasts\n\
>                    \WHERE date = ? AND forecast_type_id = ?"

> getTAtm    :: String -> Int -> IO (Maybe Float)
> getTAtm dt ftype = getFloat query [toSql dt, toSql ftype]
>     where query = "SELECT tatm FROM forecasts\n\
>                    \WHERE date = ? AND forecast_type_id = ?"

However, opacity and system temperature (tsys) are values forecast dependent
on frequency.

> getOpacity :: String -> Float -> Int -> IO (Maybe Float)
> getOpacity dt frequency ftype = 
>     getFloat query [toSql dt, toSql frequency, toSql ftype]
>   where query = "SELECT opacity\n\
>                  \FROM forecasts, forecast_by_frequency\n\
>                  \WHERE date = ? AND\n\
>                  \frequency = ? AND\n\
>                  \forecast_type_id = ? AND\n\
>                  \forecasts.id = forecast_by_frequency.forecast_id"

> getTSys :: String -> Float -> Int -> IO (Maybe Float)
> getTSys dt frequency ftype = 
>     getFloat query [toSql dt, toSql frequency, toSql ftype]
>   where query = "SELECT tsys\n\
>                  \FROM forecasts, forecast_by_frequency\n\
>                  \WHERE date = ? AND frequency = ? AND\n\
>                  \forecast_type_id = ? AND\n\
>                  \forecasts.id = forecast_by_frequency.forecast_id"

Helper function to get singular Float values out of the database.

> getFloat :: String -> [SqlValue] -> IO (Maybe Float)
> getFloat query xs = handleSqlError $ do
>     conn   <- connect
>     result <- quickQuery' conn query xs
>     case result of
>         [[SqlNull]] -> return Nothing
>         [[x]] -> return $ Just (fromSql x)
>         [[]]  -> return Nothing
>         []    -> return Nothing
>         x     -> fail "There is more than one forecast with that time stamp."
