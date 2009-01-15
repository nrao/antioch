> module Antioch.Weather where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Control.Exception (IOException, bracketOnError, catch)
> import Data.IORef
> import Data.List (elemIndex)
> import Data.Maybe (fromJust, maybe)
> import Database.HDBC
> import Database.HDBC.ODBC
> import Prelude hiding (catch)
> import Test.QuickCheck

> instance SqlType Float where
>     toSql x   = SqlDouble ((realToFrac x) :: Double)
>     fromSql x = realToFrac ((fromSql x) :: Double) :: Float

> data Weather = Weather {
>     wind            :: DateTime -> IO (Maybe Float)  -- m/s
>   , tatm            :: DateTime -> IO (Maybe Float)  -- Kelvin
>   , opacity         :: DateTime -> Frequency -> IO (Maybe Float)
>   , tsys            :: DateTime -> Frequency -> IO (Maybe Float)
>   , totalStringency :: Frequency -> Radians -> IO (Maybe Float)
>   , minOpacity      :: Frequency -> Radians -> IO (Maybe Float)
>   , minTSysPrime    :: Frequency -> Radians -> IO (Maybe Float)
>   }

> getWeather     :: Maybe DateTime -> IO Weather
> getWeather now = bracketOnError connect disconnect $ \conn' -> do
>     now'  <- maybe getCurrentTime return now
>     conn' <- connect
>     conn  <- newIORef conn'
>     return Weather {
>         wind            = pin now' $ getWind conn
>       , tatm            = pin now' $ getTAtm conn
>       , opacity         = pin now' $ getOpacity conn
>       , tsys            = pin now' $ getTSys conn
>       , totalStringency = getTotalStringency conn
>       , minOpacity      = getMinOpacity conn
>       , minTSysPrime    = getMinTSysPrime conn
>       }

> pin              :: DateTime -> (Int -> DateTime -> a) -> DateTime -> a
> pin now f target = f (forecastType target now) target

> freq2Index :: Frequency -> Int
> freq2Index =  min 50 . max 2 . round

> elev2Index :: Radians -> Int
> elev2Index =  min 90 . max 5 . round . rad2deg

Both wind speed and atmospheric temperature are values forecast independently
of frequency.

> getWind               :: IORef Connection -> Int -> DateTime -> IO (Maybe Float)
> getWind conn ftype dt =
>     getFloat conn query [toSql' dt, toSql ftype]
>   where query = "SELECT wind_speed FROM forecasts\n\
>                  \WHERE date = ? AND forecast_type_id = ?"

> toSql' = toSql . toSqlString . roundToHour

> getTAtm               :: IORef Connection -> Int -> DateTime -> IO (Maybe Float)
> getTAtm conn ftype dt =
>     getFloat conn query [toSql' dt, toSql ftype]
>   where query = "SELECT tatm FROM forecasts\n\
>                  \WHERE date = ? AND forecast_type_id = ?"

However, opacity and system temperature (tsys) are values forecast dependent
on frequency.

> getOpacity :: IORef Connection -> Int -> DateTime -> Frequency -> IO (Maybe Float)
> getOpacity conn ftype dt frequency = 
>     getFloat conn query [toSql' dt
>                        , toSql (freq2Index frequency :: Int)
>                        , toSql ftype]
>   where query = "SELECT opacity\n\
>                  \FROM forecasts, forecast_by_frequency\n\
>                  \WHERE date = ? AND\n\
>                  \frequency = ? AND\n\
>                  \forecast_type_id = ? AND\n\
>                  \forecasts.id = forecast_by_frequency.forecast_id"

> getTSys :: IORef Connection -> Int -> DateTime -> Frequency -> IO (Maybe Float)
> getTSys conn ftype dt frequency = 
>     getFloat conn query [toSql' dt
>                        , toSql (freq2Index frequency :: Int)
>                        , toSql ftype]
>   where query = "SELECT tsys\n\
>                  \FROM forecasts, forecast_by_frequency\n\
>                  \WHERE date = ? AND frequency = ? AND\n\
>                  \forecast_type_id = ? AND\n\
>                  \forecasts.id = forecast_by_frequency.forecast_id"

> getTotalStringency :: IORef Connection -> Frequency -> Radians -> IO (Maybe Float)
> getTotalStringency conn frequency elevation = 
>     getFloat conn query [toSql (freq2Index frequency :: Int)
>                        , toSql (elev2Index elevation :: Int)]
>   where query = "SELECT total FROM stringency\n\
>                  \WHERE frequency = ? AND elevation = ?"

> getMinOpacity :: IORef Connection -> Frequency -> Radians -> IO (Maybe Float)
> getMinOpacity conn frequency elevation = 
>     getFloat conn query [toSql (freq2Index frequency :: Int)
>                        , toSql (elev2Index elevation :: Int)]
>   where query = "SELECT opacity FROM min_weather\n\
>                  \WHERE frequency = ? AND elevation = ?"

> getMinTSysPrime :: IORef Connection -> Frequency -> Radians -> IO (Maybe Float)
> getMinTSysPrime conn frequency elevation = 
>     getFloat conn query [toSql (freq2Index frequency :: Int)
>                        , toSql (elev2Index elevation :: Int)]
>   where query = "SELECT prime FROM t_sys\n\
>                  \WHERE frequency = ? AND elevation = ?"

Creates a connection to the weather forecast database.

> connect :: IO Connection
> connect = handleSqlError $ connectODBC "dsn=DSS;password=asdf5!"

Helper function to determine the desired forecast type given two DateTimes.

> forecastType :: DateTime -> DateTime -> Int
> forecastType target now = 
>     case dropWhile (< difference) $ forecast_types of
>         []     -> length forecast_types
>         (x:xs) -> fromJust (elemIndex x forecast_types) + 1
>   where difference = (target - now) `div` 3600
>         forecast_types = [12, 24, 36, 48, 60]

> prop_constrained target now = forecastType target now `elem` forecast_types
>   where forecast_types = [1..5]

Helper function to get singular Float values out of the database.

> getFloat :: IORef Connection -> String -> [SqlValue] -> IO (Maybe Float)
> getFloat conn query xs = handleSqlError $ do
>     result <- tryQuery conn query xs
>     case result of
>         [[SqlNull]] -> return Nothing
>         [[x]] -> return $ Just (fromSql x)
>         [[]]  -> return Nothing
>         []    -> return Nothing
>         x     -> fail "There is more than one forecast with that time stamp."

> tryQuery :: IORef Connection -> String -> [SqlValue] -> IO [[SqlValue]]
> tryQuery conn query xs = do
>     conn' <- readIORef conn
>     quickQuery' conn' query xs `catch` \e -> do
>         print (e :: IOException)
>         c' <- connect
>         writeIORef conn c'
>         quickQuery' c' query xs

Just some test functions to make sure things are working.

> testWeather = do
>     w <- getWeather now
>     return $ (wind w target
>             , tatm w target
>             , opacity w target frequency
>             , tsys w target frequency
>             , totalStringency w frequency elevation
>             , minOpacity w frequency elevation
>             , minTSysPrime w frequency elevation)
>   where 
>     frequency = 2.0 :: Float
>     elevation = pi / 4.0 :: Radians
>     now       = Just (fromGregorian 2004 05 03 12 00 00)
>     target    = fromGregorian 2004 05 03 12 00 00
