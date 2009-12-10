> {-# OPTIONS -XMultiParamTypeClasses -XScopedTypeVariables #-}

> module Antioch.Weather where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.Generators
> import Antioch.Settings (weatherDB)
> import Control.Exception (IOException, bracketOnError, catch)
> import Control.Monad     (liftM)
> import Data.Convertible
> import Data.IORef
> import Data.List         (elemIndex)
> import Data.Maybe        (fromJust, maybe, isJust, fromMaybe)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Prelude hiding (catch)
> import Test.QuickCheck
> import qualified Data.Map as M
> import System.IO.Unsafe (unsafePerformIO)

> instance Convertible Float SqlValue where
>     safeConvert x = return $ SqlDouble ((realToFrac x) :: Double)

> instance Convertible SqlValue Float where
>     safeConvert x = do
>         val :: Double <- safeConvert x
>         return $ realToFrac val

> data Weather = Weather {
>     wind            :: DateTime -> IO (Maybe Float)  -- m/s
>   , w2_wind         :: DateTime -> IO (Maybe Float)  -- m/s
>   , wind_mph        :: DateTime -> IO (Maybe Float)  -- mph
>   , opacity         :: DateTime -> Frequency -> IO (Maybe Float)
>   , tsys            :: DateTime -> Frequency -> IO (Maybe Float)
>   , totalStringency :: Frequency -> Radians -> IO (Maybe Float)
>   , minOpacity      :: Frequency -> Radians -> IO (Maybe Float)
>   , minTSysPrime    :: Frequency -> Radians -> IO (Maybe Float)
>   , newWeather      :: Maybe DateTime -> IO Weather
>   , forecast        :: DateTime
>   }

The "unsafePerformIO hack" is a way of emulating global variables in GHC.

> {-# NOINLINE globalConnection #-}
> globalConnection :: IORef Connection
> globalConnection = unsafePerformIO $ connect >>= newIORef

This interface method makes sure that dates don't get passed in
that the DB has no data for.  Currently all modules are using this.

> getWeather :: Maybe DateTime -> IO Weather
> getWeather dt = do
>     now <- maybe getCurrentTimeSafe return dt
>     case dt of
>       Nothing -> getWeatherSafe now
>       Just x  -> getWeatherSafe x

Used for simulations/tests to ensure that we always get data, no matter what
year's worth of weather is in the database, by modifiying the date.

> getWeatherSafe :: DateTime -> IO Weather
> getWeatherSafe = getWeather' . Just . dateSafe 

Right now, the only historical weather we have is 2006.
TBF: shouldn't we be able to put 2007, 2008 in there now? No, only 2006 in DB!
However, we are importing the latest weather forecasts into this DB,
so we've deprecated 'dateSafe'.

> dateSafe :: DateTime -> DateTime
> dateSafe dt = dt --if (year == 2006) then dt else replaceYear 2006 dt
>   where
>     (year, _, _, _, _, _) = toGregorian dt
> -- TBF: do this when you have more then one year:
> --dateSafe dt = if (any (==year) [2006, 2007, 2008]) then dt else replaceYear 2006 dt

> getCurrentTimeSafe = do
>   dt <- getCurrentTime
>   return $ dateSafe dt

> getWeather'     :: Maybe DateTime -> IO Weather
> getWeather' now = readIORef globalConnection >>= \cnn -> updateWeather cnn now

> updateWeather :: Connection -> Maybe DateTime -> IO Weather
> updateWeather conn now = do
>     now' <- maybe getCurrentTimeSafe return now
>     ft   <- getForecastTime conn now'
>     (windf, w2_windf, wind_mphf)   <- getWinds'
>     (opacityf, tsysf)   <- getOpacityAndTSys'
>     stringencyf         <- getTotalStringency'
>     minOpacityf         <- getMinOpacity'
>     minTSysf            <- getMinTSysPrime'
>     return Weather {
>         wind            = pin ft now' $ windf conn
>       , w2_wind         = pin ft now' $ w2_windf conn
>       , wind_mph        = pin ft now' $ wind_mphf conn
>       , opacity         = pin ft now' $ opacityf conn
>       , tsys            = pin ft now' $ tsysf conn
>       , totalStringency = stringencyf conn
>       , minOpacity      = minOpacityf conn
>       , minTSysPrime    = minTSysf conn
>       , newWeather      = updateWeather conn
>       , forecast        = now'
>       }

forecastType takes both 'now' and a forecastTime since we have to be 
backwards compatible w/ our unit tests: requests for 2006 weather use a
deprecated method for computing the forecast_type_id and use 12 hour forecasts,
where as in Dec. of 2009 we went to 6 hour forecasts and compute the 
forecast_type_id correctly using the forecast_time from the DB.

> pin :: DateTime -> DateTime -> (Int -> DateTime -> a) -> DateTime -> a
> pin forecastTime now f target = f (forecastType target' now forecastTime) target'
>   where
>     target' = dateSafe target

> toSql' = toSql . toSqlString

> fromSql' SqlNull = Nothing
> fromSql' x       = Just . fromSql $ x

> freq2Index :: Frequency -> Int
> freq2Index = min 50 . max 2 . round

> elev2Index :: Radians -> Int
> elev2Index =  min 90 . max 5 . round . rad2deg

Both wind speeds and atmospheric temperature are values forecast independently
of frequency.

> getWinds' = do
>     cache <- newIORef M.empty
>     return (getWind cache, getW2Wind cache, getWindMPH cache)

Generic method for pulling wind speeds from the weather DB tables.
Note column and try parameters: if you don't get any results w/ the
given query (by forecast id), try again (getting most recent forecast).

> fetchWind :: Connection -> DateTime -> Int -> String -> [SqlValue] -> String -> Bool -> IO (Maybe Float)
> fetchWind cnn dt ftype query xs column try = handleSqlError $ do
>     result <- quickQuery' cnn query xs
>     case result of
>       [[wind]] -> return $ fromSql' wind
>       _        -> if try then fetchAnyWind cnn dt ftype column else return Nothing

Note: only applicable for columns 'wind_speed' and 'wind_speed_mph'

> fetchAnyWind :: Connection -> DateTime -> Int -> String -> IO (Maybe Float)
> fetchAnyWind cnn dt ftype column = handleSqlError $ do
>   print $ "Wind " ++ column ++ " was not found for date: " ++ (show . toSqlString $ dt) ++ " and forecast type: " ++ (show ftype)
>   result <- quickQuery' cnn query xs
>   case result of 
>     [wind]:_ -> return $ fromSql' wind
>     _        -> return Nothing
>     where
>       xs = [toSql' dt]
>       query = "SELECT " ++ column ++ " \n\
>              \FROM forecasts \n\
>              \INNER JOIN weather_dates \n\
>              \ON weather_date_id = weather_dates.id \n\
>              \WHERE weather_dates.date = ? \n\
>              \ORDER BY forecast_type_id"

> getWind :: IORef (M.Map (Int, Int) (Maybe Float)) -> Connection -> Int -> DateTime -> IO (Maybe Float)
> getWind cache cnn ftype dt = withCache key cache $
>     fetchWind cnn dt' ftype query xs "wind_speed" True
>   where
>     dt'   = roundToHour dt
>     key   = (dt', ftype)
>     query = "SELECT wind_speed \n\
>              \FROM forecasts \n\
>              \INNER JOIN weather_dates \n\
>              \ON weather_date_id = weather_dates.id \n\
>              \WHERE weather_dates.date = ? AND forecast_type_id = ?"
>     xs    = [toSql' dt', toSql ftype]

> getWindMPH :: IORef (M.Map (Int, Int) (Maybe Float)) -> Connection -> Int -> DateTime -> IO (Maybe Float)
> getWindMPH cache cnn ftype dt = withCache key cache $
>     fetchWind cnn dt' ftype query xs "wind_speed_mph" True
>   where
>     dt'   = roundToHour dt
>     key   = (dt', ftype)
>     query = "SELECT wind_speed_mph \n\
>              \FROM forecasts \n\
>              \INNER JOIN weather_dates \n\
>              \ON weather_date_id = weather_dates.id \n\
>              \WHERE weather_dates.date = ? AND forecast_type_id = ?"
>     xs    = [toSql' dt', toSql ftype]

> getW2Wind :: IORef (M.Map (Int, Int) (Maybe Float)) -> Connection -> Int -> DateTime -> IO (Maybe Float)
> getW2Wind cache cnn ftype dt = withCache key cache $
>     -- Note: returns Nothing if this first query fails
>     fetchWind cnn dt' ftype query xs "" False
>   where
>     dt'   = roundToHour dt
>     key   = (dt', 0)
>     query = "SELECT wind_speed \n\
>              \FROM weather_station2 \n\
>              \INNER JOIN weather_dates \n\
>              \ON weather_date_id = weather_dates.id \n\
>              \WHERE weather_dates.date = ?"
>     xs    = [toSql' dt']

However, opacity and system temperature (tsys) are values forecast dependent
on frequency.

> getOpacityAndTSys' = do
>     cache <- newIORef M.empty
>     return (getOpacity cache, getTSys cache)

> fetchOpacityAndTSys cnn dt freqIdx ftype = handleSqlError $ do
>     result <- quickQuery' cnn query xs
>     case result of
>       [[opacity, tsys]] -> return (fromSql' opacity, fromSql' tsys)
>       _                 -> fetchAnyOpacityAndTSys cnn dt freqIdx --return (Nothing, Nothing)
>   where
>     -- Crazy nested JOIN across multiple tables to emulate MySQL INNER JOIN!
>     query = "SELECT opacity, tsys\n\
>             \FROM forecast_by_frequency\n\
>             \JOIN (forecasts JOIN weather_dates ON forecasts.weather_date_id = weather_dates.id) ON forecasts.id = forecast_by_frequency.forecast_id\n\
>             \WHERE weather_dates.date = ? AND\n\
>             \forecast_by_frequency.frequency = ? AND\n\
>             \forecasts.forecast_type_id = ?"
>     xs    = [toSql' dt, toSql freqIdx, toSql ftype]

> fetchAnyOpacityAndTSys cnn dt freqIdx = handleSqlError $ do
>   result <- quickQuery' cnn query xs
>   case result of
>     [opacity, tsys]:_ -> return (fromSql' opacity, fromSql' tsys)
>     _                 -> return (Nothing, Nothing)
>     where
>       query = "SELECT opacity, tsys\n\
>             \FROM forecast_by_frequency\n\
>             \JOIN (forecasts JOIN weather_dates ON forecasts.weather_date_id = weather_dates.id) ON forecasts.id = forecast_by_frequency.forecast_id\n\
>             \WHERE weather_dates.date = ? AND\n\
>             \forecast_by_frequency.frequency = ?\n\
>             \ORDER BY forecasts.forecast_type_id"
>       xs    = [toSql' dt, toSql freqIdx]

> getOpacity :: IORef (M.Map (Int, Int, Int) (Maybe Float, Maybe Float)) -> Connection -> Int -> DateTime -> Frequency -> IO (Maybe Float)
> getOpacity cache cnn ftype dt frequency = liftM fst. withCache key cache $
>     fetchOpacityAndTSys cnn dt' freqIdx ftype
>   where
>     dt'     = roundToHour dt
>     freqIdx = freq2Index frequency
>     key     = (dt', freqIdx, ftype)

> getTSys :: IORef (M.Map (Int, Int, Int) (Maybe Float, Maybe Float)) -> Connection -> Int -> DateTime -> Frequency -> IO (Maybe Float)
> getTSys cache cnn ftype dt frequency = liftM snd . withCache key cache $
>     fetchOpacityAndTSys cnn dt' freqIdx ftype
>   where
>     dt'     = roundToHour dt
>     freqIdx = freq2Index frequency
>     key     = (dt', freqIdx, ftype)

> getTotalStringency' = caching getTotalStringency
> getMinOpacity'      = caching getMinOpacity
> getMinTSysPrime'    = caching getMinTSysPrime

> caching f = liftM f $ newIORef M.empty

> withCache :: Ord k => k -> IORef (M.Map k a) -> IO a -> IO a
> withCache key cache action = do
>     map <- readIORef cache
>     case M.lookup key map of
>         Just val -> return val
>         Nothing  -> do
>             val <- action
>             modifyIORef cache $ M.insert key val
>             return val
              
> getTotalStringency :: IORef (M.Map (Int, Int) (Maybe Float)) -> Connection -> Frequency -> Radians -> IO (Maybe Float)
> getTotalStringency cache conn frequency elevation = withCache key cache $
>     getFloat conn query [toSql freqIdx, toSql elevIdx]
>   where
>     freqIdx = freq2Index frequency
>     elevIdx = round . rad2deg $ elevation
>     key     = (freqIdx, elevIdx)
>     query   = "SELECT total FROM stringency\n\
>               \WHERE frequency = ? AND elevation = ?"

> getMinOpacity :: IORef (M.Map (Int, Int) (Maybe Float)) -> Connection -> Frequency -> Radians -> IO (Maybe Float)
> getMinOpacity cache conn frequency elevation = withCache key cache $
>     getFloat conn query [toSql freqIdx, toSql elevIdx]
>   where
>     freqIdx = freq2Index frequency
>     elevIdx = round . rad2deg $ elevation
>     key     = (freqIdx, elevIdx)
>     query   = "SELECT opacity FROM min_weather\n\
>               \WHERE frequency = ? AND elevation = ?"

> getMinTSysPrime :: IORef (M.Map (Int, Int) (Maybe Float)) -> Connection -> Frequency -> Radians -> IO (Maybe Float)
> getMinTSysPrime cache conn frequency elevation = withCache key cache $
>     getFloat conn query [toSql freqIdx, toSql elevIdx]
>   where
>     freqIdx = freq2Index frequency
>     elevIdx = round . rad2deg $ elevation
>     key     = (freqIdx, elevIdx)
>     query   = "SELECT prime FROM t_sys\n\
>               \WHERE frequency = ? AND elevation = ?"

Creates a connection to the weather forecast database.

> connect :: IO Connection
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ weatherDB ++ " user=dss"


Helper function to determine the desired forecast type given two DateTimes.
forecastType takes both 'now' and a forecastTime since we have to be 
backwards compatible w/ our unit tests: requests for 2006 weather use a
deprecated method for computing the forecast_type_id and use 12 hour forecasts,
where as in Dec. of 2009 we went to 6 hour forecasts and compute the 
forecast_type_id correctly using the forecast_time from the DB.

> forecastType :: DateTime -> DateTime -> DateTime -> Int
> forecastType target now ft = case year of
>                           2006 -> forecastType2006 target now fTypes2006 1
>                           _    -> forecastType' target ft fTypes 9
>   where
>     (year, _, _, _, _, _) = toGregorian target
>     fTypes2006 = [12, 24, 36, 48, 60]
>     fTypes = [t*6 | t <- [1 .. 16]]

This method is only for unit tests.

> forecastType2006 :: DateTime -> DateTime -> [Int] -> Int -> Int
> forecastType2006 target now forecast_types offset =
>     -- this < is wrong: should be <=; that's why this only is used
>     -- for unit tests
>     case dropWhile (< difference) forecast_types of
>         []     -> length forecast_types
>         (x:xs) -> fromJust (elemIndex x forecast_types) + offset
>   where difference = (target - now) `div` 3600

Get the forecast_type_id that is used to represent the difference between 
the two given timestamps.  Note that the  

> forecastType' :: DateTime -> DateTime -> [Int] -> Int -> Int
> forecastType' target origin forecast_types offset = 
>     case dropWhile (<= difference) forecast_types of
>         []     -> length forecast_types
>         (x:xs) -> fromJust (elemIndex x forecast_types) + offset
>   where difference = (target - origin) `div` 3600

> prop_constrained target now ft = forecastType target now ft `elem` forecast_types
>   where forecast_types = [1..5]

Get latest forecast time from the DB for given timestamp

> getForecastTime :: Connection -> DateTime -> IO DateTime
> getForecastTime cnn dt = handleSqlError $ do  
>   result <- quickQuery' cnn query xs
>   case result of
>     [forecastTime]:_ -> return $ sqlToDateTime forecastTime
>     _                -> return $ fromGregorian 2000 1 1 0 0 0 -- TBF!
>     where
>       query = "SELECT date FROM forecast_times WHERE date <= ? ORDER BY date DESC"
>       xs = [toSql' dt]

> sqlToDateTime :: SqlValue -> DateTime
> sqlToDateTime dt = fromJust . fromSqlString . fromSql $ dt

Helper function to get singular Float values out of the database.

> getFloat :: Connection -> String -> [SqlValue] -> IO (Maybe Float)
> getFloat conn query xs = handleSqlError $ do
>     result <- quickQuery' conn query xs
>     case result of
>         [[SqlNull]] -> return Nothing
>         [[x]] -> return $ Just (fromSql x)
>         [[]]  -> return Nothing
>         []    -> return Nothing
>         -- TBF: This match can cause failures for legitimate querys, e.g.,
>         -- "SELECT wind_speed FROM forecasts"
>         x     -> fail "There is more than one forecast with that time stamp."

Just some test functions to make sure things are working.

> testWeather = do
>     w <- getWeather now
>     return ( wind w target
>            , w2_wind w target
>            , opacity w target frequency
>            , tsys w target frequency
>            , totalStringency w frequency elevation
>            , minOpacity w frequency elevation
>            , minTSysPrime w frequency elevation)
>   where 
>     frequency = 2.0 :: Float
>     elevation = pi / 4.0 :: Radians
>     now       = Just (fromGregorian 2004 05 03 12 00 00)
>     target    = fromGregorian 2004 05 03 12 00 00

Quick Check Properties:

TBF: 1. can't use more then 100 connections
TBF: 2. Weather is screwed up, but these don't always find the problems: need to run
more then 100 tests.

> prop_validWeather = forAll gen2006Date $ \dt ->
>                     forAll genLookupFrequency $ \f ->
>                     forAll genLookupElevation $ \el ->
>   let values = getValues dt f el in noneAreNothing values &&
>                                windIsPositive values &&
>                                windIsReasonable values
>     where
>       noneAreNothing values = dropWhile (==True) (map (isJust . unsafePerformIO) values) == []
>       windIsPositive values = 0.0 <= fromMaybe (-1.0) (head (map unsafePerformIO values))
>       windIsReasonable values = 200.0 >= fromMaybe 201.0 (head (map unsafePerformIO values))
>       getValues dt f el = unsafePerformIO $ do
>         let target = dt
>         w' <- theWeather
>         w <- newWeather w' (Just dt)
>         return [wind w target
>             , opacity w target f
>             , tsys w target f
>             , totalStringency w f el
>         -- TBF: no table! but not being used: , minOpacity w f el
>             , minTSysPrime w f el
>             ]

TBF: is this the right way to save connections?

> theWeather = getWeather Nothing

