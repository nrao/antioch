> {-# OPTIONS -XMultiParamTypeClasses -XScopedTypeVariables #-}

> module Antioch.Weather where

> import Antioch.DateTime
> import Antioch.DBUtilities
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.Generators
> import Antioch.Settings  (weatherDB, weatherHost, weatherUnitTestDB, databasePort)
> import Control.Exception (IOException, bracketOnError, catch)
> import Control.Monad     (liftM)
> import Data.Convertible
> import Data.IORef
> import Data.Char         (toLower) 
> import Data.List         (elemIndex)
> import Data.Maybe        (fromJust, maybe, isJust, fromMaybe)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Prelude hiding    (catch)
> import Test.QuickCheck
> import qualified Data.Map as M
> import System.IO.Unsafe  (unsafePerformIO)
> import Text.Printf

This module provides an interface to the 'weather' DB that holds:
   * forecasted weather values (exs: wind, opacities, etc.)
   * actual weather values from the gbt (exs: wind, irradiance, etc.)
   * historical weather results (exs: stringency, etc.)

Note that this module uses caches for optimization.  There is a cache
for each different table we are pulling values from.

> data Weather = Weather {
>     wind            :: DateTime -> IO (Maybe Float)  -- m/s
>   , wind_mph        :: DateTime -> IO (Maybe Float)  -- mph
>   , irradiance      :: DateTime -> IO (Maybe Float)  -- 
>   , gbt_wind        :: DateTime -> IO (Maybe Float)  -- m/s
>   , gbt_irradiance  :: DateTime -> IO (Maybe Float)  -- 
>   , opacity         :: DateTime -> Frequency -> IO (Maybe Float)
>   , tsys            :: DateTime -> Frequency -> IO (Maybe Float)
>   , totalStringency :: Frequency -> Radians -> Receiver -> ObservingType -> IO (Maybe Float)
>   , minOpacity      :: Frequency -> Radians -> Receiver -> IO (Maybe Float)
>   , minTSysPrime    :: Frequency -> Radians -> Receiver -> IO (Maybe Float)
>   , newWeather      :: Maybe DateTime -> IO Weather
>   , forecast        :: DateTime
>   -- the 'best' set of functions ignores forecast type
>   , bestOpacity     :: DateTime -> Frequency -> IO (Maybe Float)
>   , bestTsys        :: DateTime -> Frequency -> IO (Maybe Float)
>   }

The "unsafePerformIO hack" is a way of emulating global variables in GHC.

> {-# NOINLINE globalConnection #-}
> {-# NOINLINE globalConnectionTest #-}
> globalConnection, globalConnectionTest :: IORef Connection
> globalConnection = unsafePerformIO $ connect >>= newIORef
> globalConnectionTest = unsafePerformIO $ connectTest >>= newIORef

This interface method makes sure that dates don't get passed in
that the DB has no data for.  Currently all modules are using this.
DateTime is simply the current time in production use and the
purported time or origin in testing and simulation.

> getWeather, getWeatherTest :: Maybe DateTime -> IO Weather

> getWeather dt = do
>     now <- maybe getCurrentTimeSafe return dt
>     case dt of
>       Nothing -> getWeatherSafe now
>       Just x  -> getWeatherSafe x

> getWeatherTest dt = do
>     now <- maybe getCurrentTimeSafe return dt
>     case dt of
>       Nothing -> getWeatherSafeTest now
>       Just x  -> getWeatherSafeTest x

Used for simulations/tests to ensure that we always get data, no matter what
year's worth of weather is in the database, by modifiying the date.

> getWeatherSafe, getWeatherSafeTest :: DateTime -> IO Weather

> getWeatherSafe = getWeather' . Just . dateSafe 

> getWeatherSafeTest = getWeatherTest' . Just . dateSafe 

Right now, the only historical weather we have is 2006.
     We've deprecated 'dateSafe' for production use, however it is still
     used for look ahead simulations.  See Story,
     https://www.pivotaltracker.com/story/show/2543985

> dateSafe :: DateTime -> DateTime
> dateSafe dt = dt -- if (year == 2006) then dt else replaceYear 2006 dt
>   where
>     (year, _, _, _, _, _) = toGregorian dt

> getCurrentTimeSafe = do
>   dt <- getCurrentTime
>   return $ dateSafe dt

> getWeather', getWeatherTest' :: Maybe DateTime -> IO Weather
> getWeather' now = readIORef globalConnection >>= \cnn -> updateWeather cnn now
> getWeatherTest' now = readIORef globalConnectionTest >>= \cnn -> updateWeather cnn now

Here the various caches are initialized.

> updateWeather :: Connection -> Maybe DateTime -> IO Weather
> updateWeather conn now = do
>     now'' <- maybe getCurrentTimeSafe return now
>     let now' = roundToHour now''
>     ft   <- getForecastTime conn now'
>     (windf, wind_mphf, irradiancef) <- getForecasts'
>     (gbt_windf, gbt_irrf) <- getGbtWeather'
>     (opacityf, tsysf)   <- getOpacityAndTSys'
>     (bestOpacityf, bestTsysf)   <- getBestOpacityAndTSys'
>     stringencyf         <- getTotalStringency'
>     minOpacityf         <- getMinOpacity'
>     minTSysf            <- getMinTSysPrime'
>     return Weather {
>         wind            = pin ft now' $ windf conn
>       , wind_mph        = pin ft now' $ wind_mphf conn
>       , irradiance      = pin ft now' $ irradiancef conn
>       , gbt_wind        = gbt_windf conn
>       , gbt_irradiance  = gbt_irrf conn
>       , opacity         = pin ft now' $ opacityf conn
>       , tsys            = pin ft now' $ tsysf conn
>       , totalStringency = stringencyf conn
>       , minOpacity      = minOpacityf conn
>       , minTSysPrime    = minTSysf conn
>       , newWeather      = updateWeather conn
>       , forecast        = now'
>       , bestOpacity     = bestOpacityf conn 
>       , bestTsys        = bestTsysf conn 
>       }

forecastType takes both 'now' and a forecastTime since we have to be 
backwards compatible w/ our unit tests: requests for 2006 weather use a
deprecated method for computing the forecast_type_id and use 12 hour forecasts,
where as in Dec. of 2009 we went to 6 hour forecasts and compute the 
forecast_type_id correctly using the forecast_time from the DB.

> pin :: DateTime -> DateTime -> (Int -> DateTime -> a) -> DateTime -> a
> pin forecastTime now f target = f (forecastType target' now forecastTime) target'
>   where
>     target' = roundToHour . dateSafe $ target

> toSql' = toSql . toSqlString

> fromSql' SqlNull = Nothing
> fromSql' x       = Just . fromSql $ x

The frequency range of the weather DB is from 2 - 120 GHz.
However, the forecast_by_frequency values use the following 
granularity: 2,3 .. 51,52,54,56 .. 118,120.
But the t_sys frequency values 
are: 2,3 .. 120

Both wind speeds and atmospheric temperature are values forecast independently
of frequency.  The initialized caces are created here.

> getForecasts' = do
>     cache <- newIORef M.empty
>     return (getWind cache
>           , getWindMPH cache
>           , getIrradiance cache)

> getGbtWeather' = do
>     cache <- newIORef M.empty
>     return (getGbtWind cache
>           , getGbtIrradiance cache)

The cache for the 'forecasts' table is simply the (datetime, forecast type, 
column name).

> type ForecastFunc = IORef (M.Map (Int, Int, String) (Maybe Float)) -> Connection -> Int -> DateTime -> IO (Maybe Float)

> getWind, getWindMPH, getIrradiance :: ForecastFunc

> getWind cache cnn ftype dt = do
>     result <- withCache key cache $ fetchWindData cnn dt ftype "wind_speed_mph"
>     -- print ("getWind", toSqlString dt, result)
>     return result
>   where
>     key = (dt, ftype, "wind_speed")

> getWindMPH cache cnn ftype dt = do
>     result <- withCache key cache $ fetchForecastData cnn dt ftype column
>     -- print ("getWindMPH", toSqlString dt, result)
>     return result
>   where
>     column = "wind_speed_mph"
>     key = (dt, ftype, column)

> getIrradiance cache cnn ftype dt = do
>     result <- withCache key cache $ fetchForecastData cnn dt ftype column
>     -- print ("getIrradiance", toSqlString dt, result)
>     return result
>   where
>     column = "irradiance"
>     key = (dt, ftype, column)

The cache for the 'gbt_weather' table is simply the (datetime, column name)

> type GbtWeatherFunc = IORef (M.Map (Int, String) (Maybe Float)) -> Connection -> DateTime -> IO (Maybe Float)

> getGbtWind, getGbtIrradiance :: GbtWeatherFunc

> getGbtWind cache cnn dt = do
>     result <- withCache key cache $ fetchGbtWeatherData cnn dt' column
>     -- print ("getGbtWind", toSqlString dt, result)
>     return result
>   where
>     dt' = roundToHour . dateSafe $ dt
>     column = "wind_speed"
>     key = (dt', column)

> getGbtIrradiance cache cnn dt = do
>     result <- withCache key cache $ fetchGbtWeatherData cnn dt' column
>     -- print ("getGbtIrradiance", toSqlString dt, result)
>     return result
>   where
>     dt' = roundToHour . dateSafe $ dt
>     column = "irradiance"
>     key = (dt', column)


Generic method for pulling scalar values from the forecasts table.
Note column and try parameters: if you don't get any results w/ the
given query (by forecast id), try again (getting most recent forecast).

> fetchForecastData cnn dt ftype column = fetchForecastData' cnn dt ftype query xs column True
>   where
>     query = getForecastDataQuery column
>     xs    = [toSql' dt, toSql ftype]

> fetchForecastData' :: Connection -> DateTime -> Int -> String -> [SqlValue] -> String -> Bool -> IO (Maybe Float)
> fetchForecastData' cnn dt ftype query xs column try = handleSqlError $ do
>     result <- quickQuery' cnn query xs
>     case result of
>       [[value]] -> return $ fromSql' value
>       _        -> if try then fetchAnyForecastValue cnn dt ftype column else return Nothing

Code changes for non-correction when wind is greater than 11 meters per second:

> fetchWindData :: Connection -> DateTime -> Int -> String -> IO (Maybe Float)
> fetchWindData cnn dt ftype column = do
>   jmph <- fetchForecastData' cnn dt ftype query xs column True
>   let jmps = do
>       mph <- jmph
>       let mps = mph2mps mph
>       return $ if mps > 11.0 then mps else correctWindSpeed dt mps
>   return jmps
>     where
>       query = getForecastDataQuery column
>       xs    = [toSql' dt, toSql ftype]

Get a value (ex: wind_speed) from the Forecasts table with the most recent
forecast type.
Note: only applicable for columns in the Forecasts table

> fetchAnyForecastValue :: Connection -> DateTime -> Int -> String -> IO (Maybe Float)
> fetchAnyForecastValue cnn dt ftype column = handleSqlError $ do
>   print $ "Value " ++ column ++ " was not found for date: " ++ (show . toSqlString $ dt) ++ " and forecast type: " ++ (show ftype)
>   result <- quickQuery' cnn query xs
>   case result of 
>     [wind]:_ -> return $ fromSql' wind
>     _        -> return Nothing
>     where
>       xs = [toSql' dt]
>       query = getAnyForecastQuery column 

> getForecastDataQuery :: String -> String
> getForecastDataQuery column = printf query column
>   where 
>     query = "SELECT %s \n\
>              \FROM forecasts \n\
>              \INNER JOIN weather_dates \n\
>              \ON weather_date_id = weather_dates.id \n\
>              \WHERE weather_dates.date = ? AND forecast_type_id = ?"

> getAnyForecastQuery :: String -> String
> getAnyForecastQuery column = printf query column
>   where
>     query = "SELECT %s \n\
>            \FROM forecasts \n\
>            \INNER JOIN weather_dates \n\
>            \ON weather_date_id = weather_dates.id \n\
>            \WHERE weather_dates.date = ? \n\
>            \ORDER BY forecast_type_id"

> fetchGbtWeatherData :: Connection -> DateTime -> String -> IO (Maybe Float)
> fetchGbtWeatherData cnn dt column = handleSqlError $ do
>     result <- quickQuery' cnn query xs
>     case result of
>       [[value]] -> return $ fromSql' value
>       _         -> return Nothing
>   where
>     xs = [toSql' dt]
>     query = printf q column
>     q     = "SELECT %s \n\
>              \FROM gbt_weather \n\
>              \INNER JOIN weather_dates \n\
>              \ON weather_date_id = weather_dates.id \n\
>              \WHERE weather_dates.date = ?"

Changes wind miles per hour to PTCS meters to second
with day/night correction

Forecasted wind (mps) to measured wind (mps)

> correctWindSpeed :: DateTime -> Float -> Float
> correctWindSpeed dt w = correctWindSpeed' cfs w
>   where
>     cfs
>         | isPTCSDayTime id dt = windDayCoeff
>         | otherwise           = windNightCoeff 

Miles per hour to meters per second conversion

> mph2mps :: Float -> Float
> mph2mps w = w / 2.237

PTCS day/night correction for meters per second

> correctWindSpeed' :: [Float] -> Float -> Float
> correctWindSpeed' cfs w = sum . map (\x -> (fst x)*w**(snd x)) . zip (reverse cfs) $ [0..]

Coeff. from DSPN6.1 using 1 May 2004 to 1 Mar 2007 weather

> {-
> windDayCoeff = [-1.70737148e-04,  6.56523342e-03,
>                 -9.82652357e-02,  7.21325467e-01,
>                 -2.68827245e+00,  5.24223121e+00,
>                 -7.61618314e-01]::[Float]
> windNightCoeff = [-3.38584062e-04,  1.19917649e-02,
>                   -1.61474697e-01,  1.02041521e+00,
>                   -2.98028690e+00,  3.89258501e+00,
>                   -5.69079000e-01]::[Float]
> -}

Coeff. using 1 Sep 2008 to 31 Aug 2010 weather
(N.B., On 18 Dec 2007 weather forecasts went from 12hr to 6hr updates;
this included a change in the wind speed algorithm.)

> windDayCoeff = [1.03136467e-04,  -4.09837631e-03,
>                 6.52629009e-02,  -5.31163369e-01,
>                 2.33592497e+00,  -5.42793252e+00,
>                 7.12632578e+00,  -1.07039484e+00]::[Float]
> windNightCoeff = [1.62510757e-04,  -6.31744909e-03,
>                   9.84059248e-02,  -7.80041906e-01,
>                   3.28349217e+00,  -6.90486810e+00,
>                   7.06266889e+00,  -1.10385720e+00]::[Float]

However, opacity and system temperature (tsys) are values forecast dependent
on frequency.  Here it's cache is initialized.

> getOpacityAndTSys' = do
>     cache <- newIORef M.empty
>     return (getOpacity cache, getTSys cache)

The 'Best' set of functions avoids dealing with forecast types.

> getBestOpacityAndTSys' = do
>     cache <- newIORef M.empty
>     return (getBestOpacity cache, getBestTSys cache)

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
> getOpacity cache cnn ftype dt frequency = do
>     result <- liftM fst. withCache key cache $ fetchOpacityAndTSys cnn dt freqIdx ftype
>     -- print ("getOpacity", toSqlString dt, frequency, result)
>     return result
>   where
>     freqIdx = freq2ForecastIndex frequency
>     key     = (dt, freqIdx, ftype)

Unlike getOpacity, getBestOpacity avoids specifying a forecast type, and
simply uses fetchAnyOpacityAndTsys to get data from the most recent forecast.

> getBestOpacity :: IORef (M.Map (Int, Int) (Maybe Float, Maybe Float)) -> Connection -> DateTime -> Frequency -> IO (Maybe Float)
> getBestOpacity cache cnn dt frequency = liftM fst. withCache key cache $
>     fetchAnyOpacityAndTSys cnn dt freqIdx 
>   where
>     freqIdx = freq2ForecastIndex frequency
>     key     = (dt, freqIdx)

> getTSys :: IORef (M.Map (Int, Int, Int) (Maybe Float, Maybe Float)) -> Connection -> Int -> DateTime -> Frequency -> IO (Maybe Float)
> getTSys cache cnn ftype dt frequency = do
>     result <- liftM snd . withCache key cache $ fetchOpacityAndTSys cnn dt freqIdx ftype
>     -- print ("getTSys", toSqlString dt, frequency, result)
>     return result
>   where
>     freqIdx = freq2ForecastIndex frequency
>     key     = (dt, freqIdx, ftype)

Unlike getTSys, getBestTSys avoids specifying a forecast type, and
simply uses fetchAnyOpacityAndTsys to get data from the most recent forecast.

> getBestTSys :: IORef (M.Map (Int, Int) (Maybe Float, Maybe Float)) -> Connection -> DateTime -> Frequency -> IO (Maybe Float)
> getBestTSys cache cnn dt frequency = liftM snd . withCache key cache $
>     fetchAnyOpacityAndTSys cnn dt freqIdx 
>   where
>     freqIdx = freq2ForecastIndex frequency
>     key     = (dt, freqIdx)

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
              
> getTotalStringency :: IORef (M.Map (Int, Int, String, Int) (Maybe Float)) -> Connection -> Frequency -> Radians -> Receiver -> ObservingType -> IO (Maybe Float)
> getTotalStringency cache conn frequency elevation rcvr obsType = do
>     result <- withCache key cache $ fetchTotalStringency conn freqIdx elevIdx rcvr obsType 
>     -- print ("getTotalStringency", frequency, freqIdx, elevation, elevIdx, result)
>     return result
>   where
>     freqIdx = freq2HistoryIndex rcvr frequency
>     elevIdx = round . rad2deg $ elevation
>     isCont = if obsType == Continuum then 1 else 0
>     key     = (freqIdx, elevIdx, show rcvr, isCont)

> fetchTotalStringency :: Connection -> Int -> Int -> Receiver -> ObservingType -> IO (Maybe Float)
> fetchTotalStringency conn freqIdx elevIdx rcvr obsType = do
>   rcvrId <- getRcvrId conn rcvr
>   obsTypeId <- getObservingTypeId conn obsType'
>   getFloat conn query [toSql freqIdx, toSql elevIdx, toSql rcvrId, toSql obsTypeId]
>     where
>       obsType'
>         | obsType == Continuum    = Continuum
>         | otherwise               = SpectralLine
>       query   = "SELECT total FROM stringency\n\
>                 \WHERE frequency = ? AND elevation = ?\n\
>                 \AND receiver_id = ? AND observing_type_id = ?"

> getMinOpacity :: IORef (M.Map (Int, Int) (Maybe Float)) -> Connection -> Frequency -> Radians -> Receiver -> IO (Maybe Float)
> getMinOpacity cache conn frequency elevation rcvr = do
>     result <- withCache key cache $ getFloat conn query [toSql freqIdx, toSql elevIdx]
>     -- print ("getMinOpacity", frequency, elevation, result)
>     return result
>   where
>     freqIdx = freq2HistoryIndex rcvr frequency
>     elevIdx = round . rad2deg $ elevation
>     key     = (freqIdx, elevIdx)
>     query   = "SELECT opacity FROM min_weather\n\
>               \WHERE frequency = ? AND elevation = ?"

> getMinTSysPrime :: IORef (M.Map (Int, Int, String) (Maybe Float)) -> Connection -> Frequency -> Radians -> Receiver -> IO (Maybe Float)
> getMinTSysPrime cache conn frequency elevation rcvr = do
>     result <- withCache key cache $ fetchMinTSysPrime conn freqIdx elevIdx rcvr 
>     return result
>   where
>     freqIdx = freq2HistoryIndex rcvr frequency
>     -- guard against Weather server returning nothing for el's < 5.0.
>     elevation' = max (deg2rad 5.0) elevation
>     elevIdx = round . rad2deg $ elevation'
>     key     = (freqIdx, elevIdx, show rcvr)

> fetchMinTSysPrime :: Connection -> Int -> Int -> Receiver -> IO (Maybe Float)
> fetchMinTSysPrime conn freqIdx elevIdx rcvr = do
>   rcvrId <- getRcvrId conn rcvr
>   getFloat conn query [toSql freqIdx, toSql elevIdx, toSql rcvrId]
>     where
>       query   = "SELECT total FROM t_sys\n\
>                 \WHERE frequency = ? AND elevation = ? AND receiver_id = ?"

> getLastImportTime :: Connection -> IO DateTime
> getLastImportTime cnn = do
>   result <- quickQuery' cnn query []
>   return $ toImportTime result
>     where
>       query = "SELECT date FROM import_times ORDER BY date DESC LIMIT 1"
>       toImportTime = sqlToDateTime . head . head

Creates a connection to the weather forecast database.

> connect, connectTest :: IO Connection

> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "host=" ++ weatherHost ++ " dbname=" ++ weatherDB ++ " port=" ++ databasePort ++ " user=dss"

> connectTest = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "host=" ++ weatherHost ++ " dbname=" ++ weatherUnitTestDB ++ " port=" ++ databasePort ++ " user=dss"

Helper function to determine the desired forecast type given two DateTimes.
forecastType takes both 'now' and a forecastTime since we have to be 
backwards compatible w/ our unit tests: requests for 2006 weather use a
deprecated method for computing the forecast_type_id and use 12 hour forecasts,
where as in Dec. of 2009 we went to 6 hour forecasts and compute the 
forecast_type_id correctly using the forecast_time from the DB.

> forecastType :: DateTime -> DateTime -> DateTime -> Int
> forecastType target _ ft = forecastType' target ft fTypes 1
>   where
>     fTypes = [t*6 | t <- [1 .. 16]]

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
>     -- the request is for a forecast_time that doesn't have an equivalent
>     -- in the DB - other calls will probably fail, so returning this 
>     -- bogus date should be safe.
>     _                -> return $ fromGregorian 2000 1 1 0 0 0 
>     where
>       query = "SELECT date FROM forecast_times WHERE date <= ? ORDER BY date DESC"
>       xs = [toSql' dt]

> sqlToDateTime :: SqlValue -> DateTime
> sqlToDateTime dt = fromJust . fromSqlString . fromSql $ dt

> getObservingTypeId :: Connection -> ObservingType -> IO Int
> getObservingTypeId cnn obsType = do
>     result <- quickQuery' cnn query xs
>     return $ fromSql . head . head $ result 
>   where
>     query = "SELECT id FROM observing_types WHERE type = ?;"
>     xs = [fromObservingType obsType]

> fromObservingType :: ObservingType -> SqlValue
> fromObservingType obsType = toSql . toLowerFirst $ show obsType 
>   where
>     toLowerFirst x = if x == "SpectralLine" then "spectral line" else [toLower . head $ x] ++ tail x

Helper function to get singular Float values out of the database.
Note:  The given query is expect to return a single result, multiple
       results will produce the failure below.

> getFloat :: Connection -> String -> [SqlValue] -> IO (Maybe Float)
> getFloat conn query xs = handleSqlError $ do
>     result <- quickQuery' conn query xs
>     case result of
>         [[SqlNull]] -> return Nothing
>         [[x]] -> return $ Just (fromSql x)
>         [[]]  -> return Nothing
>         []    -> return Nothing
>         x     -> fail ("The given query returned more than one result. " ++ query)

Just some test functions to make sure things are working.

> testWeather = do
>     w <- getWeather now
>     return ( wind w target
>            , gbt_wind w target
>            , opacity w target frequency
>            , tsys w target frequency
>            , totalStringency w frequency elevation
>            , minTSysPrime w frequency elevation)
>   where 
>     frequency = 2.0 :: Float
>     elevation = pi / 4.0 :: Radians
>     now       = Just (fromGregorian 2006 05 03 12 00 00)
>     target    = fromGregorian 2006 05 03 12 00 00

Quick Check Properties:

Note some of the limitations to this Quick Check property:
1. can't use more then 100 connections
2. Weather *can be* screwed up, but sometimes you need to run more than 100 tests to catch these.


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
>             , totalStringency w f el Rcvr1_2 SpectralLine
>             , minTSysPrime w f el Rcvr1_2
>             ]

> theWeather = getWeather Nothing

