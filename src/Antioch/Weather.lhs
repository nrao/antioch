> {-# OPTIONS -XMultiParamTypeClasses -XScopedTypeVariables #-}

> module Antioch.Weather where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Receiver  (shiftFreqIntoRange)
> import Antioch.Utilities
> import Antioch.Generators
> import Antioch.Settings  (weatherDB, weatherUnitTestDB)
> import Control.Exception (IOException, bracketOnError, catch)
> import Control.Monad     (liftM)
> -- import Control.Monad.Trans (liftIO)
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
>   , minOpacity      :: Frequency -> Radians -> IO (Maybe Float)
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
>       -- Nothing -> cannot just use getWeather' . Just $ now  TBF Why not?
>       Just x  -> getWeatherSafe x

> getWeatherTest dt = do
>     now <- maybe getCurrentTimeSafe return dt
>     case dt of
>       Nothing -> getWeatherSafeTest now
>       Just x  -> getWeatherSafeTest x

Used for simulations/tests to ensure that we always get data, no matter what
year's worth of weather is in the database, by modifiying the date.

> getWeatherSafe, getWeatherSafeTest :: DateTime -> IO Weather

> getWeatherSafe = getWeather' . Just -- dateSafe 

> getWeatherSafeTest = getWeatherTest' . Just . dateSafe 

Right now, the only historical weather we have is 2006.
TBF: shouldn't we be able to put 2007, 2008 in there now? No, only 2006 in DB!
However, we are importing the latest weather forecasts into this DB,
so we've deprecated 'dateSafe'.

> dateSafe :: DateTime -> DateTime
> dateSafe dt = if (year == 2006) then dt else replaceYear 2006 dt
>   where
>     (year, _, _, _, _, _) = toGregorian dt
> -- TBF: do this when you have more then one year:
> --dateSafe dt = if (any (==year) [2006, 2007, 2008]) then dt else replaceYear 2006 dt

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

> freqIndices :: [Int]
> freqIndices = [100, 200 .. 900] ++ [1000, 2000 .. 51000] ++ [52000, 54000 .. 120000]

Frequency in GHz (float) to GHz (integer) 2 to 120

> freq2ForecastIndex' :: Frequency -> Int
> freq2ForecastIndex' = min 120 . max 2 . round

> freq2ForecastIndex :: Frequency -> Int
> freq2ForecastIndex f = if f' > 52 && odd f' then f' + 1 else f'
>   where
>     f' = freq2ForecastIndex' f

Frequency in GHz (float) to MHz (integer) 100 to 120,000 (sparsely)

> freq2HistoryIndex' :: Frequency -> Int
> freq2HistoryIndex' freq = min 120000 . max 100 $ f -- . round . (*1000.0) 
>   where
>     -- ex: 0.256 GHz -> 200 MHz
>     f | freq < 1.0   = (*100) . round . (*10) $ freq 
>       | otherwise = (*1000) . round $ freq 

> freq2HistoryIndex :: Frequency -> Int
> freq2HistoryIndex f = if f' > 52000 && odd' f' then f' + 1000 else f'
>   where
>     f' = freq2HistoryIndex' f
>     odd' = odd . round . (/1000) . fromIntegral

The elevation range of the weather DB is from 5 - 90 degrees.

> elev2Index :: Radians -> Int
> elev2Index =  min 90 . max 5 . round . rad2deg

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
>     result <- withCache key cache $ fetchForecastData cnn dt ftype column
>     -- print ("getWind", toSqlString dt, result)
>     return result
>   where
>     column = "wind_speed"
>     key = (dt, ftype, column)

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

> correctWindSpeed :: DateTime -> Float -> Float
> correctWindSpeed dt w = correctWindSpeed' cfs w'
>   where
>     cfs
>         | isPTCSDayTime dt = windDayCoeff
>         | otherwise        = windNightCoeff
>     w' = mph2ms w

Miles per hour to meters per second conversion

> mph2ms :: Float -> Float
> mph2ms w = w / 2.237

PTCS day/night correction for meters per second

> correctWindSpeed' :: [Float] -> Float -> Float
> correctWindSpeed' cfs w = sum . map (\x -> (fst x)*w**(snd x)) . zip (reverse cfs) $ [0..]

> windDayCoeff = [-1.70737148e-04,  6.56523342e-03,
>                 -9.82652357e-02,  7.21325467e-01,
>                 -2.68827245e+00,  5.24223121e+00,
>                 -7.61618314e-01]::[Float]
> windNightCoeff = [-3.38584062e-04,  1.19917649e-02,
>                   -1.61474697e-01,  1.02041521e+00,
>                   -2.98028690e+00,  3.89258501e+00,
>                   -5.69079000e-01]::[Float]

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
>     -- print ("getTotalStringency", frequency, elevation, result)
>     return result
>   where
>     frequency' = shiftFreqIntoRange rcvr frequency
>     freqIdx = freq2HistoryIndex frequency'
>     elevIdx = round . rad2deg $ elevation
>     obsIdx = if obsType == Continuum then 1 else 0
>     key     = (freqIdx, elevIdx, show rcvr, obsIdx)

> fetchTotalStringency :: Connection -> Int -> Int -> Receiver -> ObservingType -> IO (Maybe Float)
> fetchTotalStringency conn freqIdx elevIdx rcvr obsType = do
>   rcvrId <- getRcvrId conn rcvr
>   obsTypeId <- getObservingTypeId conn obsType
>   getFloat conn query [toSql freqIdx, toSql elevIdx, toSql rcvrId, toSql obsTypeId]
>     where
>       query   = "SELECT total FROM stringency\n\
>                 \WHERE frequency = ? AND elevation = ?\n\
>                 \AND receiver_id = ? AND observing_type_id = ?"

> getMinOpacity :: IORef (M.Map (Int, Int) (Maybe Float)) -> Connection -> Frequency -> Radians -> IO (Maybe Float)
> getMinOpacity cache conn frequency elevation = do
>     result <- withCache key cache $ getFloat conn query [toSql freqIdx, toSql elevIdx]
>     -- print ("getMinOpacity", frequency, elevation, result)
>     return result
>   where
>     freqIdx = freq2HistoryIndex frequency
>     elevIdx = round . rad2deg $ elevation
>     key     = (freqIdx, elevIdx)
>     query   = "SELECT opacity FROM min_weather\n\
>               \WHERE frequency = ? AND elevation = ?"

> getMinTSysPrime :: IORef (M.Map (Int, Int, String) (Maybe Float)) -> Connection -> Frequency -> Radians -> Receiver -> IO (Maybe Float)
> getMinTSysPrime cache conn frequency elevation rcvr = do
>     result <- withCache key cache $ fetchMinTSysPrime conn freqIdx elevIdx rcvr 
>     rcvrId <- getRcvrId conn rcvr
>     return result
>   where
>     frequency' = shiftFreqIntoRange rcvr frequency
>     freqIdx = freq2HistoryIndex frequency'
>     -- guard against Weather server returning nothing for el's < 5.0.
>     elevation' = max (deg2rad 5.0) elevation
>     elevIdx = round . rad2deg $ elevation'
>     key     = (freqIdx, elevIdx, show rcvr)

> fetchMinTSysPrime :: Connection -> Int -> Int -> Receiver -> IO (Maybe Float)
> fetchMinTSysPrime conn freqIdx elevIdx rcvr = do
>   rcvrId <- getRcvrId conn rcvr
>   --rcvrId <- getRcvrId conn $ max Rcvr1_2 rcvr
>   -- TBF, WTF: PF rcvrs aren't in t_sys table because we aren't
>   -- calculating them below 2 GHz.  So for now this slight of hand.
>   --let rcvrId = min 8 rcvrId'
>   -- print ("fetchMinTSysPrime", freqIdx, elevIdx, rcvrId) need for Dana
>   getFloat conn query [toSql freqIdx, toSql elevIdx, toSql rcvrId]
>     where
>       query   = "SELECT total FROM t_sys\n\
>                 \WHERE frequency = ? AND elevation = ? AND receiver_id = ?"

Creates a connection to the weather forecast database.

> connect, connectTest :: IO Connection

> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ weatherDB ++ " user=dss"

> connectTest = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ weatherUnitTestDB ++ " user=dss"

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
>     _                -> return $ fromGregorian 2000 1 1 0 0 0 -- TBF!
>     where
>       query = "SELECT date FROM forecast_times WHERE date <= ? ORDER BY date DESC"
>       xs = [toSql' dt]

> sqlToDateTime :: SqlValue -> DateTime
> sqlToDateTime dt = fromJust . fromSqlString . fromSql $ dt

TBF: this stolen from DSSData.lhs

> getRcvrId :: Connection -> Receiver -> IO Int
> getRcvrId cnn rcvr = do
>     result <- quickQuery' cnn query xs
>     return $ fromSql . head . head $ result 
>   where
>     query = "SELECT id FROM receivers WHERE name = ?;"
>     xs = [toSql . show $ rcvr]

TBF: this is redundant too

> getObservingTypeId :: Connection -> ObservingType -> IO Int
> getObservingTypeId cnn obsType = do
>     result <- quickQuery' cnn query xs
>     return $ fromSql . head . head $ result 
>   where
>     query = "SELECT id FROM observing_types WHERE type = ?;"
>     xs = [fromObservingType obsType]

TBF: so is this is this is this.  this is redundant too.

> fromObservingType :: ObservingType -> SqlValue
> fromObservingType obsType = toSql . toLowerFirst $ show obsType 
>   where
>     toLowerFirst x = if x == "SpectralLine" then "spectral line" else [toLower . head $ x] ++ tail x

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
>            , gbt_wind w target
>            , opacity w target frequency
>            , tsys w target frequency
>            , totalStringency w frequency elevation
>            --, minOpacity w frequency elevation
>            , minTSysPrime w frequency elevation)
>   where 
>     frequency = 2.0 :: Float
>     elevation = pi / 4.0 :: Radians
>     now       = Just (fromGregorian 2006 05 03 12 00 00)
>     target    = fromGregorian 2006 05 03 12 00 00

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
>             , totalStringency w f el Rcvr1_2 SpectralLine
>         -- TBF: no table! but not being used: , minOpacity w f el
>             , minTSysPrime w f el Rcvr1_2
>             ]

TBF: is this the right way to save connections?

> theWeather = getWeather Nothing

