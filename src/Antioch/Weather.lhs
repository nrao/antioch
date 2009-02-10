> {-# OPTIONS -XMultiParamTypeClasses -XScopedTypeVariables #-}

> module Antioch.Weather where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.Generators
> import Control.Exception (IOException, bracketOnError, catch)
> import Control.Monad     (liftM)
> import Data.Convertible
> import Data.IORef
> import Data.List         (elemIndex)
> import Data.Maybe        (fromJust, maybe, isJust)
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
>   , tatm            :: DateTime -> IO (Maybe Float)  -- Kelvin
>   , opacity         :: DateTime -> Frequency -> IO (Maybe Float)
>   , tsys            :: DateTime -> Frequency -> IO (Maybe Float)
>   , totalStringency :: Frequency -> Radians -> IO (Maybe Float)
>   , minOpacity      :: Frequency -> Radians -> IO (Maybe Float)
>   , minTSysPrime    :: Frequency -> Radians -> IO (Maybe Float)
>   , newWeather      :: Maybe DateTime -> IO Weather
>   }

> getWeather     :: Maybe DateTime -> IO Weather
> getWeather now = bracketOnError connect disconnect $ \conn ->
>     updateWeather conn now

> updateWeather :: Connection -> Maybe DateTime -> IO Weather
> updateWeather conn now = do
>     now' <- maybe getCurrentTime return now
>     (windf, tatmf)    <- getWindAndTAtm'
>     (opacityf, tsysf) <- getOpacityAndTSys'
>     stringencyf <- getTotalStringency'
>     minOpacityf <- getMinOpacity'
>     minTSysf    <- getMinTSysPrime'
>     return Weather {
>         wind            = pin now' $ windf conn
>       , tatm            = pin now' $ tatmf conn
>       , opacity         = pin now' $ opacityf conn
>       , tsys            = pin now' $ tsysf conn
>       , totalStringency = stringencyf conn
>       , minOpacity      = minOpacityf conn
>       , minTSysPrime    = minTSysf conn
>       , newWeather      = updateWeather conn
>       }

Used for test to ensure the year is always 2006.

> getWeather' :: DateTime -> IO Weather
> getWeather' = getWeather . Just . replaceYear 2006

> pin              :: DateTime -> (Int -> DateTime -> a) -> DateTime -> a
> pin now f target = f (forecastType target now) target

> toSql' = toSql . toSqlString

> fromSql' SqlNull = Nothing
> fromSql' x       = Just . fromSql $ x

> freq2Index :: Frequency -> Int
> freq2Index = min 50 . max 2 . round

> elev2Index :: Radians -> Int
> elev2Index =  min 90 . max 5 . round . rad2deg

Both wind speed and atmospheric temperature are values forecast independently
of frequency.

> getWindAndTAtm' = do
>     cache <- newIORef M.empty
>     return (getWind cache, getTAtm cache)

> fetchWindAndAtm :: Connection -> DateTime -> Int -> IO (Maybe Float, Maybe Float)
> fetchWindAndAtm cnn dt ftype = handleSqlError $ do
>     result <- quickQuery' cnn query xs
>     case result of
>       [[wind, tatm]] -> return (fromSql' $ wind, fromSql' $ tatm)
>       _              -> return (Nothing, Nothing)
>   where
>     query = "SELECT wind_speed, tatm FROM forecasts WHERE date = ? AND forecast_type_id = ?"
>     xs    = [toSql' dt, toSql ftype]

> getWind :: IORef (M.Map (Int, Int) (Maybe Float, Maybe Float)) -> Connection -> Int -> DateTime -> IO (Maybe Float)
> getWind cache cnn ftype dt = liftM fst . withCache key cache $
>     fetchWindAndAtm cnn dt' ftype
>   where
>     dt' = roundToHour dt
>     key = (dt', ftype)

> getTAtm :: IORef (M.Map (Int, Int) (Maybe Float, Maybe Float)) -> Connection -> Int -> DateTime -> IO (Maybe Float)
> getTAtm cache cnn ftype dt = liftM snd . withCache key cache $
>     fetchWindAndAtm cnn dt' ftype
>   where
>     dt' = roundToHour dt
>     key = (dt', ftype)

However, opacity and system temperature (tsys) are values forecast dependent
on frequency.

> getOpacityAndTSys' = do
>     cache <- newIORef M.empty
>     return (getOpacity cache, getTSys cache)

> fetchOpacityAndTSys cnn dt freqIdx ftype = handleSqlError $ do
>     result <- quickQuery' cnn query xs
>     case result of
>       [[opacity, tsys]] -> return (fromSql' $ opacity, fromSql' $ tsys)
>       _                 -> return (Nothing, Nothing)
>   where
>     query = "SELECT opacity, tsys\n\
>             \FROM forecasts, forecast_by_frequency\n\
>             \WHERE date = ? AND\n\
>             \frequency = ? AND\n\
>             \forecast_type_id = ? AND\n\
>             \forecasts.id = forecast_by_frequency.forecast_id"
>     xs    = [toSql' dt, toSql freqIdx, toSql ftype]

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
> connect = handleSqlError $ connectPostgreSQL "dbname=dss user=dss"

Helper function to determine the desired forecast type given two DateTimes.

> forecastType :: DateTime -> DateTime -> Int
> forecastType target now = 
>     case dropWhile (< difference) forecast_types of
>         []     -> length forecast_types
>         (x:xs) -> fromJust (elemIndex x forecast_types) + 1
>   where difference = (target - now) `div` 3600
>         forecast_types = [12, 24, 36, 48, 60]

> prop_constrained target now = forecastType target now `elem` forecast_types
>   where forecast_types = [1..5]

Helper function to get singular Float values out of the database.

> getFloat :: Connection -> String -> [SqlValue] -> IO (Maybe Float)
> getFloat conn query xs = handleSqlError $ do
>     result <- quickQuery' conn query xs
>     case result of
>         [[SqlNull]] -> return Nothing
>         [[x]] -> return $ Just (fromSql $ x)
>         [[]]  -> return Nothing
>         []    -> return Nothing
>         x     -> fail "There is more than one forecast with that time stamp."

Just some test functions to make sure things are working.

> testWeather = do
>     w <- getWeather now
>     return ( wind w target
>            , tatm w target
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

Make sure we always get something - i.e. not nothing!
TBF: run it enough times, and you get a 2006/1/1 date, and, BANG!!!

> prop_notNothing = forAll gen2006Date $ \dt ->
>   dropWhile (==True) (map isJust (map unsafePerformIO (getValues dt))) == []
>     where
>       getValues dt = unsafePerformIO $ do
>         -- TBF: randomize these three vars as well!
>         let target = dt
>         let f = 2.0 :: Float
>         let el = pi / 4.0 :: Radians
>         w' <- theWeather
>         w <- newWeather w' (Just dt)
>         return $ [wind w target
>         -- TBF: this does not work & is not being used: , tatm w target
>             , opacity w target f
>             , tsys w target f
>             , totalStringency w f el
>         -- TBF: no table! but not being used: , minOpacity w f el
>             , minTSysPrime w f el
>             ]

TBF: is this the right way to save connections?

> theWeather = getWeather Nothing

