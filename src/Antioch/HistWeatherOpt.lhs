> module Antioch.HistWeatherOpt (calcStringencyByDate, getFileName, fileExpLength) where

> import Antioch.Settings
> import Antioch.DateTime
> import Antioch.DBUtilities
> import Antioch.Utilities
> import Antioch.Types
> import Antioch.Weather
> import Antioch.Score
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Antioch.HistoricalWeather
> import Control.Monad        (forM_, forM)
> import Control.Monad.Trans  (liftIO)
> import Data.IORef           (newIORef, readIORef, writeIORef)
> import Data.Maybe           (maybe, fromJust)
> import Data.List            ((\\))
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import System.IO.Unsafe     (unsafePerformIO)
> import qualified Data.Map as Map

This module contains code based of HistoricalWeather.lhs used specifically
for optimizing performance.  Currently, updateHistoricalWeather takes about
24 hours to fill a years worth of data, so any improvments will be appreciated.

This is based of fillStringencyTable, except we can pass in the
dates for we which we want to fill, and the final results are put
in a temporary file.

> calcStringencyByDate startDt endDt = do
>     print $ "Updating Stringency table for DB " ++ (show weatherDB)
>     print $ "For dates: " ++ (toSqlString startDt)
>     print $ "To: " ++ (toSqlString endDt)
>     let filename = getFileName startDt 
>     print $ "Into file: " ++ filename
>     cnn <- handleSqlError $ connectDB
>     stringencies <- newIORef Map.empty
>     rts <- getReceiverTemperatures
>     forM_ (getWeatherDates' startDt endDt) $ \dt -> do
>       -- dt offset insures that we get forecasts & not real wind
>       w <- getWeather . Just $ (addMinutes (-60) dt)
>       runScoring w [] rts $ do
>         forM_ allRcvrs' $ \rcvr -> do
>         forM_ (getRcvrFreqIndices rcvr) $ \freq -> do
>         forM_ allElevs' $ \elev -> do
>           getStringency stringencies rcvr freq elev Continuum dt
>           getStringency stringencies rcvr freq elev SpectralLine dt
>     -- it's faster to put all the results in one giant string
>     lns <- getStringencyLines stringencies hrs cnn
>     -- then write it to file
>     writeFile filename $ concat . concat . concat . concat $ lns 
>     print $ "Update of Stringency table complete."
>   where
>     hrs = (endDt `diffMinutes` startDt) `div` 60

> getStringencyLines stringencies hrs cnn = do
>     strs <- readIORef stringencies
>     forM allRcvrs' $ \rcvr -> do
>     forM (getRcvrFreqIndices rcvr) $ \freq -> do
>     forM allElevs' $ \elev -> do
>     forM obsTypes' $ \obsType -> do 
>     getStringencyLine strs rcvr freq elev obsType hrs cnn

Given the map of our results, and all the elements of the key to that map,
returns the string representation of the tuple of keys & value.  This form
is taken so that it can be easily parsed by another program.  Ex:
(8,2000,45,1,1.22)\n

> getStringencyLine stringencies rcvr freq elev obstype hrs cnn = do
>     rcvrId <- getRcvrId cnn rcvr
>     obsTypeId <- getObservingTypeId cnn obstype
>     let str = maybe (0.0 :: Float) (\c -> fromIntegral hrs / fromIntegral c) $ Map.lookup (rcvr, freq, elev, obstype) stringencies
>     return $ (show (rcvrId, freq, elev, obsTypeId, str)) ++  "\n"

We simply need to create a filename that is semi-unique.  Ex:
stringency2006-01-01.txt

> getFileName :: DateTime -> String
> getFileName dt =  "stringency" ++ (take 10 $ toSqlString dt) ++ ".txt"

> getWeatherDates' s e = [(h * 60) `addMinutes` s | h <- [0 .. (hrs - 1)]]
>   where
>     hrs = (e `diffMinutes` s) `div` 60

> allRcvrs' = [Rcvr_RRI .. RcvrArray18_26] \\ [Zpectrometer]

> allElevs' :: [Int]
> allElevs' = [5 .. 90]

> obsTypes' = [Continuum, SpectralLine]

> fileExpLength = (length allElevs') * (length $ concatMap getRcvrFreqIndices allRcvrs') * (2) 

Deprecated: turns out, calling this for each stringency value is WAY to
slow (IO bound)

> {-
> writeStringency filename stringencies rcvr freq elev obstype = do
>     let str = maybe (0.0 :: Float) (\c -> fromIntegral hours / fromIntegral c) $ Map.lookup (rcvr, freq, elev, obstype) stringencies
>     let line = (show (rcvrId, freq, elev, obsTypeId, str)) ++  "\n"
>     appendFile filename line
>   where
>     rcvrId = getRcvrId' rcvr
>     obsTypeId = getObservingTypeId' obstype
> -}

