Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

 module Antioch.HistoricalWeather where

> module Antioch.HistoricalWeather
>     (updateHistoricalWeather
>    , getStringency
>    , truncateTable
>    , fillTsysTable
>    , connectDB
>    , showTsysTable
>    , showStringencyTable
>    , getWeatherDates
>    , stringencyLimit
>    , tSysPrimeNow'
>    , getMinEffSysTemp
>    , getGas
>    , getRcvrFreqIndices) where

> import Antioch.DateTime
> import Antioch.DSSData (getThresholdDefault)
> import Antioch.DBUtilities
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Antioch.Score
> import Antioch.Settings     (weatherDB, weatherHost, databasePort)
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.Weather
> import Antioch.Reports      (plotStringencyVsFrequencySpecLine) 
> import Antioch.Reports      (plotStringencyVsFrequencyCont) 
> import Antioch.Reports      (plotMinEffSysTemp) 
> import Control.Monad        (forM_)
> import Control.Monad.Trans  (liftIO)
> import Data.IORef           (newIORef, readIORef, writeIORef)
> import Data.List            ((\\))
> import Data.Maybe           (maybe)
> import System.Cmd
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import System.IO.Unsafe     (unsafePerformIO)
> import qualified Data.Map as Map

Calculate the minimum value of the effective system temperature.

First calculate the total system temperature (DSPN5.2 equation 7; the
constant value is from spillover (3 K) and the comsic microwave background
(2.7 K), so we should use 5.7 K instead of 6 K.  I think we are already
doing this in the code.  Anyhow, this will be updated in DSPN5.3).

Calculate Stringency.  Loop over the receivers, observing type, frequency,
and elevation.  For observing type we just have continuum and spectral line.
For frequency, we should use the frequencies from the weather forecast
database that reside within the receiver boundaries as specified by the
reciever calibration data.  For elevation, we should use 5,6,...,89,90.
For each hour, over n full years we calculate the tracking error limit,
the observing efficiency limit, and the atmospheric stability limit and 
determine the number of times the condition below is met.  The stringency 
is just the total number of hours divided by the number of hours the 
condition is met.

for jrx in range(len(rx)):                           # loop over receiver
    for jobs in range(len(obs)):                     # loop over observing type (cont=0/line=1)
        for jfreq in range(rx[jrx]):                 # loop over frequency (within receiver)
            for jelev in range(len(elev)):           # loop over elevation
                for iweather in range(len(weather)): # loop over all weather hours
                    
                    # effective system temperature
                    tsysEffective[jrx,jfreq,jelev,iweather] = tsysTotal[jrx,jfreq,jelev,iweather]*exp(opacity[jrx,jfreq,jelev,iweather])

                    # stringency
                    if jobs == 0:  # continuum
                        if trackingErrorLimit >= 1 and obsEffLimit >= self.eta_min and atmStabLimit >= 1:
                            istring[jrx,jobs,jfreq,jelev] += 1
                    else:          # line
                        if trackingErrorLimit >= 1 and obsEffLimit >= self.eta_min:
                            istring[jrx,jobs,jfreq,jelev] += 1

minTsysEffective[jrx,jfreq,jelev] = min(tsysEffective[jrx,jfreq,jelev,])
stringencyTotal[jrx,jobs,jfreq,jelev] = float(len(tsysPrime))/float(istring[jrx,jobs,jfreq,jelev])


> start = fromGregorian 2008 1 1 0 0 0
> end   = fromGregorian 2009 1 1 0 0 0
> hours = (end `diffMinutes` start) `div` 60

> updateHistoricalWeather :: IO ()
> updateHistoricalWeather = do
>     print $ "Updating historical weather in " ++ (show weatherDB)
>     cnn <- handleSqlError $ connectDB
>     {-
>     print "truncating table t_sys"
>     truncateTable cnn "t_sys"
>     fillTsysTable cnn start end
>     showTsysTable
>     -}
>     print "truncating table stringency"
>     truncateTable cnn "stringency"
>     print $ "filling table stringency "  ++ (toSqlString start) ++ " to " ++ (toSqlString end)
>     fillStringencyTable cnn
>     --showStringencyTable
>     disconnect cnn

This method takes start & end datetimes as input so that we can 
also call it from the parrallel version ('genhists').

> allRcvrs' = [Rcvr68_92]

> fillTsysTable cnn startDt endDt = do
>     print $ "filling table t_sys " ++ (toSqlString startDt) ++ " to " ++ (toSqlString endDt)
>     efficiencies <- newIORef Map.empty
>     rts <- getReceiverTemperatures
>     forM_ (getWeatherDates' startDt endDt) $ \dt -> do
>       -- dt offset insures that we get forecasts & not real wind
>       w <- getWeather . Just $ (addMinutes (-60) dt)
>       runScoring w [] rts $ do
>         forM_ allRcvrs' $ \rcvr -> do
>         forM_ (getRcvrFreqIndices rcvr) $ \freq -> do
>         forM_ [5 .. 90 :: Int] $ \elev -> do
>           getMinEffSysTemp efficiencies rcvr freq elev dt
>     effs <- readIORef efficiencies
>     forM_ allRcvrs' $ \rcvr -> do
>     forM_ (getRcvrFreqIndices rcvr) $ \freq -> do
>     forM_ [5 .. 90] $ \elev -> do
>       putMinEffSysTemp cnn effs rcvr freq elev

> fillStringencyTable cnn = do
>     stringencies <- newIORef Map.empty
>     rts <- getReceiverTemperatures
>     forM_ getWeatherDates $ \dt -> do
>       liftIO $ print (toSqlString dt)
>       -- dt offset insures that we get forecasts & not real wind
>       w <- getWeather . Just $ (addMinutes (-60) dt)
>       runScoring w [] rts $ do
>         forM_ allRcvrs' $ \rcvr -> do
>         forM_ (getRcvrFreqIndices rcvr) $ \freq -> do
>         forM_ (getGas rcvr) $ \gas -> do
>         forM_ [5 .. 90 :: Int] $ \elev -> do
>           getStringency stringencies rcvr freq elev Continuum gas dt
>           getStringency stringencies rcvr freq elev SpectralLine gas dt
>             
>     strs <- readIORef stringencies
>     forM_ allRcvrs' $ \rcvr -> do
>     forM_ (getRcvrFreqIndices rcvr) $ \freq -> do
>     forM_ (getGas rcvr) $ \gas -> do
>     forM_ [5 .. 90] $ \elev -> do
>       putStringency cnn strs rcvr freq elev Continuum gas
>       putStringency cnn strs rcvr freq elev SpectralLine gas

> getGas rcvr = if rcvr == Rcvr_PAR then [True, False] else [False]

> getWeatherDates = [(h * 60) `addMinutes` start | h <- [0 .. (hours - 1)]]

> getWeatherDates' startDt endDt = [(h * 60) `addMinutes` startDt | h <- [0 .. (hours' - 1)]]
>   where
>     hours' = (endDt `diffMinutes` startDt) `div` 60

---------------Min. Effective System Temperature---------------

> -- getMinEffSysTemp :: IOBase.IORef -> Receiver -> Int -> Int -> DateTime -> IO ()
> getMinEffSysTemp efficiencies rcvr freq elev dt = do
>     --liftIO $ print $ "getMinEffSysTemp: " ++ (toSqlString dt)
>     new <- tSysPrimeNow' rcvr f e dt
>     liftIO $ alter efficiencies (updateEff new) (rcvr, freq, elev)
>   where
>     f = fromIntegral freq / 1000.0  -- MHz -> GHz
>     e = fromIntegral elev

> alter ref f key = do
>     map <- readIORef ref
>     f (Map.lookup key map) $ \new -> case new of
>       Just val -> writeIORef ref $! Map.insert key val map
>       Nothing  -> return ()

> updateEff Nothing    Nothing    k = k Nothing
> updateEff Nothing    (Just old) k = k $! Just old
> updateEff (Just new) Nothing    k = k $! Just new
> updateEff (Just new) (Just old) k
>     | new < old = k $! Just new
>     | otherwise = k $! Just old

We originally were able to use Score.tSysPrime directly, but for
maximizing performance, we wanted to use the 'bestTsys/Opacity' methods
below.  These methods allow us to maximize use of the cache and minimize
our hits to the DB.  Otherwise, it could take weeks to fill the t_sys
table.

> -- tSysPrimeNow' :: Antioch.Types.Receiver -> Float -> Float -> Antioch.DateTime.DateTime -> RWST ScoringEnv [Trace] () IO (Maybe Float)
> tSysPrimeNow' rcvr freq elev dt = do
>   rt   <- receiverTemperatures
>   -- here we are using the 'best' methods to get close to "real" weather
>   w    <- weather
>   tk'  <- liftIO $ bestTsys w dt freq
>   zod' <- liftIO $ bestOpacity w dt freq
>   trx' <- liftIO $ getReceiverTemperature rt (Just rcvr) freq
>   let za = pi/2 - (deg2rad elev) 
>   return $ do 
>       tk <- tk'
>       zod <- zod'
>       trx <- trx'
>       -- finally use common code by calling the tSysPrime' from Score.lhs
>       return $ tSysPrime' trx tk zod za

--------------Stringency--------------------

> getStringency stringencies rcvr freq elev obstype gas dt = do
>     new <- stringencyLimit rcvr f e obstype gas dt
>     --liftIO $ print ("getStringency: ", (toSqlString dt), rcvr, freq, elev, obstype, new, gas)
>     liftIO $ alter stringencies (updateStr new) (rcvr, freq, elev, obstype, gas)
>   where
>     f = fromIntegral freq
>     e = fromIntegral elev

> updateStr :: (Num t) => Bool -> Maybe t -> (Maybe t -> b) -> b
> updateStr False Nothing    k = k $! Just 0
> updateStr False (Just old) k = k $! Just old
> updateStr True  Nothing    k = k $! Just 1
> updateStr True  (Just old) k = let r = old + 1 in r `seq` (k $! Just r)

A combination of different limiting scoring factors (tracking error limit,
observing efficiency limit, atmospheric stability limit, depending on
observing type). Note that in order to reuse code from Score.lhs, we
create a dummy session to score.

Note: frequency passed in should be in GHz

> -- stringencyLimit :: Receiver -> Frequency -> Float -> ObservingType -> Bool -> DateTime -> RWST ScoringEnv [Trace] () IO Bool
> stringencyLimit rcvr freq elev obstype gas dt = do
>     fs <- observingEfficiencyLimit dt s'
>     --liftIO $ print ("observingEfficiency", fs)
>     if eval fs >= 1
>       then do
>         fs2 <- trackingErrorLimit dt s'
>         if eval fs2 >= 1
>           then do
>             fs3 <- atmosphericStabilityLimit dt s'
>             return $ eval fs3 >= 1
>           else return False
>       else return False
>   where
>     s  = mkDummySession rcvr freq elev obstype gas dt
>     s' = s {trkErrThreshold = getThresholdDefault s}

Creates a dummy session with the given attributes.  The only tricky part
is giving the session a target that will be at the specified elevation
at the specified time.

> mkDummySession rcvr freq elev obstype gas dt = defaultSession {
>       frequency = freq / 1000.0  -- MHz -> GHz
>     , receivers = [[rcvr]]
>     , ra = ra'
>     , dec = dec'
>     , oType = obstype
>     , goodAtmStb = gas
>     }
>   where
>     ra'  = hrs2rad . utc2lstHours $ dt
>     dec' = realToFrac $ (realToFrac . deg2rad $ elev) - (pi/2 - gbtLat)

-------------Database----------------------------

> connectDB = connectPostgreSQL $ "host=" ++ weatherHost ++ " dbname=" ++ weatherDB ++ " port=" ++ databasePort ++ " user=dss"

> truncateTable cnn table = do
>     run cnn ("TRUNCATE TABLE " ++ table ++ " CASCADE") []
>     commit cnn

> putMinEffSysTemp cnn efficiencies rcvr freq elev = do
>     let tsys = maybe 0.0 id $ Map.lookup (rcvr, freq, elev) efficiencies
>     rcvrId <- getRcvrId cnn rcvr
>     run cnn query (xs rcvrId tsys)
>     commit cnn
>   where
>     query = "INSERT INTO t_sys (receiver_id, frequency, elevation, total, prime) VALUES (?, ?, ?, ?, 0.0)"
>     xs rcvrId tsys = [toSql rcvrId, toSql freq, toSql elev, toSql tsys]

> putStringency cnn stringencies rcvr freq elev obstype gas = do
>     if rcvr == Rcvr_PAR then putStringencyMustang cnn stringencies rcvr freq elev obstype gas 
>                         else putStringency' cnn stringencies rcvr freq elev obstype gas

When we are inserting stringencies for Mustang we need to also populate the stringency_parameters table.

> putStringencyMustang cnn stringencies rcvr freq elev obstype gas = do
>     rcvrId <- getRcvrId cnn rcvr
>     obsTypeId <- getObservingTypeId cnn obstype
>     putStringency' cnn stringencies rcvr freq elev obstype gas
>     putStringencyParameters cnn rcvrId obsTypeId freq elev gas

> putStringency' cnn stringencies rcvr freq elev obstype gas = do
>     let str = maybe (0.0 :: Float) (\c -> fromIntegral hours / fromIntegral c) $ Map.lookup (rcvr, freq, elev, obstype, gas) stringencies
>     rcvrId <- getRcvrId cnn rcvr
>     obsTypeId <- getObservingTypeId cnn obstype
>     run cnn query (xs rcvrId obsTypeId str)
>     commit cnn
>   where
>     query = "INSERT INTO stringency (receiver_id, observing_type_id, frequency, elevation, total) VALUES (?, ?, ?, ?, ?)"
>     xs rcvrId obsTypeId str = [toSql rcvrId, toSql obsTypeId, toSql freq, toSql elev, toSql str]

> putStringencyParameters cnn rcvrId obsTypeId freq elev gas = do
>     strId <- getStrId cnn rcvrId obsTypeId freq elev 
>     run cnn query (xs strId gas)
>     commit cnn
>   where
>     query = "INSERT INTO stringency_parameters VALUES (default, ?, ?)"
>     xs strId gas = [toSql strId, toSql gas]

------------Plots------------------------------------

Force the user to check their results.

> showTsysTable = do
>   plotMinEffSysTemp
>   system "xv minEffSysTemp.png &"


> showStringencyTable = do
>     plotStringencyVsFrequencySpecLine
>     system "xv strinFreqSpecLine.png &"
>     plotStringencyVsFrequencyCont
>     system "xv strinFreqCont.png &"

