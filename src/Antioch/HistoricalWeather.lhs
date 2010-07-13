> module Antioch.HistoricalWeather (updateHistoricalWeather) where

> import Antioch.DateTime
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Antioch.Score
> import Antioch.Settings     (weatherDB)
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.Weather
> import Control.Monad        (forM_)
> import Control.Monad.Trans  (liftIO)
> import Data.IORef           (newIORef, readIORef, writeIORef)
> import Data.List            ((\\))
> import Data.Maybe           (maybe)
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
and elevation.  For observing type we just have continuum and line.
For frequency, we should use the frequencies from the weather forecast
database that reside within the receiver boundaries as specified by the
reciever calibration data.  For elevation, we should use 5,6,...,89,90.
For each hour, over n full years we calculate the tracking error limit,
the observing efficiency limit, and the stability limit and determine the
number of times the condition below is met.  The stringency is just the
total number of hours divided by the number of hours the condition is met.

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


> start = fromGregorian 2006 6 10 0 0 0
> end   = fromGregorian 2006 6 17 0 0 0
> hours = (end `diffMinutes'` start) `div` 60

> updateHistoricalWeather :: IO ()
> updateHistoricalWeather = do
>     cnn <- handleSqlError $ connectDB
>     truncateTable cnn "t_sys"
>     truncateTable cnn "stringency"
>     ericWasHere cnn
>     disconnect cnn

> ericWasHere cnn = do
>     efficiencies <- newIORef Map.empty
>     stringencies <- newIORef Map.empty
>     rts <- getReceiverTemperatures
>     forM_ getWeatherDates $ \dt -> do
>       w <- getWeather . Just $ dt
>       runScoring w [] rts $ do
>         forM_ allRcvrs $ \rcvr -> do
>         forM_ (getRcvrFreqIndices rcvr) $ \freq -> do
>         forM_ [5 .. 90 :: Int] $ \elev -> do
>           getMinEffSysTemp efficiencies rcvr freq elev dt
>           getStringency stringencies rcvr freq elev Continuum dt
>           getStringency stringencies rcvr freq elev SpectralLine dt
>     effs <- readIORef efficiencies
>     strs <- readIORef stringencies
>     forM_ allRcvrs $ \rcvr -> do
>     forM_ (getRcvrFreqIndices rcvr) $ \freq -> do
>     forM_ [5 .. 90] $ \elev -> do
>       putMinEffSysTemp cnn effs rcvr freq elev
>       putStringency cnn strs rcvr freq elev Continuum
>       putStringency cnn strs rcvr freq elev SpectralLine

> getWeatherDates = [(h * 60) `addMinutes'` start | h <- [0 .. hours]]

> allRcvrs = [Rcvr_RRI .. RcvrArray18_26] \\ [Zpectrometer]

> getRcvrFreqIndices rcvr = takeWhile (<=hi) . dropWhile (<lo) $ freqIndices
>   where
>     (lo', hi') = getRcvrRange rcvr
>     [lo,  hi]  = map freq2HistoryIndex [lo', hi']

---------------Min. Effective System Temperature---------------

> getMinEffSysTemp efficiencies rcvr freq elev dt = do
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

> tSysPrimeNow' rcvr freq elev dt = do
>   rt   <- receiverTemperatures
>   trx' <- liftIO $ getReceiverTemperature rt (Just rcvr) freq
>   -- Simply use this line to get results that agree with previous values
>   -- But don't forget that this could be changed in Receiver too! idiot
>   --let trx' = Just $ oldReceiverTemperature dt defaultSession {frequency = freq}
>   -- here we are using the 'best' methods to avoid having to deal
>   -- with specifiying the forecast type
>   w    <- weather
>   tk'  <- liftIO $ bestTsys w dt freq
>   zod' <- liftIO $ bestOpacity w dt freq
>   let za = pi/2 - (deg2rad elev) 
>   return $ do 
>       tk <- tk'
>       zod <- zod'
>       trx <- trx'
>       -- Call the tSysPrime' method used in Score.lhs
>       return $ tSysPrime' trx tk zod za

--------------Stringency--------------------

> getStringency stringencies rcvr freq elev obstype dt = do
>     new <- stringencyLimit rcvr f e obstype dt
>     liftIO $ alter stringencies (updateStr new) (rcvr, freq, elev, obstype)
>   where
>     f = fromIntegral freq
>     e = fromIntegral elev

> updateStr False Nothing    k = k $! Just 0
> updateStr False (Just old) k = k $! Just old
> updateStr True  Nothing    k = k $! Just 1
> updateStr True  (Just old) k = let r = old + 1 in r `seq` (k $! Just r)

A combination of different limiting scoring factors (tracking error limit,
observing efficiency limit, atmospheric stability limit, depending on
observing type). Note that in order to reuse code from Score.lhs, we
create a dummy session to score.

Note: frequency passed in should be in GHz

> stringencyLimit rcvr freq elev obstype dt = do
>     fs <- observingEfficiencyLimit dt s
>     if eval fs >= 1
>       then do
>         fs2 <- trackingErrorLimit dt s
>         if eval fs2 >= 1
>           then do
>             fs3 <- atmosphericStabilityLimit dt s
>             return $ eval fs3 >= 1
>           else return False
>       else return False
>   where
>     s = mkDummySession rcvr freq elev obstype dt

Creates a dummy session with the given attributes.  The only tricky part
is giving the session a target that will be at the specified elevation
at the specified time.

> mkDummySession rcvr freq elev obstype dt = defaultSession {
>       frequency = freq
>     , receivers = [[rcvr]]
>     , ra = ra'
>     , dec = dec'
>     , oType = obstype
>     }
>   where
>     ra'  = hrs2rad . utc2lstHours $ dt
>     dec' = realToFrac $ (realToFrac . deg2rad $ elev) - (pi/2 - gbtLat)

-------------Database----------------------------

> connectDB = connectPostgreSQL $ "dbname=" ++ weatherDB ++ " user=dss"

> truncateTable cnn table = do
>     run cnn ("TRUNCATE TABLE " ++ table) []
>     commit cnn

> putMinEffSysTemp cnn efficiencies rcvr freq elev = do
>     let tsys = maybe 0.0 id $ Map.lookup (rcvr, freq, elev) efficiencies
>     rcvrId <- getRcvrId cnn rcvr
>     run cnn query (xs rcvrId tsys)
>     commit cnn
>   where
>     query = "INSERT INTO t_sys (receiver_id, frequency, elevation, total, prime) VALUES (?, ?, ?, ?, 0.0)"
>     xs rcvrId tsys = [toSql rcvrId, toSql freq, toSql elev, toSql tsys]

> putStringency cnn stringencies rcvr freq elev obstype = do
>     let str = maybe (0.0 :: Float) (\c -> fromIntegral hours / fromIntegral c) $ Map.lookup (rcvr, freq, elev, obstype) stringencies
>     rcvrId <- getRcvrId cnn rcvr
>     obsTypeId <- getObservingTypeId cnn obstype
>     run cnn query (xs rcvrId obsTypeId str)
>     commit cnn
>   where
>     query = "INSERT INTO stringency (receiver_id, observing_type_id, frequency, elevation, total) VALUES (?, ?, ?, ?, ?)"
>     xs rcvrId obsTypeId str = [toSql rcvrId, toSql obsTypeId, toSql freq, toSql elev, toSql str]
