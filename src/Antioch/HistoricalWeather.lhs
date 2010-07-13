> {-# OPTIONS -XImplicitParams #-}
> module Antioch.HistoricalWeather where

> import Antioch.DateTime
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Antioch.Score
> import Antioch.Settings  (weatherDB)
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.Weather
> import Control.Monad.RWS.Strict
> import Data.Char         (toLower)
> import Data.IORef        (IORef, newIORef, readIORef)
> import Data.List 
> import Data.Maybe        (catMaybes, maybeToList)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import System.CPUTime
> import System.IO.Unsafe  (unsafePerformIO)

From Dana Balser:

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


> updateHistoricalWeather :: IO ()
> updateHistoricalWeather = do
>     cnn <- connectDB
>     truncateTable cnn "t_sys"
>     truncateTable cnn "stringency"
>     ericWasHere cnn
>     disconnect cnn

> ericWasHere     :: Connection -> IO ()
> ericWasHere cnn = do
>     rts <- getReceiverTemperatures
>     let ?rts = rts
>     w <- getWeather Nothing
>     let ?w = w
>     forM_ allRcvrs $ \rcvr -> do
>     forM_ (getRcvrFreqIndices rcvr) $ \freq -> do
>     forM_ [5 .. 90] $ \elev -> do
>       minEffSysTemp <- getMinEffSysTemp rcvr freq elev
>       putMinEffSysTemp cnn rcvr freq elev minEffSysTemp
>       forM_ [False, True] $ \cont -> do
>         str <- getStringency rcvr freq elev cont
>         putStringency cnn rcvr freq elev cont str

> allRcvrs = [Rcvr_RRI .. RcvrArray18_26] \\ [Zpectrometer]

> getRcvrFreqIndices :: Receiver -> [Int]
> getRcvrFreqIndices rcvr = takeWhile (<=hi) . dropWhile (<lo) $ freqIndices
>   where
>     (lo', hi') = getRcvrRange rcvr
>     lo = freq2HistoryIndex lo'
>     hi = freq2HistoryIndex hi'

Returns an array of every hour that we have weather for
TBF: eventually this should go from about May 2004 to the present (6 years!)

> getWeatherDates :: [DateTime]
> getWeatherDates = unsafePerformIO $ readIORef theWeatherDates

> {-# NOINLINE theWeatherDates #-}
> theWeatherDates :: IORef [DateTime]
> theWeatherDates = unsafePerformIO $ newIORef initWeatherDates

> initWeatherDates :: [DateTime]
> initWeatherDates = map (addHours start) [0 .. hours]
>   where
>     start = fromGregorian 2006 6 10 0 0 0 -- TBF
>     end   = fromGregorian 2006 6 11 0 0 0 -- TBF
>     hours = (diffMinutes' end start) `div` 60
>     addHours dt hrs = (hrs * 60) `addMinutes'` dt

---------------Min. Effective System Temperature---------------

Gets the Minimum Effective System Temperature

> getMinEffSysTemp                :: (?rts :: ReceiverTemperatures, ?w :: Weather) => Receiver -> Int -> Int -> IO Float
> getMinEffSysTemp rcvr freq elev = runScoring ?w [] ?rts $
>     liftM (minimum . catMaybes) $ mapM (tSysPrimeNow' rcvr f e) getWeatherDates
>   where
>     f = fromIntegral freq / 1000.0 -- MHz -> GHz
>     e = fromIntegral elev

We originally were able to use Score.tSysPrime directly, but for
maximizing performance, we wanted to use the 'bestTsys/Opacity' methods
below.  These methods allow us to maximize use of the cache and minimize
our hits to the DB.  Otherwise, it could take weeks to fill the t_sys
table.

> tSysPrimeNow' :: Receiver -> Frequency -> Float -> DateTime -> Scoring (Maybe Float)
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

> getStringency :: (?rts :: ReceiverTemperatures) => Receiver -> Int -> Int -> Bool -> IO Float
> getStringency rcvr freq elev cont = do
>     limits <- calculateStringencyLimits rcvr freq elev cont
>     return $ (fromIntegral . length $ limits) / (sum $ concatMap maybeToList limits)

> calculateStringencyLimits :: (?rts :: ReceiverTemperatures) => Receiver -> Int -> Int -> Bool -> IO [Maybe Float]
> calculateStringencyLimits rcvr freq elev cont = mapM (calcStringencyLimit rcvr freq elev cont) getWeatherDates

> calcStringencyLimit :: (?rts :: ReceiverTemperatures) => Receiver -> Int -> Int -> Bool -> DateTime -> IO (Maybe Float)
> calcStringencyLimit rcvr freq elev cont dt = do
>     w <- getWeather $ Just dt
>     runScoring w [] ?rts $ stringencyLimit rcvr f e cont dt
>   where
>     f = fromIntegral freq
>     e = fromIntegral elev

A combination of different limiting scoring factors (tracking error limit,
observing efficiency limit, atmospheric stability limit, depending on
observing type). Note that in order to reuse code from Score.lhs, we
create a dummy session to score.

Note: frequency passed in should be in GHz

TBF: should I put this in Score.lhs?

> stringencyLimit :: Receiver -> Float -> Float -> Bool -> DateTime -> Scoring (Maybe Float)
> stringencyLimit rcvr freq elev cont dt = do
>     -- observing efficiency limit
>     fs <- observingEfficiencyLimit dt s
>     let obsEffLimit = eval fs
>     let obsEffOK = obsEffLimit >= 1 
>     -- tracking error limit
>     fs2 <- trackingErrorLimit dt s
>     let trErrLimit = eval fs2
>     let trErrOK = trErrLimit >= 1
>     -- atmospheric statbility
>     fs3 <- atmosphericStabilityLimit dt s
>     let atmStbLimit = eval fs3
>     let atmStbOK = atmStbLimit >= 1
>     return $ if obsEffOK && trErrOK && atmStbOK then (Just 1.0) else (Just 0.0)
>   where
>     s = mkDummySession rcvr freq elev cont dt

Creates a dummy session with the given attributes.  The only tricky part
is giving the session a target that will be at the specified elevation
at the specified time.

> mkDummySession :: Receiver -> Float -> Float -> Bool -> DateTime -> Session
> mkDummySession rcvr freq elev cont dt = defaultSession { frequency = freq
>     , receivers = [[rcvr]]
>     , ra = ra'
>     , dec = dec'
>     , oType = if cont then Continuum else SpectralLine }
>   where
>     ra' = fst $ getRaDec elev dt
>     dec' = snd $ getRaDec elev dt

> getRaDec :: Float -> DateTime -> (Radians, Radians)
> getRaDec elev dt = (ra, dec)
>   where
>     ra = hrs2rad . utc2lstHours $ dt
>     dec = realToFrac $ (realToFrac . deg2rad $ elev) - (pi/2 - gbtLat)

-------------Database----------------------------

TBF: refactor to share code from Weather and DSSData

> connectDB :: IO Connection
> connectDB = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ weatherDB ++ " user=dss"

> truncateTable :: Connection -> String -> IO ()
> truncateTable cnn table = do
>     result <- quickQuery' cnn query []
>     commit cnn
>   where
>     query = "TRUNCATE TABLE " ++ table

> putMinEffSysTemp :: Connection -> Receiver -> Int -> Int -> Float -> IO ()
> putMinEffSysTemp cnn rcvr freq elev tsys = do
>     rcvrId <- getRcvrId cnn rcvr
>     result <- quickQuery' cnn query (xs rcvrId)
>     commit cnn
>   where
>     query = "INSERT INTO t_sys (receiver_id, frequency, elevation, total, prime) VALUES (?, ?, ?, ?, 0.0)"
>     xs rcvrId = [toSql rcvrId, toSql freq, toSql elev, toSql tsys]

> putStringency :: Connection -> Receiver -> Int -> Int -> Bool -> Float -> IO ()
> putStringency cnn rcvr freq elev cont str = do
>     rcvrId <- getRcvrId cnn rcvr
>     obsTypeId <- getObservingTypeId cnn (obsType cont)
>     result <- quickQuery' cnn query (xs rcvrId obsTypeId)
>     commit cnn
>   where
>     query = "INSERT INTO stringency (receiver_id, observing_type_id, frequency, elevation, total) VALUES (?, ?, ?, ?, ?)"
>     xs rcvrId obsTypeId = [toSql rcvrId, toSql obsTypeId, toSql freq, toSql elev, toSql str]
>     obsType cont = if cont then Continuum else SpectralLine
