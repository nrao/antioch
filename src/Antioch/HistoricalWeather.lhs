> module Antioch.HistoricalWeather where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Antioch.Weather
> import Antioch.Settings                (weatherDB)
> import Data.Maybe (maybeToList)
> import Database.HDBC
> import Database.HDBC.PostgreSQL

From Dana Balser:

Calculate the minimum value of the effective system temperature.
First calculate the total system temperature (DSPN5.2 equation 7;
the constant value is from spillover (3 K) and the comsic microwave
background (2.7 K), so we should use 5.7 K instead of 6 K.  I think
we are already doing this in the code.  Anyow, this will be updated
in DSPN5.3).
Calculate Stringency.  Loop over the receivers, observing type,
frequency, and elevation.  For observing type we just have continuum
and line.  For frequency, we should use the frequencies from the
weather forecast database that reside within the receiver boundaries
as specified by the reciever calibration data.  For elevation, we
should use 5,6,...,89,90.  For each hour, over n full years we
calculate the tracking error limit, the observing efficiency limit,
and the stability limit and determine the number of times the
condition below is met.  The stringency is just the total number of
hours divided by the number of hours the condition is met.


for jrx in range(len(rx)):                           # loop over receiver
    for jobs in range(len(obs)):                     # loop over observing type (cont=0/line=1)
        for jfreq in range(rx[jrx]):                 # loop over frequency (within receiver)
            for jelev in range(len(elev)):           # loop over elevation
                for iweather in range(len(weather)): # loop over all weather hours
                    
                    effective system temperature
                    tsysEffective[jrx,jfreq,jelev,iweather] = tsysTotal[jrx,jfreq,jelev,iweather]*exp(opacity[jrx,jfreq,jelev,iweather])

                     stringency
                    if jobs == 0:  # continuum
                        if trackingErrorLimit >= 1 and obsEffLimit >= self.eta_min and atmStabLimit >= 1:
                            istring[jrx,jobs,jfreq,jelev] += 1
                    else:          # line
                        if trackingErrorLimit >= 1 and obsEffLimit >= self.eta_min:
                            istring[jrx,jobs,jfreq,jelev] += 1

minTsysEffective[jrx,jfreq,jelev] = min(tsysEffective[jrx,jfreq,jelev,])
stringencyTotal[jrx,jobs,jfreq,jelev] = float(len(tsysPrime))/float(istring[jrx,jobs,jfreq,jelev])


Here's our code:

> updateHistoricalWeather :: IO ()
> updateHistoricalWeather = do
>   cnn <- connectDB
>   -- TBF: first init the DB
>   -- Then the min. effective system temperature
>   mapM (updateMinEffSysTemp cnn) getMinEffSysTempArgs 
>   -- Then the stringency
>   return ()

> --getMinEffSysTemp' :: (Receiver, Int, Int) -> IO Float
> --getMinEffSysTemp' (r, f, e) = getMinEffSysTemp r f e

We need to not just iterate through all elevations, but, more complicated,
iterate through the frequency range of each receiver, for every receiver.
However, we don't need to go below 2 GHz, since we don't have forecast
values below 2 GHz, so leave those receivers out.

> getMinEffSysTempArgs :: [(Receiver, Int, Int)]
> getMinEffSysTempArgs = [(r, f, e) | (r, f) <- rcvrFreqs, e <- elevations]
>   where
>     elevations = [5 .. 90]
>     rcvrFreqs = concatMap getRcvrFreqs [Rcvr1_2 .. RcvrArray18_26]

> getRcvrFreqs :: Receiver -> [(Receiver, Int)]
> getRcvrFreqs rcvr = [(rcvr, freq) | freq <- [(round low) .. (round hi)]]
>   where
>     (low, hi) = getRcvrRange rcvr

Returns an array of every hour that we have weather for

> getWeatherDates :: [DateTime]
> getWeatherDates = map (addHours start) [0 .. hours]
>   where
>     start = fromGregorian 2006 6 1 0 0 0 -- TBF
>     end   = fromGregorian 2006 6 2 0 0 0 -- TBF
>     hours = (diffMinutes' end start) `div` 60
>     addHours dt hrs = (hrs * 60) `addMinutes'` dt

> updateMinEffSysTemp :: Connection -> (Receiver, Int, Int) -> IO ()
> updateMinEffSysTemp cnn (rcvr, freq, elev) = do
>   print $ (show rcvr) ++ " " ++ (show freq) ++ " " ++ (show elev)
>   minEffSysTemp <- getMinEffSysTemp rcvr freq elev
>   -- TBF: uncomment this when you release
>   --putMinEffSysTemp cnn rcvr freq elev minEffSysTemp
>   print $ minEffSysTemp 

> getMinEffSysTemp :: Receiver -> Int -> Int ->  IO Float
> getMinEffSysTemp rcvr freq elev = do 
>     sysTemps <- calculateEffSysTemps rcvr freq elev 
>     return $ minimum $ concatMap maybeToList sysTemps

> calculateEffSysTemps :: Receiver -> Int -> Int -> IO [(Maybe Float)]
> calculateEffSysTemps rcvr freq elev = mapM (tSysPrimeNow rcvr freq elev)  getWeatherDates
>  -- where 
>  --   f = fromIntegral freq
>  --   e = fromIntegral elev

> tSysPrimeNow :: Receiver -> Int -> Int -> DateTime -> IO (Maybe Float)
> tSysPrimeNow rcvr freq elev dt = do
>   w <- getWeather $ Just dt
>   rt <- getReceiverTemperatures
>   tsys' <- runScoring w [] rt $ tSysPrime rcvr f e dt
>   return tsys'
>     where
>       f = fromIntegral freq
>       e = fromIntegral elev

> getStringency :: Receiver -> Int -> Int -> Bool -> IO Float
> getStringency rcvr freq elev cont = do
>     limits <- calculateStringencyLimits rcvr freq elev cont
>     return $ (fromIntegral . length $ limits) / (sum $ concatMap maybeToList limits)

> calculateStringencyLimits :: Receiver -> Int -> Int -> Bool -> IO [(Maybe Float)]
> calculateStringencyLimits rcvr freq elev cont = mapM (calcStringencyLimit rcvr freq elev cont) getWeatherDates

> calcStringencyLimit ::  Receiver -> Int -> Int -> Bool -> DateTime -> IO (Maybe Float)
> calcStringencyLimit rcvr freq elev cont dt = do
>   w <- getWeather $ Just dt
>   rt <- getReceiverTemperatures
>   strg <- runScoring w [] rt $ stringencyLimit rcvr f e cont dt
>   return strg
>     where
>       f = fromIntegral freq
>       e = fromIntegral elev

A combination of different limiting scoring factors (tracking error limit,
observing efficiency limit, atmospheric stability limit, depending on observing
type). Note that in order to reuse code from Score.lhs, we create a dummy
session to score.
TBF: should I put this in Score.lhs?

> stringencyLimit :: Receiver -> Float -> Float -> Bool -> DateTime -> Scoring (Maybe Float)
> stringencyLimit rcvr freq elev cont dt = do
>   -- observing efficiency limit
>   fs <- observingEfficiencyLimit dt s
>   let obsEffLimit = eval fs
>   let obsEffOK = obsEffLimit >= eta_min
>   -- tracking error limit
>   fs2 <- trackingErrorLimit dt s
>   let trErrLimit = eval fs2
>   let trErrOK = trErrLimit >= 1
>   -- atmospheric statbility
>   fs3 <- atmosphericStabilityLimit dt s
>   let atmStbLimit = eval fs3
>   let atmStbOK = atmStbLimit >= 1
>   return $ if obsEffOK && trErrOK && atmStbOK then (Just 1.0) else (Just 0.0)
>     where
>       s = mkDummySession rcvr freq elev cont dt
>       eta_min = 0.2 -- TBF???

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

TBF: this sucks, what constraint do I need to use to get this?

> getRaDec :: Float -> DateTime -> (Float, Float)
> getRaDec elev dt = (0.0, 1.5)

TBF: refactor to share code from Weather and DSSData

> connectDB :: IO Connection
> connectDB = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ weatherDB ++ " user=dss"

TBF: change schema to handle rcvrs

> putMinEffSysTemp :: Connection -> Receiver -> Int -> Int -> Float -> IO ()
> putMinEffSysTemp cnn rcvr freq elev tsys = do
>   result <- quickQuery' cnn query xs
>   commit cnn
>   return ()
>     where
>       query = "INSERT INTO tsys (frequency, elevation, total) VALUES (?, ?, ?)"
>       xs = [toSql freq, toSql elev, toSql tsys]
