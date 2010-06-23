> module Antioch.HistoricalWeather where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Receiver
> import Antioch.ReceiverTemperatures
> import Antioch.Weather
> import Antioch.Utilities
> import Antioch.Settings                (weatherDB)
> import Data.Maybe (maybeToList)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import System.CPUTime
> import Control.Monad.RWS.Strict

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

TBF: just running updateMinEffSysTemp, it's clear that we should optimize this.
For example, just covering a year's worth of forecasts, to run getMinEffSysTemp
(that is for a given receiver, frequency and elevation) takes ~5 minutes.
That means it might take 20 days to run (for just one year's worth!).
It seems that one flaw is how weather and recevier temperatures are not
shared across calls to tSysPrimeNow.  

> updateHistoricalWeather :: IO ()
> updateHistoricalWeather = do
>   cnn <- connectDB
>   -- initialize the caches for these DB connections here
>   rts <- getReceiverTemperatures
>   w <- getWeather Nothing
>   -- First init the DB
>   truncateTable cnn "t_sys"
>   --truncateTable cnn "stringency"
>   -- Then the min. effective system temperature
>   mapM (updateMinEffSysTemp cnn w rts) getMinEffSysTempArgs 
>   -- TBF: Then the stringency
>   return ()

We need to not just iterate through all elevations, but, more complicated,
iterate through the frequency range of each receiver, for every receiver.
However, we don't need to go below 2 GHz, since we don't have forecast
values below 2 GHz, so leave those receivers out.

> getMinEffSysTempArgs :: [(Receiver, Int, Int)]
> getMinEffSysTempArgs = [(r, f, e) | (r, f) <- rcvrFreqs, e <- elevations]
>   where
>     elevations = [5 .. 90]
>     -- for min eff sys temp, we dont want the PF rcvrs, and we don't want
>     -- to go below 2 GHz
>     -- TBF: until the new weather DB is ready, don't go above 50 either
>     rcvrFreqs = concatMap (getRcvrFreqs (\f -> f >= 2)) [Rcvr1_2 .. Rcvr40_52] --RcvrArray18_26]

Use the receivers frequency range to create an array that looks like:
[(rcvr, low), (rcvr, low+1) .. (rcvr, high)].  Note one can specify a 
gaurd as well to keep out unwanted frequencies.

> getRcvrFreqs :: (Int -> Bool) -> Receiver -> [(Receiver, Int)]
> getRcvrFreqs g rcvr = [(rcvr, freq) | freq <- [(round low) .. (round hi)], g freq]
>   where
>     (low, hi) = getRcvrRange rcvr

Returns an array of every hour that we have weather for
TBF: eventually this should go from about May 2004 to the present (6 years!)

> getWeatherDates :: [DateTime]
> getWeatherDates = map (addHours start) [0 .. hours]
>   where
>     start = fromGregorian 2006 2 1 0 0 0 -- TBF
>     end   = fromGregorian 2006 12 30 0 0 0 -- TBF
>     hours = (diffMinutes' end start) `div` 60
>     addHours dt hrs = (hrs * 60) `addMinutes'` dt

---------------Min. Effective System Temperature---------------

Gets the Minimum Effective System Temperature, and writes it to the DB.

> updateMinEffSysTemp :: Connection -> Weather -> ReceiverTemperatures -> (Receiver, Int, Int) -> IO ()
> updateMinEffSysTemp cnn w rts (rcvr, freq, elev) = do
>   print $ (show rcvr) ++ " " ++ (show freq) ++ " " ++ (show elev)
>   --begin <- getCurrentTime
>   minEffSysTemp <- getMinEffSysTemp w rts rcvr freq elev
>   --print $ minEffSysTemp 
>   --end <- getCurrentTime
>   --let execTime = end - begin
>   --print $ "mins to calculate: " ++ (show ((fromIntegral execTime) / 60.0))
>   -- TBF: uncomment this when you release
>   putMinEffSysTemp cnn rcvr freq elev minEffSysTemp
>   return ()

Gets the Minimum Effective System Temperature

> getMinEffSysTemp :: Weather -> ReceiverTemperatures -> Receiver -> Int -> Int ->  IO Float
> getMinEffSysTemp w rts rcvr freq elev = do 
>     sysTemps <- calculateEffSysTemps w rts rcvr freq elev 
>     return $ minimum $ concatMap maybeToList sysTemps

> calculateEffSysTemps :: Weather -> ReceiverTemperatures -> Receiver -> Int -> Int -> IO [(Maybe Float)]
> calculateEffSysTemps w rts rcvr freq elev = mapM (tSysPrimeNow w rts rcvr freq elev)  getWeatherDates

> tSysPrimeNow :: Weather -> ReceiverTemperatures -> Receiver -> Int -> Int -> DateTime -> IO (Maybe Float)
> tSysPrimeNow w rts rcvr freq elev dt = do
>   tsys' <- runScoring w [] rts $ tSysPrimeNow' rcvr f e dt
>   return tsys'
>     where
>       f = fromIntegral freq
>       e = fromIntegral elev

We originally were able to use Score.sSysPrime directly, but for maximizing
performance, we wanted to use the 'bestTsys/Opacity' methods below.  These
methods allow us to maximize use of the cache and minimize our hits to the DB.
Otherwise, it could take weeks to fill the t_sys table.

> tSysPrimeNow' :: Receiver -> Float -> Float -> DateTime -> Scoring (Maybe Float)
> tSysPrimeNow' rcvr freq elev dt = do
>   rt <- receiverTemperatures
>   trx' <- liftIO $ getReceiverTemperature rt (Just rcvr) freq
>   -- Simply use this line to get results that agree with previous values
>   --let trx' = Just $ oldReceiverTemperature dt defaultSession {frequency = freq}
>   -- here we are using the 'best' methods to avoid having to deal
>   -- with specifiying the forecast type, which 
>   w   <- weather
>   tk' <- liftIO $ bestTsys w dt freq
>   zod' <- liftIO $ bestOpacity w dt freq
>   let za = pi/2 - (deg2rad elev) 
>   return $ do 
>       tk <- tk'
>       zod <- zod'
>       trx <- trx'
>       -- Call the tSysPrime' method used in Score.lhs
>       return $ tSysPrime' trx tk zod za

--------------Stringency--------------------

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

> truncateTable :: Connection -> String -> IO ()
> truncateTable cnn table = do
>   result <- quickQuery' cnn query []
>   commit cnn
>   return ()
>     where
>       query = "TRUNCATE TABLE " ++ table

TBF: change schema to handle rcvrs

> putMinEffSysTemp :: Connection -> Receiver -> Int -> Int -> Float -> IO ()
> putMinEffSysTemp cnn rcvr freq elev tsys = do
>   rcvrId <- getRcvrId cnn rcvr
>   result <- quickQuery' cnn query (xs rcvrId)
>   commit cnn
>   return ()
>     where
>       query = "INSERT INTO t_sys (receiver_id, frequency, elevation, total, prime) VALUES (?, ?, ?, ?, 0.0)"
>       xs rcvrId = [toSql rcvrId, toSql freq, toSql elev, toSql tsys]

TBF: this stolen from DSSData.lhs

> getRcvrId :: Connection -> Receiver -> IO Int
> getRcvrId cnn rcvr = do
>     result <- quickQuery' cnn query xs
>     return $ fromSql . head . head $ result 
>   where
>     query = "SELECT id FROM receivers WHERE name = ?;"
>     xs = [toSql . show $ rcvr]
