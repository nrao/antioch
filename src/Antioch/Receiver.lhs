> module Antioch.Receiver where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Settings                (dssDataDB, databasePort)
> import Antioch.ReceiverTemperatures
> import Antioch.Utilities               (getRcvrRange, getRcvrFreqIndices)
> import Antioch.Utilities               (freq2HistoryIndex, printList)
> import Control.Monad.Trans             (liftIO)
> import Data.List                       (find, (\\))
> import Data.Char                       (toUpper)
> import Maybe                           (fromJust)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
 
> connect :: IO Connection 
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ dssDataDB ++ " port=" ++ databasePort ++ " user=dss"

Using this function does not take advantage of the cache in 
ReceiverTemperatures.  We still need to incorporate this into the
Scoring Monad by using ReceiverTemperatures, i.e., we want to
make a call to getReceiverTemperatures only once.
ReceiverTemperatures' is good for testing.

TBF: Calling getReceiverTemperatures should create a map
where the key is (Receiver, Int) where the Int is taken
from freqIndices, i.e., the result of freq2HistoryIndex.
The value is the temperature from the receiver_temperatures
table.

> getReceiverTemperature' :: Session -> IO (Maybe Float)
> getReceiverTemperature' s = do
>     rt <- getReceiverTemperatures
>     getReceiverTemperature rt rcvrName freq
>   where
>     freq = frequency s
>     rcvrName = getPrimaryReceiver s

A Session can have a logical grouping of receivers - but what receiver
should we use to lookup the rcvr temperature?  Use the frequency of the
session as a key.

TBF should we use get "nearest" receiver to given frequency instead
rather than returning Nothing?

> getPrimaryReceiver :: Session -> Maybe Receiver
> getPrimaryReceiver s
>   | length rs == 1 = Just . head $ rs
>   | otherwise      = find (rcvrInFreqRange . frequency $ s) rs
>     where
>       rs = concat . receivers $ s

Is the given frequency in the range of the given receiver?

> rcvrInFreqRange :: Frequency -> Receiver -> Bool
> rcvrInFreqRange freq rcvr = low <= freq && freq <= high
>   where
>     (low, high) = getRcvrRange rcvr

> getReceiverTemperature :: ReceiverTemperatures -> Maybe Receiver -> Frequency -> IO (Maybe Float)
> getReceiverTemperature rt rcvr freq =
>   if rcvr == Nothing then do return Nothing
>                      else do let rcvr' = fromJust rcvr
>                              rcvrTemps <- temperatures rt rcvr'
>                              temp <- temperature rt rcvr' freq rcvrTemps
>                              return $ (Just temp)
> -- substitute this function def to try and improve performance
> --getReceiverTemperature rt rcvr freq = getReceiverTemperatureCache rt rcvr freq


> getRcvrTemperature :: ReceiverTemperatures -> Session -> IO (Maybe Float)
> getRcvrTemperature rt s = do
>     getReceiverTemperature rt rcvrName freq
>   where
>     freq = frequency s
>     rcvrName = getPrimaryReceiver s

Instead of dynamically retrieving the receiver temperatures from the database,
if reconcile ourselves to the fact that the actual subset of *used* rcvr
temps is quite small, then we can use this function to retrive the needed
rcvr temperature from a static list in memory.  The purpose of doing this is
that it *might* improve performance.
The reason this subset is small is because we need to use the receiver temp
for a frequency that matches to our historical weather grid, which looks
something like this: [100, 200 ... 1000, 2000 ... 120000] (MHz).
Also see comments below.

> getReceiverTemperatureCache :: ReceiverTemperatures -> Maybe Receiver -> Frequency -> IO (Maybe Float)
> getReceiverTemperatureCache _ rcvr freq = do
>     let r = head $ dropWhile (\x -> (fst x) /= fromJust rcvr) allUsedRcvrTemps
>     return $ snd . head $ dropWhile (\x -> (fst x) /= fi) $ snd r
>   where
>     fi = freq2HistoryIndex (fromJust rcvr) freq

Call this function to print out all the receiver temperatures that will ever
get used from our database.  This is a small set due to the output from
'getRcvrFreqIndices'.  For example, for Rcvr1_2, this is simply
[1000, 2000] (MHz).

> getAllRcvrTemps :: IO [(Receiver, [(Int, Maybe Float)])]
> getAllRcvrTemps = do
>   rt <- getReceiverTemperatures
>   let rfis = map getRcvrFreqIndices rcvrs
>   let rfs = map i2f rfis
>   rts <- mapM (rtrs rt) $ zip rcvrs rfs 
>   let xs = zip rcvrs $ zipWith (\a b -> zip a b) rfis rts
>   printList xs
>   return xs
>     where
>   i2f rfi = map ((/1000) . fromIntegral) rfi
>   rtrs rt (rcvr, rfs) = mapM (getReceiverTemperature rt (Just rcvr) ) rfs 
>   rcvrs = [Rcvr_RRI .. RcvrArray18_26] \\ [Zpectrometer]

This function is where you cut and past the output from 'getAllRcvrTemps'

> allUsedRcvrTemps :: [(Receiver, [(Int, Maybe Float)])]
> allUsedRcvrTemps = [
>     (Rcvr_342,[(200,Just 17.835),(300,Just 9.934999),(400,Just 10.325)])
>   , (Rcvr_450,[(300,Just 16.635),(400,Just 11.1050005),(500,Just 13.18),(600,Just 37.4)])
>   , (Rcvr_600,[(500,Just 10.275),(600,Just 11.42),(700,Just 12.85)])
>   , (Rcvr_800,[(600,Just 32.03875),(700,Just 16.15875),(800,Just 13.3125),(900,Just 8.67875),(1000,Just 5.0437503)])
>   , (Rcvr_1070,[(900,Just 15.74),(1000,Just 7.9675),(2000,Just 30.7375)])
>   , (Rcvr1_2,[(1000,Just 22.125),(2000,Just 7.555)])
>   , (Rcvr2_3,[(1000,Just 6.6600003),(2000,Just 8.490015),(3000,Just 12.268335)])
>   , (Rcvr4_6,[(3000,Just 21.07),(4000,Just 13.6975),(5000,Just 6.455),(6000,Just 9.4),(7000,Just 7.875)])
>   , (Rcvr8_10,[(8000,Just 21.08),(9000,Just 15.67),(10000,Just 11.87)])
>   , (Rcvr12_18,[(12000,Just 21.574999),(13000,Just 12.225),(14000,Just 10.6625),(15000,Just 8.8075),(16000,Just 9.7300005)])
>   , (Rcvr18_26,[(18000,Just 40.27),(19000,Just 19.535),(20000,Just 17.315),(21000,Just 16.59),(22000,Just 23.56),(23000,Just 20.737501),(24000,Just 20.5),(25000,Just 24.505),(26000,Just 25.572498),(27000,Just 38.09667)])
>   , (Rcvr26_40,[(26000,Just 25.275002),(27000,Just 16.599998),(28000,Just 18.77),(29000,Just 12.700001),(30000,Just 11.005),(31000,Just 10.070001),(32000,Just 16.59),(33000,Just 19.385),(34000,Just 12.475),(35000,Just 25.685),(36000,Just 24.060001),(37000,Just 34.925),(38000,Just 35.12),(39000,Just 31.064999),(40000,Just 63.9)])
>   , (Rcvr40_52,[(38000,Just 31.4275),(39000,Just 31.4275),(40000,Just 31.4275),(41000,Just 28.2325),(42000,Just 26.1525),(43000,Just 26.6675),(44000,Just 25.8675),(45000,Just 27.0825),(46000,Just 28.762499),(47000,Just 33.4325),(48000,Just 36.1975),(49000,Just 37.775),(50000,Just 37.775)])
>   , (Rcvr_PAR,[(80000,Just 120.0),(82000,Just 120.0),(84000,Just 120.0),(86000,Just 120.0),(88000,Just 120.0),(90000,Just 120.0),(92000,Just 120.0),(94000,Just 120.0),(96000,Just 120.0),(98000,Just 120.0),(100000,Just 120.0)])
>   , (Holography,[(11000,Just 1000000.0),(12000,Just 1000000.0),(13000,Just 1000000.0)])
>  ,  (RcvrArray18_26,[(17000,Just 31.277845),(18000,Just 31.277845),(19000,Just 28.258923),(20000,Just 24.28654),(21000,Just 22.234539),(22000,Just 22.957077),(23000,Just 23.709616),(24000,Just 27.916155),(25000,Just 30.750538),(26000,Just 35.654232),(27000,Just 35.654232),(28000,Just 35.654232)])
>   ]
