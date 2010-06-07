> module Antioch.Receiver where

> import Antioch.DateTime
> import Antioch.Types
> --import Antioch.Score
> --import Antioch.Reservations
> import Antioch.Settings                (dssDataDB)
> --import Antioch.DSSReversion            (putPeriodReversion)
> import Antioch.ReceiverTemperatures
> import Antioch.Utilities
> import Control.Monad.Trans             (liftIO)
> import Data.List                       (find)
> import Data.Char                       (toUpper)
> import Maybe                           (fromJust)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
 
> connect :: IO Connection 
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ dssDataDB ++ " user=dss"

Using this function does not take advantage of the cache in 
ReceiverTemperatures.  We still need to incorporate this into the
Scoring Monad by using ReceiverTemperatures, i.e., we want to
make a call to getReceiverTemperatures only once. ReceiverTemperatures'
is good for testing.

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

> getPrimaryReceiver :: Session -> Maybe Receiver
> getPrimaryReceiver s
>   | length rs == 1 = Just . head $ rs
>   | otherwise      = find (rcvrInFreqRange . frequency $ s) rs
>     where
>       rs = concat . receivers $ s

Is the given frequency in the range of the given receiver?

> rcvrInFreqRange :: Float -> Receiver -> Bool
> rcvrInFreqRange freq rcvr = low <= freq && freq <= high
>   where
>     low = fst $ getRcvrRange rcvr  
>     high = snd $ getRcvrRange rcvr  

> getRcvrRange :: Receiver -> (Float, Float)
> getRcvrRange rcvr = snd . head $ filter (\r -> (fst r) == rcvr) rcvrRanges

Here are the ranges for all receivers: this was copied from the DSS database.

> rcvrRanges :: [(Receiver, (Float, Float))]
> rcvrRanges = [
>     (NoiseSource,    ( 0.0,  0.0)),
>     (Rcvr_RRI,       ( 0.1,  1.6)),
>     (Rcvr_342,       ( 0.29, 0.395)),
>     (Rcvr_450,       ( 0.385,0.52)),
>     (Rcvr_600,       ( 0.51, 0.69)),
>     (Rcvr_800,       ( 0.68, 0.92)),
>     (Rcvr_1070,      ( 0.91, 1.23)),
>     (Rcvr1_2,        ( 1.15, 1.73)),
>     (Rcvr2_3,        ( 1.73, 2.6)),
>     (Rcvr4_6,        ( 3.95, 6.1)),
>     (Rcvr8_10,       ( 8.0, 10.0)),
>     (Rcvr12_18,      (12.0, 15.4)),
>     (Rcvr18_26,      (18.0, 26.5)),
>     (Rcvr26_40,      (26.0, 39.5)),
>     (Rcvr40_52,      (38.2, 49.8)),
>     (Rcvr_PAR,       (80.0,100.0)),
>     (Zpectrometer,   ( 0.0,  0.0)),
>     (Holography,     (11.7, 12.2)),
>     (RcvrArray18_26, (17.0, 27.5))]

> getReceiverTemperature :: ReceiverTemperatures -> Maybe Receiver -> Float -> IO (Maybe Float)
> getReceiverTemperature rt rcvr freq =
>   if rcvr == Nothing then do return Nothing
>                      else do let rcvr' = fromJust rcvr
>                              rcvrTemps <- temperatures rt rcvr'
>                              temp <- temperature rt rcvr' freq rcvrTemps
>                              return $ (Just temp)


