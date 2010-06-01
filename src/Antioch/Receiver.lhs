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
> import Data.List                       (sort, nub, find)
> import Data.Char                       (toUpper)
> import Maybe                           (fromJust)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
 
> connect :: IO Connection 
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ dssDataDB ++ " user=dss"

TBF: Using this function does not take advantage of the cache in 
ReceiverTemperatures.  We still need to incorporate this into the
Scoring Monda.

> getReceiverTemperature :: Session -> IO (Float)
> getReceiverTemperature s = do
>     rt <- getReceiverTemperatures
>     getReceiverTemperature' rt rcvrName freq
>   where
>     freq = frequency s
>     rcvrName = getPrimaryReceiver s

A Session can have a logical grouping of receivers - but what receiver
should we use to lookup the rcvr temperature?  Use the frequency of the
session as a key.

> getPrimaryReceiver :: Session -> Receiver
> getPrimaryReceiver s = head $ filter (rcvrInFreqRange . frequency $ s) $ concat $ receivers s

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
> rcvrRanges = [(NoiseSource, (0.0, 0.0)),
>     (Rcvr_RRI, (0.10000000000000001, 1.6000000000000001)),
>     (Rcvr_342, (0.28999999999999998, 0.39500000000000002)),
>     (Rcvr_450, (0.38500000000000001, 0.52000000000000002)),
>     (Rcvr_600, (0.51000000000000001, 0.68999999999999995)),
>     (Rcvr_800, (0.68000000000000005, 0.92000000000000004)),
>     (Rcvr_1070, (0.91000000000000003, 1.23)),
>     (Rcvr1_2, (1.1499999999999999, 1.73)),
>     (Rcvr2_3, (1.73, 2.6000000000000001)),
>     (Rcvr4_6, (3.9500000000000002, 6.0999999999999996)),
>     (Rcvr8_10, (8.0, 10.0)),
>     (Rcvr12_18, (12.0, 15.4)),
>     (Rcvr18_26, (18.0, 26.5)),
>     (Rcvr26_40, (26.0, 39.5)),
>     (Rcvr40_52, (38.200000000000003, 49.799999999999997)),
>     (Rcvr_PAR, (80.0, 100.0)),
>     (Zpectrometer, (0.0, 0.0)),
>     (Holography, (11.699999999999999, 12.199999999999999)),
>     (RcvrArray18_26, (17.0, 27.5))]

> getReceiverTemperature' :: ReceiverTemperatures -> Receiver -> Float -> IO (Float)
> getReceiverTemperature' rt rcvr freq = do
>     rcvrTemps <- temperatures rt rcvr
>     temp <- temperature rt rcvr freq rcvrTemps
>     return $ temp


