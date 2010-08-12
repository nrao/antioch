> module Antioch.Receiver where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Settings                (dssDataDB)
> import Antioch.ReceiverTemperatures
> import Antioch.Utilities               (getRcvrRange)
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

> getRcvrTemperature :: ReceiverTemperatures -> Session -> IO (Maybe Float)
> getRcvrTemperature rt s = do
>     getReceiverTemperature rt rcvrName freq
>   where
>     freq = frequency s
>     rcvrName = getPrimaryReceiver s
