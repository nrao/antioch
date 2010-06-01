> {-# OPTIONS -XMultiParamTypeClasses -XScopedTypeVariables #-}

> module Antioch.ReceiverTemperatures where

> import Antioch.DateTime
> import Antioch.Types
> --import Antioch.Weather
> --import Antioch.Reservations
> import Antioch.Settings                (dssDataDB)
> import Antioch.Utilities
> import Control.Monad.Trans             (liftIO)
> import Data.Convertible
> import Data.List                       (sort, nub, find)
> import Data.Char                       (toUpper)
> import Maybe                           (fromJust)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Data.IORef
> import System.IO.Unsafe  (unsafePerformIO)
> import qualified Data.Map as M

This modules only responsibility is for maintaining a cache of the each rcvr's
temperatures.

> instance Convertible Float SqlValue where
>     safeConvert x = return $ SqlDouble ((realToFrac x) :: Double)

> instance Convertible SqlValue Float where
>     safeConvert x = do
>         val :: Double <- safeConvert x
>         return $ realToFrac val

> data ReceiverTemperatures = ReceiverTemperatures {
>     temperatures :: Receiver -> IO ([(Float, Float)])
> }

The "unsafePerformIO hack" is a way of emulating global variables in GHC.

> {-# NOINLINE globalConnection #-}
> globalConnection :: IORef Connection
> globalConnection = unsafePerformIO $ connect >>= newIORef

> getReceiverTemperatures :: IO ReceiverTemperatures
> getReceiverTemperatures = readIORef globalConnection >>= \cnn -> updateReceiverTemperatures cnn

> getRcvrTemps = do
>     cache <- newIORef M.empty
>     return $ getRcvrTemps' cache

> updateReceiverTemperatures :: Connection -> IO ReceiverTemperatures
> updateReceiverTemperatures conn = do
>     temperaturesf   <- getRcvrTemps
>     return ReceiverTemperatures { temperatures = temperaturesf conn }

> connect :: IO Connection 
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ dssDataDB ++ " user=dss"

> getRcvrTemps' ::  IORef (M.Map (String) ([(Float,Float)])) -> Connection -> Receiver -> IO ([(Float, Float)])
> --getRcvrTemps' cache cnn rcvr = withCache key cache $ fetchRcvrTemps cnn rcvr 
> getRcvrTemps' cache cnn rcvr = withCache key cache $ fetchRcvrTemps cnn rcvr 
>   where 
>     key = show rcvr 

> fetchRcvrTemps ::  Connection -> Receiver -> IO [(Float, Float)]
> fetchRcvrTemps cnn rcvr = do
>     print query
>     result <- quickQuery' cnn query [toSql . show $ rcvr]
>     print . head $ result
>     return $ toRcvrTempList result
>   where
>     query = "SELECT rt.frequency, rt.temperature FROM receiver_temperatures as rt, receivers as r WHERE r.id = rt.receiver_id AND r.name = ?"
>     toRcvrTempList = map toRcvrTemp
>     toRcvrTemp (freq:temp:[]) = (fromSql freq, fromSql temp)

> withCache :: Ord k => k -> IORef (M.Map k a) -> IO a -> IO a
> withCache key cache action = do
>     map <- readIORef cache
>     case M.lookup key map of
>         Just val -> return val
>         Nothing  -> do
>             val <- action
>             modifyIORef cache $ M.insert key val
>             return val

