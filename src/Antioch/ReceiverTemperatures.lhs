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
>   , temperature  :: Receiver -> Float -> [(Float, Float)] -> IO (Float)
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

> getRcvrTemp = do
>     cache <- newIORef M.empty
>     return $ getRcvrTemp' cache

> updateReceiverTemperatures :: Connection -> IO ReceiverTemperatures
> updateReceiverTemperatures conn = do
>     temperaturesf   <- getRcvrTemps
>     temperaturef    <- getRcvrTemp
>     return ReceiverTemperatures { temperatures = temperaturesf conn 
>                                 , temperature  = temperaturef conn }

> connect :: IO Connection 
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ dssDataDB ++ " user=dss"

> getRcvrTemps' ::  IORef (M.Map (String) ([(Float,Float)])) -> Connection -> Receiver -> IO ([(Float, Float)])
> getRcvrTemps' cache cnn rcvr = withCache key cache $ fetchRcvrTemps cnn rcvr 
>   where 
>     key = show rcvr 

> fetchRcvrTemps ::  Connection -> Receiver -> IO [(Float, Float)]
> fetchRcvrTemps cnn rcvr = do
>     result <- quickQuery' cnn query [toSql . show $ rcvr]
>     return $ toRcvrTempList result
>   where
>     query = "SELECT rt.frequency, rt.temperature FROM receiver_temperatures as rt, receivers as r WHERE r.id = rt.receiver_id AND r.name = ?"
>     toRcvrTempList = map toRcvrTemp
>     toRcvrTemp (freq:temp:[]) = (fromSql freq, fromSql temp)

> getRcvrTemp' ::  IORef (M.Map (String, Float) (Float)) -> Connection -> Receiver -> Float -> [(Float, Float)] -> IO (Float)
> getRcvrTemp' cache cnn rcvr freq temps = withCache key cache $ findRcvrTemp rcvr freq temps
>   where 
>     key = (show rcvr, freq) 

TBF: interpolation? nearest neighbor?

> findRcvrTemp ::  Receiver -> Float -> [(Float, Float)] -> IO (Float)
> findRcvrTemp rcvr freq temps = do
>     return $ snd . last $ takeWhile (findFreq freq) temps
>   where
>     findFreq f freqTemp = f >= (fst freqTemp)

> withCache :: Ord k => k -> IORef (M.Map k a) -> IO a -> IO a
> withCache key cache action = do
>     map <- readIORef cache
>     case M.lookup key map of
>         Just val -> return val
>         Nothing  -> do
>             val <- action
>             modifyIORef cache $ M.insert key val
>             return val

