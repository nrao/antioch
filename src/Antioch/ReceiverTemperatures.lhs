> module Antioch.Receiver where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Reservations
> import Antioch.Settings                (dssDataDB)
> import Antioch.DSSReversion            (putPeriodReversion)
> import Antioch.Utilities
> import Control.Monad.Trans             (liftIO)
> import Data.List                       (sort, nub, find)
> import Data.Char                       (toUpper)
> import Maybe                           (fromJust)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Data.IORef

> data ReceiverTemperatures = ReceiverTemperatures {
>     temperature :: Float -> Receiver -> IO (Maybe Float)
> }

The "unsafePerformIO hack" is a way of emulating global variables in GHC.

> {-# NOINLINE globalConnection #-}
> globalConnection :: IORef Connection
> globalConnection = unsafePerformIO $ connect >>= newIORef

> getReceiverTemperatures :: IO ReceiverTemperature
> getReceiverTemperatures = readIORef globalConnection >>= \cnn -> updateReceiverTemperatures cnn

> {-
> getRcvrTemps = do
>     cache <- newIORef M.empty
>     return $ getRcvrTemps' cache
> -}

> updateReceiverTemperatures :: Connection -> IO ReceiverTemperatures
> updateReceiverTemperatures conn = do
>     temperaturef   <- getRcvrTemps
>     (opacityf, tsysf)   <- getOpacityAndTSys'
>     stringencyf         <- getTotalStringency'
>     minOpacityf         <- getMinOpacity'
>     minTSysf            <- getMinTSysPrime'
>     return ReceiverTemperature { temperature = temperaturef conn }

> connect :: IO Connection 
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ dssDataDB ++ " user=dss"

