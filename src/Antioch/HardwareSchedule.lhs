> module Antioch.HardwareSchedule where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Weather
> import Data.List (groupBy)
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Control.Monad.Trans (liftIO)

Get the DB connection, and use it to fetch the dates, from the DB, then 
convert to a ReceiverSchedule.

> getReceiverSchedule :: DateTime -> IO ReceiverSchedule
> getReceiverSchedule dt = do
>     cnn <- connect
>     dates <- fetchReceiverDates cnn dt
>     return $ receiverDates2Schedule dates

> receiverDates2Schedule :: [(DateTime, Receiver)] -> ReceiverSchedule
> receiverDates2Schedule = map collapse . groupBy (\x y -> fst x == fst y) 
>   where
>     collapse rcvrGroup = (fst . head $ rcvrGroup, map (\(dt, rcvr) -> rcvr) rcvrGroup) 

> fetchReceiverDates :: Connection -> DateTime -> IO [(DateTime, Receiver)]
> fetchReceiverDates cnn dt = handleSqlError $ do
>   result <- quickQuery' cnn query []
>   return $ toRcvrDates result
>     where
>       query = "SELECT name, start_date FROM receiver_schedule, receivers WHERE receiver_schedule.receiver_id = receivers.id ORDER BY start_date"
>       xs = [toSql' dt]
>       toRcvrDates = map toRcvrElement 
>       toRcvrElement (rcvr:start:[]) = (fromSql start, read . fromSql $ rcvr)

