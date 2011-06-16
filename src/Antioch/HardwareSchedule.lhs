Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

> module Antioch.HardwareSchedule where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Settings (hardwareScheduleDB, dssHost, databasePort)
> import Maybe (fromJust)
> import Data.List (groupBy)
> import Database.HDBC
> import Database.HDBC.PostgreSQL

> connect :: IO Connection
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "host=" ++ dssHost ++ " dbname=" ++ hardwareScheduleDB ++ " port=" ++ databasePort ++ " user=dss" 

Get the DB connection, and use it to fetch the dates, from the DB, then 
convert to a ReceiverSchedule.

> getReceiverSchedule :: Maybe DateTime -> IO ReceiverSchedule
> getReceiverSchedule dt = do
>     cnn <- connect
>     dates <- fetchReceiverDates cnn 
>     return $ receiverDates2Schedule dt dates

> receiverDates2Schedule :: Maybe DateTime -> [(DateTime, Receiver)] -> ReceiverSchedule
> receiverDates2Schedule dt = trimSchedule dt . map collapse . groupBy (\x y -> fst x == fst y) 
>   where
>     collapse rcvrGroup = (fst . head $ rcvrGroup, map (\(_, rcvr) -> rcvr) rcvrGroup) 

> trimSchedule dt schd = case dt of 
>                          Nothing -> schd
>                          Just dt -> let (first, second) = trimSchedule' dt schd in (last' first) ++ second
>   where 
>     last' xs = case xs of
>                  [] -> []
>                  xs -> [last xs]

> trimSchedule' dt = span (\(x, _) -> x < dt)

> fetchReceiverDates :: Connection -> IO [(DateTime, Receiver)]
> fetchReceiverDates cnn = handleSqlError $ do
>   result <- quickQuery' cnn query []
>   return $ toRcvrDates result
>     where
>       query = "SELECT name, start_date FROM receiver_schedule, receivers WHERE receiver_schedule.receiver_id = receivers.id ORDER BY start_date"
>       toRcvrDates = map toRcvrElement 
>       toRcvrElement (rcvr:start:[]) = (sqlToDateTime start, read . fromSql $ rcvr)

> sqlToDateTime :: SqlValue -> DateTime
> sqlToDateTime dt = fromJust . fromSqlString . fromSql $ dt


