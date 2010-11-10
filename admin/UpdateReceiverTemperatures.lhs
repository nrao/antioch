> module Main where

> import System                         (getArgs)
> import Control.Exception              (assert)
> import Database.HDBC
> import Database.HDBC.MySQL
> import Database.HDBC.PostgreSQL
> import Maybe
> import Data.List

Given the name of the database containing the receiver_temperatures'
table, the receiver's name, and a sql date string (YYYY-MM-DD),
this program replaces the fields for the given receiver in the table
receiver_temperatures with the values from the database *receivers*
from the given calibration date.

Example command line: 
runhaskell UpdateReceiverTemperatures <name> Rcvr1_2 2005-05-27

> main = do
>   args <- getArgs
>   let dbname  = assert (length args == 3) args !! 0
>   let rcvr    = assert (length args == 3) args !! 1
>   let calDate = assert (length args == 3) args !! 2
>
>   putStr ("Replacing receiver temperatures in " ++ dbname ++
>           " for " ++ rcvr ++
>           " from measurements taken on " ++ calDate ++ "\n")
>
>   let (rcvr', channel) = getMySqlRcvr rcvr
>
>   pcnn <- pconnect dbname
>   mcnn <- mconnect "receivers"
>   fts <- collectNewTemps mcnn rcvr' channel calDate
>   
>   removeOldTemps pcnn rcvr
>   insertNewTemps pcnn rcvr fts
>   putStr $ "Wrote " ++ (show . length $ fts) ++ " Receiver Temperature measurements to database.\n"

> getMySqlRcvr :: String -> (String, Maybe String)
> getMySqlRcvr dssRcvr | dssRcvr `elem` pf1Rcvrs = ("RcvrPF_1", Just (getChannel dssRcvr))
>                      | dssRcvr == pf2Rcvr      = ("RcvrPF_2", Just "1070")
>                      | otherwise               = (dssRcvr   , Nothing)
>   where
>     pf1Rcvrs = ["Rcvr_342", "Rcvr_450", "Rcvr_600", "Rcvr_800"]
>     pf2Rcvr  = "Rcvr_1070"

> getChannel :: String -> String
> getChannel rcvr = drop ((fromJust $ elemIndex '_' rcvr) + 1) rcvr

Reads calibration frequencies and receiver temperatures for the given
receiver which were collected on the given date and returns the means
of the temperatures across all beams and polarizations, i.e., 
[(frequency, temperature)]

Some receivers do not have calibrations, so fake a constant temperature

> collectNewTemps cnn r chn dt 
>    | r == "Rcvr_PAR"   = return $ [(80.0,120.0),(100.0,120.0)] 
>    | r == "Rcvr_RRI"   = return $ [(0.1,300.0),(1.6,300.0)] 
>    | r == "Holography" = return $ [(11.7,1e6),(12.2,1e6)] 
>    | otherwise = collectNewTemps' cnn r chn dt


> -- collectNewTemps :: Database.HDBC.MySQL.Connection.Connection -> String -> String -> IO [(Double, Double)]
> collectNewTemps' cnn r chn dt = do
>   let channel = "%" ++ (maybe "" id chn)
>   result <- quickQuery' cnn query [toSql r, toSql dt, toSql channel]
>   let temps = assert (not . null $ result) map fromSqlList result
>   return $ reverse . means . collapse $ temps
>     where
>       query = "SELECT t.frequency, t.temperature FROM tcal AS t, measurement AS m WHERE m.id = t.id AND m.type=\"t\" AND m.calibrator=\"B\" AND m.receiver = ? AND m.date = ? AND channel LIKE ? ORDER BY t.frequency;"

Given a receiver name and a list of frequency/temperature pairs, insert
the values into the *receiver_temperatures* table.

> insertNewTemps :: Connection -> String -> [(Double, Double)] -> IO ()
> insertNewTemps cnn r fts = do
>     rcvrId <- getRcvrId cnn r
>     result <- mapM (insertNewTemp cnn rcvrId) fts
>     return ()

> insertNewTemp :: Connection -> Int -> (Double, Double) -> IO ()
> insertNewTemp cnn rcvrId freqTemp = do
>     result <- quickQuery' cnn query xs 
>     commit cnn
>     return () 
>   where
>     query = "INSERT INTO receiver_temperatures VALUES (DEFAULT, ?, ?, ?)"
>     freq = fst freqTemp
>     temp = snd freqTemp
>     xs = [toSql rcvrId, toSql freq, toSql temp]

> getRcvrId :: Connection -> String -> IO (Int)
> getRcvrId cnn rname = do
>     result <- quickQuery' cnn query [toSql rname]
>     return $ fromSql . head . head $ result
>   where
>      query = "SELECT id FROM receivers WHERE name = ?"

Translate [[frequency, temperature]] where frequency may be duplicated
into [(frequency, [temperature])] where frequency is unique.

> collapse :: [[Double]] -> [(Double,[Double])]
> collapse ds = foldl collapse' [(head . head $ ds, [])] $ ds

> collapse' :: [(Double,[Double])] -> [Double] -> [(Double,[Double])]
> collapse' (m:ms) ts
>   | fst m == new_freq  = (new_freq,  new_temp:temp_list):ms
>   | otherwise          = (new_freq, [new_temp]): m         :ms
>     where
>       new_freq = head ts
>       new_temp = last ts
>       temp_list = snd m

Translate [(frequency, [temperature])] into [(frequency, mean temperature)]

> means :: [(Double,[Double])] -> [(Double,Double)]
> means fts = map mean fts
>   where
>     mean ft = (fst ft, (sum ts) / (fromIntegral . length $ ts))
>       where
>         ts = snd ft

Strip out all rows for the specified receiver in *receiver_temperatures*.

> removeOldTemps :: Connection -> String -> IO ()
> removeOldTemps cnn r = do
>   result <- quickQuery' cnn query1 [toSql r]
>   let rid = assert (not . null $ result) fromSqlInt . head . head $ result
>   quickQuery' cnn query2 [toSql rid]
>   commit cnn
>     where
>       query1 = "SELECT id FROM receivers WHERE name = ?;"
>       query2 = "DELETE FROM receiver_temperatures WHERE receiver_id = ?;"

> -- mconnect :: String -> IO Database.HDBC.MySQL.Connection.Connection
> mconnect dbname = handleSqlError $ connectMySQL defaultMySQLConnectInfo
>    {
>       mysqlHost     = "gbtdata.gbt.nrao.edu"
>     , mysqlDatabase = "receivers"
>     , mysqlUser     = "receiver_query"
>     , mysqlPassword = "braindead"
>    }

> pconnect :: String -> IO Connection
> pconnect dbname = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ dbname ++ " user=dss"

> fromSqlInt :: SqlValue -> Int
> fromSqlInt i = fromSql i

> fromSqlDouble :: SqlValue -> Double
> fromSqlDouble x = fromSql x

> fromSqlList :: [SqlValue] -> [Double]
> fromSqlList xs = map fromSqlDouble xs
