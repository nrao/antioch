> module Antioch.DBUtilities where

> import Antioch.Types
> import Database.HDBC
> import Database.HDBC.PostgreSQL

You've got a receiver, like Rcvr1_2, but what's it's Primary Key in the DB?

> getRcvrId :: Connection -> Receiver -> IO Int
> getRcvrId cnn rcvr = do
>     result <- quickQuery' cnn query xs
>     return $ fromSql . head . head $ result 
>   where
>     query = "SELECT id FROM receivers WHERE name = ?;"
>     xs = [toSql . show $ rcvr]
