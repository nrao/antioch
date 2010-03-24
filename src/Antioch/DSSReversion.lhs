> module Antioch.DSSReversion where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Score
> import Antioch.Reservations
> import Antioch.Settings                (dssDataDB)
> import Antioch.Utilities
> import Control.Monad.Trans             (liftIO)
> import Data.List                       (sort, nub, find)
> import Data.Char                       (toUpper)
> import Maybe                           (fromJust)
> import Database.HDBC
> import Database.HDBC.PostgreSQL

This module is responsible for keep keeping our change tracking up to date:
Nell uses the django-reversion package for change tracking: it keeps track of
changes to objects as long as those changes happen in nell (django).  But here
in Antioch we write periods to the DB - so if we want to track that, we need
to do what the django-reversion package does down at the DB level.

In practice, this means filling out the reversion_revision and reversion_version
tables in the correct manner.

> putPeriodReversion :: Connection -> Period -> IO ()
> putPeriodReversion cnn p = do
>     -- make a new reversion_revision entry
>     -- and get it's ID
>     revisionId <- putPeriodRevision cnn
>     -- TBF: make an entry for the period accounting table, passing on it's ID
>     -- for each related period table, make an entry
>     putPeriodVersion cnn p revisionId
>     return ()

Create a new revision entry that marks a change for right now, due to 
Antioch.DSSData.putPeriod.  Then each piece of info about the new period
is associated w/ this new revision.

> putPeriodRevision :: Connection -> IO (Int)
> putPeriodRevision cnn = do 
>     now <- getCurrentTime
>     -- add the time zone to the current time string
>     let nowStr = (toSqlString now) ++ "-04"
>     print query
>     quickQuery' cnn query (xs nowStr)
>     commit cnn
>     revisionId <- getNewestID cnn "reversion_revision"
>     print $ "created revision w/ id: " ++ (show revisionId)
>     return revisionId
>   where
>     query = "INSERT INTO reversion_revision (date_created, user_id, comment) VALUES (?, NULL, ?)"
>     comment = "WHO: antioch WHERE: putPeriod"
>     xs nowStr = [toSql nowStr, toSql comment]

There are a few objects associated with the newly created revision:
   * period
   * period_accounting

Here we take the period we wrote to the DB and mimic what would have been put
in the reversion_version table if it had been created in Django:

> putPeriodVersion :: Connection -> Period -> Int -> IO ()
> putPeriodVersion cnn p revisionId = do
>     quickQuery' cnn query xs
>     commit cnn
>   where
>     query = "INSERT INTO reversion_version (revision_id, object_id, content_type_id, format, serialized_data, object_repr) VALUES (?, ?, 34, 'json', ?, ?)"
>     serialData = serializePeriod p
>     objRepr = representPeriod p
>     xs = [toSql revisionId, toSql . peId $ p, toSql serialData, toSql objRepr]

This should replicate the django.core.serialize product for a Period.

> serializePeriod :: Period -> String
> serializePeriod p =  "[{\"pk\": " ++ pk ++ ", \"model\": \"sesshuns.period\", \"fields\": {\"score\": " ++ sc ++ ", \"moc_ack\": " ++ moc ++ ", \"forecast\": \"" ++ forecast ++ "\", \"start\": \"" ++ start ++ "\", \"state\": " ++ state ++ ", \"session\": " ++ sessionId ++ ", \"duration\": " ++ dur ++ ", \"accounting\": " ++ accountingId ++ ", \"backup\": " ++ backup ++ "}}]"
>   where
>     pk = show . peId $ p
>     sc = show . pScore $ p
>     moc = "false" -- TBF
>     forecast = toSqlString . pForecast $ p
>     start = toSqlString . startTime $ p
>     state = show . stateTypeToPK . pState $ p
>     sessionId = show . sId . session $ p
>     dur = show . duration $ p
>     accountingId = "4099" -- TBF
>     backup = toSqlBool . pBackup $ p -- TBF

 serializePeriod p = "[{\"pk\": 3685, \"model\": \"sesshuns.period\", \"fields\": {\"score\": 66.0, \"moc_ack\": false, \"forecast\": \"2010-03-23 17:30:00\", \"start\": \"2010-03-23 00:00:00\", \"state\": 1, \"session\": 339, \"duration\": 1.0, \"accounting\": 4099, \"backup\": false}}]"

Need this special function because Show Bool gives "True" and "False"

> toSqlBool :: Bool -> String
> toSqlBool bool = if bool then "true" else "false"


TBF: simple mapping, but static - perhaps it should read the DB to get
these period state Primary Keys?

> stateTypeToPK :: StateType -> Int
> stateTypeToPK stateType | stateType == Pending   = 1
>                         | stateType == Scheduled = 2
>                         | stateType == Deleted   = 3
>                         | stateType == Complete  = 4

This should replicate the __str__ method for the Django Period Model:

> representPeriod :: Period -> String
> representPeriod p = "Period for Session (" ++ id ++ "): " ++ start ++ " for " ++ dur ++ " Hrs (" ++ state ++ ")"
>   where
>     id = show . peId $ p
>     start = show . toSqlString . startTime $ p
>     dur = show $ (fromIntegral . duration $ p) / 60.0
>     state = show. head . show . pState $ p -- ex: 'P', 'S', ...

Utilities

What's the largest (i.e. newest) primary key in the given table?

> getNewestID :: Connection -> String -> IO Int
> getNewestID cnn table = do
>     r <- quickQuery' cnn query xs
>     return $ toId r
>   where
>     xs = [] 
>     query = "SELECT MAX(id) FROM " ++ table
>     toId [[x]] = fromSql x
