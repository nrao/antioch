> module Server.JPeriods where

> import Control.Monad.Trans                   (liftIO)
> import Data.Record.Label
> import Data.List                             (intercalate)
> import Data.Maybe                            (maybeToList)
> import Database.HDBC
> import Database.HDBC.PostgreSQL              (Connection)
> import Network.Protocol.Http
> import Network.Protocol.Uri
> import Network.Salvia.Handlers.Error         (hError)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter)
> import Network.Salvia.Httpd
> import qualified Data.ByteString.Lazy.Char8 as L
> import Server.Json
> import Server.List
> -- this works!
> -- import Antioch.Reports

> periodsHandler     :: Connection -> Handler ()
> periodsHandler cnn = hMethodRouter [
>       (GET,  listPeriods cnn)
>     --, (POST, handlePost cnn)
>     ] $ hError NotFound

> data JPeriod = JPeriod {
>       jperiod_id :: Int
>     , session_id :: Maybe Int
>     , start_time :: Maybe String
>     , duration :: Maybe Double
> }

> defaultJPeriod = JPeriod {
>       jperiod_id = 0
>     , session_id = Nothing 
>     , start_time = Nothing
>     , duration = Nothing
> }

> instance JSON JPeriod where
>     readJSON = jsonToJPeriod
>     showJSON = jperiodToJson

> jsonToJPeriod _ = undefined

> jperiodToJson period = makeObj $
>       ("id", showJSON . jperiod_id $ period)
>     : concatMap field [
>           ("session_id",         showJSON' . session_id )
>         , ("start_time",      showJSON' . start_time)
>         , ("duration", showJSON' . duration)
>      ]
>   where
>     field (name, accessor) = maybeToList . fmap ((,) name) . accessor $ period

> showJSON' :: JSON a => Maybe a -> Maybe JSValue
> showJSON' = fmap showJSON

> listPeriods cnn = do
>     -- try scheduling! this actually works!
>     -- liftIO $ sim09B 4 "sims" 
>     liftIO $ print "querying periods from DB!"
>     rst <- liftIO $ quickQuery' cnn query []
>     let periods = map buildPeriod rst
>     jsonHandler $ makeObj [("periods", showJSON periods)]
>     liftIO $ print "finished listPeriods"
>   where
>     query           = "SELECT id, session_id, start, duration FROM periods"
>     buildPeriod xs = defaultJPeriod {
>         jperiod_id   = fromSql $ xs !! 0
>       , session_id   = fromSql $ xs !! 1
>       , start_time   = fromSql $ xs !! 2
>       , duration     = fromSql $ xs !! 3
>       }

