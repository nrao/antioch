> module Server.RunScheduler where

> import Control.Monad.Trans                   (liftIO)
> import Data.Record.Label
> import Data.List                             (find, intercalate)
> import Data.Maybe                            (maybeToList)
> import Data.Time                             (getCurrentTimeZone, localTimeToUTC)
> import Database.HDBC
> import Control.Monad.State.Lazy              (StateT)
> import Database.HDBC.PostgreSQL              (Connection)
> import Network.Protocol.Http
> import Network.Protocol.Uri
> import Network.Salvia.Handlers.Error         (hError)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter)
> import Network.Salvia.Httpd
> import qualified Data.ByteString.Lazy.Char8 as L
> import Server.Json
> import Server.List
> --import Antioch.Reports
> import Network.Protocol.Uri 
> import Network.Salvia.Handlers.Redirect      (hRedirect)
> import Maybe
> import Antioch.Settings                      (proxyListenerPort)
> import Antioch.DateTime
> import Antioch.RunDailySchedule

Get params from the URL that can then be used to run the simulator
for the given date range.

> runSchedule :: StateT Context IO ()
> runSchedule = do
>     -- parse params; TBF: why does this look different from others?
>     bytes <- contents
>     let params   = maybe [] id $ bytes >>= parseQueryParams . L.unpack
>     let params'  = getKeyValuePairs params
>     liftIO $ print params'
>     let tz       = getParam "tz" params'
>     let days'    = read (getParam "duration" params')::Int
>     let days     = if (days' == 0) then 0 else (days' - 1)
>     let startStr = (take 10 $ getParam "start" params') ++ " 00A00A00"
>     liftIO $ print startStr
>     edt <- liftIO getCurrentTimeZone
>     liftIO $ print edt
>     liftIO $ print (parseUTCTime httpFormat startStr)
>     let utc  | tz == "ET" = localTimeToUTC edt . fromJust . parseLocalTime httpFormat $ startStr
>              | otherwise  = fromJust . parseUTCTime httpFormat $ startStr
>     liftIO $ print utc
>     let start = toSeconds utc
>     liftIO $ print start
>
>     -- schedule something! 
>     liftIO $ print (fromSeconds start)
>     liftIO $ runDailySchedulePack start days
>   where
>     getKeyValuePairs pairs = [(key, value) | (key, Just value) <- pairs]

> getParam :: String -> [(String, String)] -> String
> getParam key params = case pair of
>              Just pair -> snd pair
>              _      -> ""
>   where
>     pair = find (\x -> ((fst x) == key)) params 

> runSchedulerHandler :: Handler ()
> runSchedulerHandler         = hMethodRouter [
>         (POST, runScheduleAndReturn)
>       --, (GET,  runSchedule)
>     ] $ hError NotFound

Just like the name says: create a schedule, then return an OK status.

> runScheduleAndReturn :: StateT Context IO ()
> runScheduleAndReturn = do
>     runSchedule
>     jsonHandler $ makeObj [("success", showJSON "ok")]


