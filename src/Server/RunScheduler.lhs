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
> import Antioch.DailySchedule

> scheduleAndRedirectHandler :: Handler ()
> scheduleAndRedirectHandler = hMethodRouter [
>        (POST, runScheduleAndRedirect)
>      -- POST only works because trying to get params from a GET will hang
>      -- , (GET,  runScheduleAndRedirect) 
>     ] $ hError NotFound

Get params from the URL that can then be used to run the simulator
for the given date range.

> runSchedule :: StateT Context IO ()
> runSchedule = do
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
>     -- schedule something! 
>     liftIO $ print (fromSeconds start)
>     liftIO $ dailySchedulePack start days
>   where
>     getKeyValuePairs pairs = [(key, value) | (key, Just value) <- pairs]

Just like the name says: run the schedule, then redirect to a new page.

> runScheduleAndRedirect :: StateT Context IO ()
> runScheduleAndRedirect = do
>     runSchedule
>     -- now redirect caller back to the scheduling page
>     hRedirect getSchedulingPage 

> {-

TBF: currently, a date time string entered in a form's text box gets parsed
as (example): "2009-06-20 00%3A00%3A00".  To avoid the issue with the time,
we are rounding off to the nearest day.

> schedule :: [(String, String)] -> IO ()
> schedule params = dailySchedulePack start days
>   where
>     tz    = getParam "tz" params
>     time  = if tz == "UTC" then "00:00:00" else "05:00:00" -- TBF: ET??? 
>     start = case start' of
>                 Just dt -> dt
>                 _       -> fromGregorian 2009 6 1 0 0 0 
>     start'' = take 10 $ getParam "start" params
>     start' = fromSqlString $ start'' ++ " " ++ time
>     days'  = read (getParam "duration" params)::Int
>     days   = if (days' == 0) then 0 else (days' - 1)
> -}

> getParam :: String -> [(String, String)] -> String
> getParam key params = case pair of
>              Just pair -> snd pair
>              _      -> ""
>   where
>     pair = find (\x -> ((fst x) == key)) params 

> getSchedulingPage ::URI
> getSchedulingPage = fromJust . parseURI $ "http://trent.gb.nrao.edu:" ++ (show proxyListenerPort) ++ "/schedule" --"/sessions/schedule"

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


