> module Server.RunScheduler where

> import Control.Monad.Trans                   (liftIO)
> import Data.Record.Label
> import Data.List                             (find, intercalate)
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
> import Antioch.Reports
> import Network.Protocol.Uri 
> import Network.Salvia.Handlers.Redirect      (hRedirect)
> import Maybe
> import Antioch.Settings                      (proxyListenerPort)
> import Antioch.DateTime

> scheduleAndRedirectHandler :: Handler ()
> scheduleAndRedirectHandler = hMethodRouter [
>        (POST, runScheduleAndRedirect)
>      -- POST only works because trying to get params from a GET will hang
>      -- , (GET,  runScheduleAndRedirect) 
>     ] $ hError NotFound

Get params from the URL that can then be used to run the simulator
for the given date range.

> runSchedule = do
>     bytes <- contents
>     let params = maybe [] id $ bytes >>= parseQueryParams . L.unpack
>     liftIO $ print params
>     -- schedule something! 
>     liftIO $ schedule (getKeyValuePairs params) 
>   where
>     getKeyValuePairs pairs = [(key, value) | (key, Just value) <- pairs]

Just like the name says: run the schedule, then redirect to a new page.

> runScheduleAndRedirect = do
>     runSchedule
>     -- now redirect caller back to the scheduling page
>     hRedirect getSchedulingPage 

TBF: currently, a date time string entered in a form's text box gets parsed
as (example): "2009-06-20 00%3A00%3A00".  To avoiad the issue with the time,
we are rounding off to the nearest day.

> schedule params = sim09B' start days "." 
>   where
>     start = case start' of
>                 Just dt -> dt
>                 _       -> fromGregorian 2009 6 1 0 0 0 
>     start'' = take 10 $ getParam "start" params
>     start' = fromSqlString $ start'' ++ " 00:00:00"
>     days  = read (getParam "duration" params)::Int

> getParam :: String -> [(String, String)] -> String
> getParam key params = case pair of
>              Just pair -> snd pair
>              _      -> ""
>   where
>     pair = find (\x -> ((fst x) == key)) params 

> getSchedulingPage = fromJust . parseURI $ "http://trent.gb.nrao.edu:" ++ (show proxyListenerPort) ++ "/schedule" --"/sessions/schedule"

> runSchedulerHandler :: Handler ()
> runSchedulerHandler         = hMethodRouter [
>         (POST, runScheduleAndReturn)
>       --, (GET,  runSchedule)
>     ] $ hError NotFound

Just like the name says: create a schedule, then return an OK status.

> runScheduleAndReturn = do
>     runSchedule
>     jsonHandler $ makeObj [("success", showJSON "ok")]


