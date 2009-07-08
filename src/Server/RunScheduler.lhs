> module Server.RunScheduler where

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
> import Antioch.Reports
> import Network.Protocol.Uri 
> import Network.Salvia.Handlers.Redirect      (hRedirect)
> import Maybe
> import Antioch.Settings                      (proxyListenerPort)

> scheduleAndRedirectHandler :: Handler ()
> scheduleAndRedirectHandler = hMethodRouter [
>         (POST, runScheduleAndRedirect)
>       , (GET,  runScheduleAndRedirect) -- currently only POST is used
>     ] $ hError NotFound

> runScheduleAndRedirect = do
>     -- schedule something! TBF: need to pass in start & dur
>     liftIO $ sim09B 3 "sims" 
>     -- now redirect caller back to the scheduling page
>     hRedirect getSchedulingPage 

> getSchedulingPage = fromJust . parseURI $ "http://trent.gb.nrao.edu:" ++ (show proxyListenerPort) ++ "/schedule"

This is just example code - no one currently uses this code.

> runSchedulerHandler :: Handler ()
> runSchedulerHandler         = hMethodRouter [
>         (POST, runSchedule)
>       , (GET,  runSchedule)
>     ] $ hError NotFound

> runSchedule = do
>     -- schedule something!
>     liftIO $ sim09B 4 "sims" 
>     jsonHandler $ makeObj [("success", showJSON "ok")]


