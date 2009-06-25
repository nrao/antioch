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

> runSchedulerHandler :: Handler ()
> runSchedulerHandler         = hMethodRouter [
>         (POST, runSchedule)
>     --  , (GET, runSchedule)
>     ] $ hError NotFound

> runSchedule = do
>     -- schedule something!
>     liftIO $ sim09B 4 "sims" 
>     jsonHandler $ makeObj [("success", showJSON "ok")]


