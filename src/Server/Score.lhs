> module Server.Score (
>     setScoreHandler
>   , setScore
>   ) where
 
> import Control.Monad      (liftM)
> import Control.Monad.Trans                   (liftIO)
> import Control.Monad.State.Lazy              (StateT)
> import Control.Monad.RWS.Strict
> import Data.Record.Label
> import Data.List                             (find, intercalate, sortBy)
> import Data.Maybe                            (maybeToList)
> import Database.HDBC
> import Database.HDBC.PostgreSQL              (Connection)
> import Network.Protocol.Http
> import Network.Protocol.Uri
> import Network.Salvia.Handlers.PathRouter    (hParameters)
> import Network.Salvia.Handlers.Error         (hError)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter)
> import Network.Salvia.Httpd
> import qualified Data.ByteString.Lazy.Char8 as L
> import Server.Json
> import Server.List
> import Antioch.Reports
> import Network.Protocol.Uri 
> import Text.Printf
> import Maybe
> import Antioch.DateTime
> import Antioch.DSSData                       (getProjects, fetchPeriod, setPeriodScore)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Simulate
> import Antioch.Types
> import Antioch.Weather                       (getWeather)

> setScoreHandler :: Connection -> Handler()
> setScoreHandler cnn = hMethodRouter [
>     --  (GET,  setScore cnn)
>       (POST, setScore cnn)
>     ] $ hError NotFound

> setScore :: Connection -> StateT Context IO ()
> setScore cnn = do
>     liftIO $ print "setScore"
>     bytes <- contents
>     let params   = maybe [] id $ bytes >>= parseQueryParams . L.unpack
>     let params'  = getKeyValuePairs params
>     --liftIO $ print params'
>     let id       = read . getParam "id" $ params'
>
>     -- get target period, start time, and scoring sessions
>     projs <- liftIO getProjects
>     let sess = concatMap sessions projs
>     let ps = concatMap periods sess
>     let p = head $ filter (\p -> (peId p) == id) ps
>     let name = sName . session $ p
>     let s = head $ filter (\s -> (sName s) == name) sess 
>     let dt = startTime p
>     let ss = scoringSessions dt sess
>     w <- liftIO $ getWeather $ Just . pForecast $ p 
>     rs <- liftIO $ getReceiverSchedule $ Just dt
>     score <- liftIO $ scorePeriod p s ss w rs
>     liftIO $ setPeriodScore cnn score id
>     liftIO $ print score
>     jsonHandler $ makeObj [("success", showJSON "ok")]
>   where
>     getKeyValuePairs pairs = [(key, value) | (key, Just value) <- pairs]

> getParam :: String -> [(String, String)] -> String
> getParam key params = case pair of
>              Just pair -> snd pair
>              _      -> ""
>   where
>     pair = find (\x -> ((fst x) == key)) params 


