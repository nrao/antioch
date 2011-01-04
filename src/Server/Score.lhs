> module Server.Score (
>     scoreHandler
>   ) where
 
> --import Control.Monad      (liftM)
> import Control.Monad.Trans                   (liftIO)
> import Control.Monad.State.Lazy              (StateT)
> --import Control.Monad.RWS.Strict
> --import Data.Record.Label
> import Data.List                             (find)
> import Data.Maybe                            (catMaybes)
> --import Database.HDBC
> import Database.HDBC.PostgreSQL              (Connection)
> import Network.Protocol.Http
> --import Network.Protocol.Uri
> import Network.Salvia.Handlers.PathRouter    (hParameters)
> import Network.Salvia.Handlers.PathRouter  (hPrefixRouter) -- trying
> import Network.Salvia.Handlers.Error         (hError)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter)
> import Network.Salvia.Httpd
> --import qualified Data.ByteString.Lazy.Char8 as L
> import Server.Json
> --import Server.List
> --import Antioch.Reports
> --import Network.Protocol.Uri                  (parseQueryParams)
> import Text.Printf
> import Maybe                                 (fromJust)
> import Antioch.DateTime
> import Antioch.DSSData                       (getProjects)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Filters
> import Antioch.Types
> import Antioch.Weather                       (getWeather, Weather)
> import Antioch.ReceiverTemperatures
> import Antioch.RunScores

Example URL:
http://trent.gb.nrao.edu:9051/score/periods?pids=6957&pids=6931&pids=6939

> getPScore :: Connection -> StateT Context IO ()
> getPScore cnn = do
>     params <- hParameters
>     liftIO $ print params
>
>     -- Interpret options: pids
>     let spids = (catMaybes . map snd $ params)::[String]
>     let pids = urlToPids spids
>     liftIO $ print pids
>
>     -- compute the scores 
>     projs <- liftIO getProjects
>     retvals <- liftIO $ runScorePeriods pids projs False
>     liftIO $ print retvals

>     -- send them back
>     jsonHandler $ makeObj [("scores", scoresListToJSValue retvals)]

Example URL:
http://trent.gb.nrao.edu:8002/score/session?duration=195&start=2010-03-16+11%3A45%3A00&sid=666

> getSScore :: Connection -> StateT Context IO ()
> getSScore cnn = do
>     params <- hParameters
>     liftIO $ print params
>     -- Interpret options: id, start, duration
>     let id       = read . fromJust . fromJust . lookup "sid" $ params
>     -- start at ...
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     let utc      = fromJust . parseUTCTime httpFormat $ startStr
>     let dt = toSeconds utc
>     -- duration
>     let dur = read . fromJust . fromJust . lookup "duration" $ params
>
>     -- compute the score
>     projs <- liftIO getProjects
>     score <- liftIO $ runScoreSession id dt dur projs False
>     liftIO $ print ("getSScore score: ", score)
>     
>      -- send it back
>     jsonHandler $ makeObj [("score", showJSON score)]

> scoresListToJSValue :: [(Int, Score)] -> JSValue
> scoresListToJSValue = JSArray . map scorePairToJson

> scorePairToJson :: (Int, Score) -> JSValue
> scorePairToJson pid_score = makeObj $
>       [
>           ("pid",   showJSON  . fst $ pid_score)
>         , ("score", showJSON  . snd $ pid_score)
>       ]

> scoreHandler cnn = do
>     hPrefixRouter [
>           ("/periods",  getPScore cnn) 
>         , ("/session",  getSScore cnn) 
>       ] $ hError NotFound

> urlToPids :: [String] -> [Int]
> urlToPids ss = map read ss
