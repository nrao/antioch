> module Server.Scores (
>     getScoresHandler
>   , getScores
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
> import Network.Salvia.Handlers.Redirect      (hRedirect)
> import Text.Printf
> import Maybe
> import Antioch.DateTime
> import Antioch.DSSData                       (getProjects, getSessionFromPeriod)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Settings                      (proxyListenerPort)
> import Antioch.Simulate
> import Antioch.Types
> import Antioch.Utilities                     (readMinutes)
> import Antioch.Weather                       (getWeather, Weather)

> getScoresHandler :: Connection -> Handler()
> getScoresHandler cnn = hMethodRouter [
>       (GET,  getScores cnn)
>     -- , (POST, getScores)
>     ] $ hError NotFound

> getScores :: Connection -> StateT Context IO ()
> getScores cnn = do
>     liftIO $ print "starting getScores"
>     params <- hParameters
>     liftIO $ print "got params"
>     liftIO $ print params
>
>     -- Interpret options: id, start, duration
>     let id       = read . fromJust . fromJust . lookup "id" $ params
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     let dt       = fromJust . fromHttpString $ startStr
>     let duration = read . fromJust . fromJust . lookup "duration" $ params
>     let second = 15 `addMinutes` dt
>     let last =  addMinutes (-15) . addMinutes duration $ dt
>     let dts      = [dt, addMinutes 15 dt .. addMinutes (-15) . addMinutes duration $ dt]
>
>     -- get target session, and scoring sessions
>     projs <- liftIO getProjects
>     let ss = scoringSessions dt . concatMap sessions $ projs
>     let s = getSessionFromPeriod id cnn 
>
>     w <- liftIO $ getWeather Nothing
>     rs <- liftIO $ getReceiverSchedule $ Just dt
>     scores <- liftIO $ mapM (scoreAt w rs ss s) dts
>     liftIO $ print scores
>     let pscores = toPScores id startStr scores
>     jsonHandler $ makeObj [("pscores", showJSON pscores)]
>     liftIO $ print "finished getScores"
>   where
>     toPScores pid strt scs = defaultJScores {
>         sPeriodId   = Just pid
>       , sStart      = Just strt
>       , sScores     = Just scs
>     }

> scoreAt :: Weather -> ReceiverSchedule -> [Session] -> IO Session -> DateTime -> IO Score
> scoreAt w rs ss s dt = do
>     s' <- liftIO s
>     fs <- runScoring w rs $ genScore ss >>= \f -> f dt s'
>     return . eval  $ fs

> data JScores = JScores {
>     sPeriodId  :: Maybe Int
>   , sStart     :: Maybe String
>   , sScores    :: Maybe [Float]
> } deriving Show

> defaultJScores = JScores {
>     sPeriodId = Nothing
>   , sStart    = Nothing
>   , sScores   = Nothing
> }

> instance JSON JScores where
>     readJSON = jsonToJScores
>     showJSON = jScoresToJson

> jsonToJScores _ = undefined

> jScoresToJson :: JScores -> JSValue
> jScoresToJson scores = makeObj $
>     concatMap field [
>         ("projId",     showJSON' . sPeriodId)
>       , ("start",      showJSON' . sStart)
>       , ("scores",     showJSON' . sScores)
>     ]
>   where
>     field (name, accessor) = maybeToList . fmap ((,) name) . accessor $ scores

> showJSON' :: JSON a => Maybe a -> Maybe JSValue
> showJSON' = fmap showJSON
