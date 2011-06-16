> module Server.MinObsConditions (
>     getMOCHandler
>   , getMOCsHandler
>   , getMOC
>   ) where

> import Antioch.DateTime
> import Antioch.DSSData                      (getSession
>                                            , getDiscretionaryPeriods)
> import Antioch.Types
> import Antioch.HardwareSchedule             (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Weather                      (getWeather)
> import Antioch.ReceiverTemperatures
> import Antioch.RunScores
> import Control.Monad.State.Lazy             (StateT)
> import Control.Monad.Trans                  (liftIO)
> import Data.Maybe                           (maybeToList)
> import Database.HDBC
> import Database.HDBC.PostgreSQL              (Connection)
> import Maybe
> import Network.Protocol.Http
> import Network.Salvia.Handlers.Error        (hError)
> import Network.Salvia.Handlers.MethodRouter (hMethodRouter)
> import Network.Salvia.Handlers.PathRouter   (hParameters)
> import Network.Salvia.Httpd
> import Server.Json

> getMOCHandler :: Connection -> Handler()
> getMOCHandler cnn = hMethodRouter [
>       (GET, getMOC cnn)
>     ] $ hError NotFound

e.g., http://trent.gb.nrao.edu:8004/moc?session_id=144&start=2011-06-08+15%3A00%3A00&duration=4

> getMOC :: Connection -> StateT Context IO ()
> getMOC cnn = do
>     -- parse params
>     params <- hParameters
>     liftIO $ print ("getMOC: ", params)
>     let start     = fromJust . fromHttpString $ getParam "start" params
>     let dur = read $ getParam "duration" params
>     let sessionId = read $ getParam "session_id" params
>     -- compute MOC
>     session <- liftIO $ getSession sessionId cnn
>     moc <- liftIO $ runMOC start dur session False
>     -- send result back
>     jsonHandler $ makeObj [("moc", showJSON . fromMaybe True $ moc)]
>   where
>     getParam p ps = fromJust . fromJust . lookup p $ ps

e.g., http://trent.gb.nrao.edu:8004/mocs?start=2011-06-08+15%3A00%3A00&duration=3

> getMOCfailures:: Connection -> StateT Context IO ()
> getMOCfailures cnn = do
>     -- parse params
>     params <- hParameters
>     liftIO $ print ("getMOCfailures: ", params)
>     let start     = fromJust . fromHttpString $ getParam "start" params
>     let days = read (getParam "duration" params)::Int
>     -- get scheduled windowed and open periods in specified range
>     ps <- liftIO $ getDiscretionaryPeriods cnn start (days*24*60)
>     -- get peIds of periods which failed their MOCs
>     peIds <- liftIO $ runMOCfailures ps False 
>     jsonHandler $ makeObj [("mocFailures", JSArray . map showJSON $ peIds)]
>   where
>     getParam p ps = fromJust . fromJust . lookup p $ ps

> computeMOC :: Connection -> [(String, Maybe String)] ->  StateT Context IO (Int, Maybe Bool)
> computeMOC cnn params = do
>     liftIO $ print ("getMOC: ", params)
>     let start     = fromJust . fromHttpString $ getParam "start" params
>     let dur = read $ getParam "duration" params
>     let sessionId = read $ getParam "session_id" params
>     let periodId = read $ getParam "period_id" params
>     -- compute MOC
>     session <- liftIO $ getSession sessionId cnn
>     moc <- liftIO $ runMOC start dur session False
>     return (periodId, moc)
>   where
>     getParam p ps = fromJust . fromJust . lookup p $ ps

> getMOCsHandler :: Connection -> Handler()
> getMOCsHandler cnn = hMethodRouter [
>       (GET, getMOCfailures cnn)
>       --(GET, getMOCs cnn)
>     ] $ hError NotFound

> -- deprecated?
> mocsListToJSValue :: [(Int, Bool)] -> JSValue
> mocsListToJSValue = JSArray . map mocPairToJson

> -- deprecated?
> mocPairToJson :: (Int, Bool) -> JSValue
> mocPairToJson pid_moc = makeObj $
>       [
>           ("pid",   showJSON  . fst $ pid_moc)
>         , ("moc",   showJSON  . snd $ pid_moc)
>       ]

> -- deprecated?
> data JMinObsCondition = JMinObsCondition {
>       moc :: Maybe Bool
> } deriving Show

> -- deprecated?
> instance JSON JMinObsCondition where
>     readJSON = jsonToJMinObsCondition
>     showJSON = jMinObsConditionToJson

> -- deprecated?
> jsonToJMinObsCondition _ = undefined

> -- deprecated?
> jMinObsConditionToJson :: JMinObsCondition -> JSValue
> jMinObsConditionToJson conditions = makeObj $
>        [("moc", showJSON . moc $ conditions)]

> -- deprecated?
> showJSON' :: JSON a => Maybe a -> Maybe JSValue
> showJSON' = fmap showJSON

> -- deprecated?
> urlToPids :: [String] -> [Int]
> urlToPids ss = map read ss
