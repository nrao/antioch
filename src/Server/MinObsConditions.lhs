> module Server.MinObsConditions (
>     getMOCHandler
>   , getMOC
>   ) where

> import Antioch.DateTime
> import Antioch.DSSData                      (getSession)
> import Antioch.HardwareSchedule             (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Weather                      (getWeather)
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
> getMOCHandler cnn= hMethodRouter [
>       (GET, getMOC cnn)
>     ] $ hError NotFound

> getMOC :: Connection -> StateT Context IO ()
> getMOC cnn = do
>     liftIO $ print "starting getMOC"
>     params <- hParameters
>     liftIO $ print "got params"
>     liftIO $ print params
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     let start = fromJust . fromHttpString $ startStr
>     liftIO $ print "start string"
>     liftIO $ print startStr
>     liftIO $ print "start"
>     liftIO $ print start
>     let sessionId = read . fromJust . fromJust . lookup "session_id" $ params
>     liftIO $ print "session id"
>     liftIO $ print sessionId
>     session <- liftIO $ getSession sessionId cnn
>     liftIO $ print "session"
>     liftIO $ print session
>     w <- liftIO $ getWeather . Just $ start
>     rs <- liftIO $ getReceiverSchedule $ Just start
>     moc <- liftIO $ runScoring w rs $ do
>         minimumObservingConditions start session
>     liftIO $ print "moc"
>     liftIO $ print moc
>     jsonHandler $ makeObj [("moc", showJSON . fromJust $ moc)]
>     liftIO $ print "finished getMOC"


> data JMinObsCondition = JMinObsCondition {
>       moc :: Maybe Bool
> } deriving Show

> instance JSON JMinObsCondition where
>     readJSON = jsonToJMinObsCondition
>     showJSON = jMinObsConditionToJson

> jsonToJMinObsCondition _ = undefined

> jMinObsConditionToJson :: JMinObsCondition -> JSValue
> jMinObsConditionToJson conditions = makeObj $
>        [("moc", showJSON . moc $ conditions)]

> showJSON' :: JSON a => Maybe a -> Maybe JSValue
> showJSON' = fmap showJSON
