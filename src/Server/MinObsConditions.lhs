> module Server.MinObsConditions (
>     getMOCHandler
>   , getMOC
>   ) where

> import Antioch.DateTime
> import Antioch.DSSData                      (getSession)
> import Antioch.HardwareSchedule             (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Weather                      (getWeather)
> import Antioch.ReceiverTemperatures
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

> getMOC :: Connection -> StateT Context IO ()
> getMOC cnn = do
>     params <- hParameters
>     let start     = fromJust . fromHttpString $ getParam "start" params
>     let sessionId = read $ getParam "session_id" params
>     session <- liftIO $ getSession sessionId cnn
>     w <- liftIO $ getWeather Nothing
>     rs <- liftIO $ getReceiverSchedule $ Just start
>     rt <- liftIO $ getReceiverTemperatures
>     moc <- liftIO $ runScoring w rs rt $ do
>         minimumObservingConditions start session
>     jsonHandler $ makeObj [("moc", showJSON . fromJust $ moc)]
>   where
>     getParam p ps = fromJust . fromJust . lookup p $ ps

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
