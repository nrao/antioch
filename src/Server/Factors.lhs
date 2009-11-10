> module Server.Factors (
>     getFactorsHandler
>   , getFactors
>   ) where
 
> import Control.Monad      (liftM)
> import Control.Monad.Trans                   (liftIO)
> import Control.Monad.State.Lazy              (StateT)
> import Control.Monad.RWS.Strict
> import Data.Record.Label
> import Data.List                             (find, intercalate, sortBy)
> import Data.Maybe                            (maybeToList)
> import Data.Time                             (getCurrentTimeZone, localTimeToUTC)
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
> import Antioch.DSSData                       (getProjects, getSession)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Settings                      (proxyListenerPort)
> import Antioch.Simulate
> import Antioch.Types
> import Antioch.Utilities                     (readMinutes)
> import Antioch.Weather                       (getWeather)

> getFactorsHandler :: Connection -> Handler()
> getFactorsHandler cnn = hMethodRouter [
>       (GET,  getFactors cnn)
>     -- , (POST, getFactors)
>     ] $ hError NotFound

> getFactors :: Connection -> StateT Context IO ()
> getFactors cnn = do
>     liftIO $ print "starting getFactors"
>     params <- hParameters
>     -- Interpret options: id, start, tz, duration
>     let id       = read . fromJust . fromJust . lookup "id" $ params
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     -- timezone
>     let timezone = fromJust . fromJust . lookup "tz" $ params
>     -- start at ...
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     liftIO $ print startStr
>     edt <- liftIO getCurrentTimeZone
>     let utc  | timezone == "ET" = localTimeToUTC edt . fromJust . parseLocalTime httpFormat $ startStr
>              | otherwise        = fromJust . parseUTCTime httpFormat $ startStr
>     liftIO $ print utc
>     let dt = toSeconds utc
>     liftIO $ print dt
>     let dur = read . fromJust . fromJust . lookup "duration" $ params
>     -- get target session, and scoring sessions
>     projs <- liftIO getProjects
>     let ss = scoringSessions dt . concatMap sessions $ projs
>     s <- liftIO $ getSession id cnn 
>
>     w <- liftIO $ getWeather . Just $ dt 
>     rs <- liftIO $ getReceiverSchedule $ Just dt
>     factors <- liftIO $ scoreFactors s ss dt dur rs
>     liftIO $ print (head factors)
>     liftIO $ print (length factors)
>     jsonHandler $ makeObj [("factors", factorsListToJSValue factors)]
>     liftIO $ print "finished getFactors"

> data JFactor = JFactor {
>       fName     :: String
>     , fScore    :: Maybe Score
> } deriving Show

> defaultJFactor = JFactor {
>       fName     =  ""
>     , fScore    =  Nothing
> }

> toJFactor :: Factor -> JFactor
> toJFactor f = defaultJFactor {
>                   fName = fst f
>                 , fScore = snd f
>                              }

> instance JSON JFactor where
>     readJSON = jsonToJFactor
>     showJSON = jFactorToJson

> jsonToJFactor _ = undefined

> factorsToJSValue :: [Factor] -> JSValue
> factorsToJSValue = JSArray . map showJSON

> factorsListToJSValue :: [[Factor]] -> JSValue
> factorsListToJSValue = JSArray . map factorsToJSValue

> jFactorToJson :: JFactor -> JSValue
> jFactorToJson jfactor = makeObj $
>       [
>           ("name",  showJSON  . fName $ jfactor)
>         , ("score", showJSON  . fScore $ jfactor)
>       ]

