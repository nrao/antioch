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
> --import Data.Time.LocalTime                   (utcToLocalTime)
> import Text.Printf
> import Maybe
> import Antioch.DateTime
> import Antioch.DSSData                       (getProjects, getSession)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Settings                      (proxyListenerPort)
> import Antioch.Simulate
> import Antioch.Types
> import Antioch.Utilities                     (readMinutes, rad2deg, rad2hrs)
> import Antioch.Weather                       (getWeather)

> getFactorsHandler :: Connection -> Handler()
> getFactorsHandler cnn = hMethodRouter [
>       (GET,  getFactors cnn)
>     -- , (POST, getFactors)
>     ] $ hError NotFound

> getFactors :: Connection -> StateT Context IO ()
> getFactors cnn = do
>     params <- hParameters
>     liftIO $ print params
>     -- Interpret options: id, start, tz, duration
>     let id       = read . fromJust . fromJust . lookup "id" $ params
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     -- timezone
>     let timezone = fromJust . fromJust . lookup "tz" $ params
>     -- start at ...
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     edt <- liftIO getCurrentTimeZone
>     let utc  | timezone == "ET" = localTimeToUTC edt . fromJust . parseLocalTime httpFormat $ startStr
>              | otherwise        = fromJust . parseUTCTime httpFormat $ startStr
>     let dt = toSeconds utc
>     -- duration
>     let dur = read . fromJust . fromJust . lookup "duration" $ params
>     -- get target session, and scoring sessions
>     projs <- liftIO getProjects
>     let ss = concatMap sessions projs
>     let sss = scoringSessions dt ss
>     let s = head $ filter (\s -> (sId s) == id) ss
>     w <- liftIO $ getWeather Nothing
>     rs <- liftIO $ getReceiverSchedule $ Just dt
>     factors' <- liftIO $ scoreFactors s w sss dt dur rs
>     let scores = map (\x -> [x]) . zip (repeat "score") . map Just . map eval $ factors'
>     factors <- liftIO $ scoreElements s w sss dt dur rs
>     let scoresNfactors = zipWith (++) scores factors
>     --liftIO $ print scoresNfactors
>     jsonHandler $ makeObj [("ra", showJSON . floatStr . rad2hrs . ra $ s)
>                          , ("dec", showJSON . floatStr . rad2deg . dec $ s)
>                          , ("freq", showJSON . floatStr . frequency $ s)
>                          , ("alive", showJSON . alive dt dur $ s)
>                          , ("type", showJSON . isSchedulableType dt dur $ s)
>                          , ("time", showJSON . hasTimeSchedulable dt $ s)
>                          , ("not_complete", showJSON . isNotComplete dt $ s)
>                          , ("enabled", showJSON . enabled $ s)
>                          , ("authorized", showJSON . authorized $ s)
>                          , ("observers", showJSON . hasObservers dt $ s)
>                          , ("factors", factorsListToJSValue scoresNfactors)]
>       where alive dt dur s = (hasTimeSchedulable dt s) && (isSchedulableType dt dur s)

> floatStr :: Float -> String
> floatStr f = printf "%.2f" f

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

