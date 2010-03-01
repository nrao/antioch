> module Server.PeriodFactors (
>     getPeriodFactorsHandler
>   , getPeriodFactors
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

This is a service which provides a means to investigate how a period was 
originally scheduled: ignoring period-dependent scoring factors, this
returns factors whose weighted average score should reproduce the same
score of the specified period.

> getPeriodFactorsHandler :: Connection -> Handler()
> getPeriodFactorsHandler cnn = hMethodRouter [
>       (GET,  getPeriodFactors cnn)
>     -- , (POST, getFactors)
>     ] $ hError NotFound

> getPeriodFactors :: Connection -> StateT Context IO ()
> getPeriodFactors cnn = do
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
>     let dur = read . fromJust . fromJust . lookup "duration" $ params
>     -- find out which period is being evaluated
>     projs <- liftIO getProjects
>     let sess = concatMap sessions projs
>     let ps = concatMap periods sess
>     let psCandidates = filter (\p -> (startTime p) == dt) ps
>     -- it's possible the time they gave for the start of the period
>     -- is wrong: there's no period that starts then?
>     let maybePeriod = if (length psCandidates == 0) then Nothing else Just . head $ psCandidates
>     liftIO $ print $ "got period: " ++ (show maybePeriod)
>     -- if you've found a period for the given time, use it's start time & dur;
>     -- otherwise, use the passed in start & dur.
>     let weatherDt = if isJust maybePeriod then pForecast . fromJust $ maybePeriod else dt
>     let factorsDur = if isJust maybePeriod then duration . fromJust $ maybePeriod else dur
>     -- get target session, and scoring sessions
>     let ss = scoringSessions dt sess 
>     let s = head $ filter (\s -> (sId s) == id) $ sess
>     -- make sure we set the weather origin so it can reproduce the period's
>     -- score
>     w <- liftIO $ getWeather . Just $ weatherDt
>     rs <- liftIO $ getReceiverSchedule $ Just dt
>     factors' <- liftIO $ scoreFactors s w ss dt factorsDur [] --rs
>     let scores = map (\x -> [x]) . zip (repeat "score") . map Just . map eval $ factors'
>     let rawScores = map eval factors' 
>     liftIO $ print $ "weighted mean score: " ++ (show . weightedMeanScore $ rawScores)
>     factors <- liftIO $ scoreElements s w ss dt factorsDur rs
>     let scoresNfactors = zipWith (++) scores factors
>     -- make sure we can reproduce the score
>     periodScore <- liftIO $ scoreMaybePeriod maybePeriod s ss w []
>     let scoreFromPeriod = if isJust maybePeriod then (show . pScore . fromJust $ maybePeriod) else "unknown" 
>     liftIO $ print $ "scorePeriod result: " ++ (show periodScore) ++ " vs. " ++ scoreFromPeriod 
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
>                          , ("period_score", showJSON . floatStr $ periodScore)
>                          , ("score_from_period", showJSON scoreFromPeriod)
>                          , ("factors", factorsListToJSValue scoresNfactors)]
>       where 
>     alive dt dur s = (hasTimeSchedulable dt s) && (isSchedulableType dt dur s)
>     scorePeriod' s ss w rs p = scorePeriod p s ss w rs

> scoreMaybePeriod mp s ss w rs = do
>     if isJust mp then scorePeriod (fromJust mp) s ss w rs 
>                  else return ((-1.0)::Score)

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

