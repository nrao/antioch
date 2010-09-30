> module Server.Nominees (
>     getNomineesHandler
>   , getNominees
>   ) where
 
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
> import Antioch.DSSData                       (getProjects)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Settings                      (proxyListenerPort)
> --import Antioch.Simulate
> import Antioch.Filters
> import Antioch.Types
> import Antioch.Utilities                     (readMinutes)
> import Antioch.Weather                       (getWeather)
> import Antioch.ReceiverTemperatures

> getNomineesHandler :: Handler()
> getNomineesHandler = hMethodRouter [
>       (GET,  getNominees)
>     --, (POST, getNominees)
>     ] $ hError NotFound

> getNominees :: StateT Context IO ()
> getNominees = do
>     params <- hParameters
>     liftIO $ print params
>
>     -- Interpret options:
>     -- timezone
>     let timezone = fromJust . fromJust . lookup "tz" $ params
>     -- start at ...
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     edt <- liftIO getCurrentTimeZone
>     let utc  | timezone == "ET" = localTimeToUTC edt . fromJust . parseLocalTime httpFormat $ startStr
>              | otherwise        = fromJust . parseUTCTime httpFormat $ startStr
>     let dt = toSeconds utc
>     -- duration of the hole (if one) ...
>     let upper = fmap readMinutes . fromJust . lookup "duration" $ params
>     -- ignore minimum duration limit?
>     let lower = maybe Nothing (\v -> if v == "true" then Just 0 else Nothing) . fromJust . lookup "minimum" $ params
>     let rfi         = fromJust . fromJust . lookup "rfi" $ params
>     let timeBetween = fromJust . fromJust . lookup "timeBetween" $ params
>     let blackout    = fromJust . fromJust . lookup "blackout" $ params
>     let sfs = catMaybes [if rfi == "true" then Nothing else Just needsLowRFI
>                        , if timeBetween == "true" then Nothing else Just enoughTimeBetween
>                        , if blackout == "true" then Nothing else Just observerAvailable
>                         ]
>     -- use only backup sessions?
>     let backup = fromJust . fromJust . lookup "backup" $ params
>     -- include completed sessions?
>     let completed = fromJust . fromJust . lookup "completed" $ params
>     let filter = catMaybes . concat $ [
>             if completed == "true" then [Nothing] else [Just hasTimeSchedulable, Just isNotComplete]
>           , [Just isApproved]
>           , [Just hasObservers]
>           , if backup == "true" then [Just isBackup] else [Nothing]
>                                       ]
>     let schedSessions = filterSessions dt undefined filter
>
>     -- This is kind of awkward, the various selections the user may
>     -- specify must be implemented by sessions selection, scoring
>     -- factors, and/or period selection.
>     projs <- liftIO getProjects
>     let ss = concatMap sessions projs
>     w <- liftIO $ getWeather Nothing
>     rt <- liftIO $ getReceiverTemperatures
>     rs <- liftIO $ getReceiverSchedule $ Just dt
>     nominees <- liftIO $ runScoring w rs rt $ do
>         sf <- genPartScore dt sfs . scoringSessions dt undefined $ ss
>         genNominees sf dt lower upper . schedSessions $ ss
>     liftIO $ print nominees
>     jsonHandler $ makeObj [("nominees", JSArray . map showJSON $ nominees)]
>     --liftIO $ print "finished getNominees"

> genNominees :: ScoreFunc -> DateTime -> Maybe Minutes -> Maybe Minutes -> [Session] -> Scoring [JNominee]
> genNominees sf dt lower upper ss = do
>     durations <- bestDurations sf dt lower upper ss
>     return $ map toJNominee . take 30 . sortBy jNomOrder . filter (\(_, v, _) -> v > 1e-6) $ durations
>   where
>     jNomOrder (_, v, _) (_, v', _) = compare v' v
>     toJNominee (s, v, m) = defaultJNominee {
>                                 nSessName = Just . sName $ s
>                               , nSessType = Just . take 1 . show . sType $ s
>                               , nProjName = Just . pName . project $ s
>                               , nScore    = Just v
>                               , nScoreStr = Just . scoreStr $ v
>                               , nDuration = Just m
>                               , nDurStr   = Just . durationStr hr $ mn
>                                            }
>       where (hr, mn) = divMod m 60

> data JNominee = JNominee {
>       nSessName :: Maybe String
>     , nSessType :: Maybe String
>     , nProjName :: Maybe String
>     , nScore    :: Maybe Score
>     , nScoreStr :: Maybe String
>     , nDuration :: Maybe Minutes
>     , nDurStr   :: Maybe String
> } deriving Show

> defaultJNominee = JNominee {
>       nSessName = Nothing
>     , nSessType = Nothing
>     , nProjName = Nothing
>     , nScore    = Nothing
>     , nScoreStr = Nothing
>     , nDuration = Nothing
>     , nDurStr   = Nothing
> }

> instance JSON JNominee where
>     readJSON = jsonToJNominee
>     showJSON = jNomineeToJson

> jsonToJNominee _ = undefined

> jNomineeToJson :: JNominee -> JSValue
> jNomineeToJson nominee = makeObj $
>       concatMap field [
>           ("sess_name",      showJSON' . nSessName)
>         , ("sess_type",      showJSON' . nSessType)
>         , ("proj_name",      showJSON' . nProjName)
>         , ("score",          showJSON' . nScore)
>         , ("scoreStr",       showJSON' . nScoreStr)
>         , ("duration",       showJSON' . nDuration)
>         , ("durationStr",    showJSON' . nDurStr)
>       ]
>   where
>     field (name, accessor) = maybeToList . fmap ((,) name) . accessor $ nominee

> durationStr :: Int -> Int -> String
> durationStr h m = printf "%d:%02d" h m

> scoreStr :: Float -> String
> scoreStr s = printf "%.3f" s

> showJSON' :: JSON a => Maybe a -> Maybe JSValue
> showJSON' = fmap showJSON
