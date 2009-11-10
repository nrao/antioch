> module Server.Nominees (
>     getNomineesHandler
>   , getNominees
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
> import Antioch.DSSData                       (getProjects)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Settings                      (proxyListenerPort)
> import Antioch.Simulate
> import Antioch.Types
> import Antioch.Utilities                     (readMinutes)
> import Antioch.Weather                       (getWeather)

> getNomineesHandler :: Handler()
> getNomineesHandler = hMethodRouter [
>       (GET,  getNominees)
>     , (POST, getNominees)
>     ] $ hError NotFound

> getNominees :: StateT Context IO ()
> getNominees = do
>     --liftIO $ print "starting getNominees"
>     params <- hParameters
>     --liftIO $ print "got params"
>     --liftIO $ print params
>
>     -- Interpret options:
>     -- timezone
>     let timezone = fromJust . fromJust . lookup "tz" $ params
>     -- start at ...
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     --liftIO $ print startStr
>     edt <- liftIO getCurrentTimeZone
>     let utc  | timezone == "ET" = localTimeToUTC edt . fromJust . parseLocalTime httpFormat $ startStr
>              | otherwise        = fromJust . parseUTCTime httpFormat $ startStr
>     --liftIO $ print utc
>     let dt = toSeconds utc
>     --liftIO $ print dt
>     -- duration of the hole (if one) ...
>     let upper = fmap readMinutes . fromJust . lookup "duration" $ params
>     --liftIO $ print upper
>     -- ignore minimum duration limit?
>     let lower = maybe Nothing (\v -> if v == "true" then Just 0 else Nothing) . fromJust . lookup "minimum" $ params
>     --liftIO $ print lower
>     -- ignore timeBetween limit?
>     -- ignore observer blackout times?
>     let timeBetween = fromJust . fromJust . lookup "timeBetween" $ params
>     let blackout = fromJust . fromJust . lookup "blackout" $ params
>     let sfs = catMaybes [if timeBetween == "true" then Nothing else Just enoughTimeBetween
>                        , if blackout == "true" then Nothing else Just observerAvailable]
>     --liftIO $ print sfs
>     -- use only backup sessions?
>     let backup = fromJust . fromJust . lookup "backup" $ params
>     -- include completed sessions?
>     let completed = fromJust . fromJust . lookup "completed" $ params
>     let filter = catMaybes . concat $ [
>             [Just isTypeOpen]
>           , if completed == "true" then [Nothing] else [Just hasTimeSchedulable, Just isNotComplete]
>           , [Just isSchedulableSemester]
>           , [Just isSchedulable]
>           , [Just hasObservers]
>           , if backup == "true" then [Just isBackup] else [Nothing]
>                                       ]
>     --liftIO $ print filter
>     let schedSessions = filterSessions dt filter
>     --let scoreSessions = filterSessions dt [isSchedulableSemester, isGradeA]
>
>     -- This is kind of awkward, the various selections the user may
>     -- specify must be implemented by sessions selection, scoring
>     -- factors, and/or period selection.
>     projs <- liftIO getProjects
>     let ss = concatMap sessions projs
>     w <- liftIO $ getWeather . Just $ dt 
>     rs <- liftIO $ getReceiverSchedule $ Just dt
>     nominees <- liftIO $ runScoring w rs $ do
>         sf <- genPartScore sfs . scoringSessions dt $ ss
>         genNominees sf dt lower upper . schedSessions $ ss
>     --liftIO $ print "returning stuff ..."
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
>                               , nProjName = Just . pName . project $ s
>                               , nScore    = Just v
>                               , nDuration = Just m
>                               , nDurStr   = Just . durationStr hr $ mn
>                                            }
>       where (hr, mn) = divMod m 60

> data JNominee = JNominee {
>       nSessName :: Maybe String
>     , nProjName :: Maybe String
>     , nScore    :: Maybe Score
>     , nDuration :: Maybe Minutes
>     , nDurStr   :: Maybe String
> } deriving Show

> defaultJNominee = JNominee {
>       nSessName = Nothing
>     , nProjName = Nothing
>     , nScore    = Nothing
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
>         , ("proj_name",      showJSON' . nProjName)
>         , ("score",          showJSON' . nScore)
>         , ("duration",       showJSON' . nDuration)
>         , ("durationStr",    showJSON' . nDurStr)
>       ]
>   where
>     field (name, accessor) = maybeToList . fmap ((,) name) . accessor $ nominee

> durationStr :: Int -> Int -> String
> durationStr h m = printf "%d:%02d" h m

> showJSON' :: JSON a => Maybe a -> Maybe JSValue
> showJSON' = fmap showJSON
