> module Sessions where

> import Control.Monad.Trans                   (liftIO)
> import Data.Record.Label
> import Data.List                             (intercalate)
> import Data.Maybe                            (maybeToList)
> import Database.HDBC
> import Database.HDBC.PostgreSQL              (Connection)
> import Json
> import List
> import Network.Protocol.Http
> import Network.Protocol.Uri
> import Network.Salvia.Handlers.Error         (hError)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter)
> import Network.Salvia.Httpd
> import qualified Data.ByteString.Lazy.Char8 as L

> data Session = Session {
>     session_id   :: Int
>   , name         :: Maybe String
>   , project      :: Maybe String
>   , session_type :: Maybe String
>   , lst          :: Maybe Double
>   , dec          :: Maybe Double
>   , frequency    :: Maybe Double
>   , minDuration  :: Maybe Int
>   , maxDuration  :: Maybe Int
>   , timeBetween  :: Maybe Int
>   , allotted     :: Maybe Int
>   }

> defaultSession = Session {
>     session_id   = 0
>   , name         = Nothing
>   , project      = Nothing
>   , session_type = Nothing
>   , lst          = Nothing
>   , dec          = Nothing
>   , frequency    = Nothing
>   , minDuration  = Nothing
>   , maxDuration  = Nothing
>   , timeBetween  = Nothing
>   , allotted     = Nothing
>   }

> instance JSON Session where
>     readJSON = jsonToSession
>     showJSON = sessionToJson

> jsonToSession _ = undefined

> sessionToJson session = makeObj $
>       ("id", showJSON . session_id $ session)
>     : concatMap field [
>           ("name",         showJSON' . name)
>         , ("project",      showJSON' . project)
>         , ("session_type", showJSON' . session_type)
>         , ("lst",          showJSON' . lst)
>         , ("dec",          showJSON' . dec)
>         , ("frequency",    showJSON' . frequency)
>         , ("min_duration", showJSON' . minDuration)
>         , ("max_duration", showJSON' . maxDuration)
>         , ("time_between", showJSON' . timeBetween)
>         , ("allotted",     showJSON' . allotted)
>         ]
>   where
>     field (name, accessor) = maybeToList . fmap ((,) name) . accessor $ session

> showJSON' :: JSON a => Maybe a -> Maybe JSValue
> showJSON' = fmap showJSON

> sessionsHandler     :: Connection -> Handler ()
> sessionsHandler cnn = hMethodRouter [
>       (GET,  listSessions cnn)
>     , (POST, handlePost cnn)
>     ] $ hError NotFound

> listSessions cnn = do
>     rst <- liftIO $ quickQuery' cnn query []
>     let sessions = map buildSession rst
>     jsonHandler $ makeObj [("sessions", showJSON sessions)]
>   where
>     query           = "SELECT * FROM sessions"
>     buildSession xs = defaultSession {
>         session_id   = fromSql $ xs !! 0
>       , name         = fromSql $ xs !! 1
>       , project      = fromSql $ xs !! 2
>       , session_type = fromSql $ xs !! 3
>       , lst          = fromSql $ xs !! 4
>       , dec          = fromSql $ xs !! 5
>       , minDuration  = fromSql $ xs !! 6
>       , maxDuration  = fromSql $ xs !! 7
>       , timeBetween  = fromSql $ xs !! 8
>       , allotted     = fromSql $ xs !! 9
>       }

> handlePost cnn = do
>     bytes <- contents
>     let params = maybe [] id $ bytes >>= parseQueryParams . L.unpack
>     case lookup' Nothing "_method" params of
>         Nothing       -> newSession cnn params
>         Just "delete" -> deleteSession cnn params
>         Just "put"    -> saveSession cnn params

> newSession cnn params = do
>     [[n]] <- liftIO $ withTransaction cnn $ \cnn -> do
>         run cnn query []
>         quickQuery' cnn "SELECT CURRVAL('sessions_id_seq')" []
>     jsonHandler $ defaultSession { session_id = fromSql n }
>   where
>     query = "INSERT INTO sessions(name) VALUES('')"

> -- PUT /sessions/:id
> saveSession cnn params = do
>     '/' : sid <- getM $ path % uri % request
>     liftIO $ updateQuery cnn sid slots
>     jsonHandler $ makeObj [("success", showJSON "ok")]
>   where
>     getParam name = maybeToList . fmap ((,) name) . lookup' Nothing name $ params
>     slots = concatMap getParam ["name", "project", "session_type", "lst", "dec", "frequency", "min_duration", "max_duration", "time_between", "allotted"]

> updateQuery :: Connection -> String -> [(String, String)] -> IO ()
> updateQuery cnn key slots = withTransaction cnn $ \cnn -> do
>     sRun cnn query $ map (Just . snd) slots ++ [Just key]
>     return ()
>   where
>     query  = "UPDATE sessions SET " ++ query' ++ " WHERE id = ?"
>     query' = intercalate ", " $ [name ++ " = ?" | (name, _) <- slots]

> deleteSession _ _ = hError NotFound
