Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

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
> import Antioch.RunScores
> import Control.OldException
> import Data.Either

> getNomineesHandler :: Handler()
> getNomineesHandler = hMethodRouter [
>       (GET,  getNominees)
>     --, (POST, getNominees)
>     ] $ hError NotFound

Example URL:
/nominees?start=2011-01-04+03%3A45%3A00&duration=165&timeBetween=false&minimum=false&blackout=false&backup=false&completed=false&rfi=false&tz=UTC&sortField=null&sortDir=NONE

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

>     -- compute the best durations (nominees)
>     result <- liftIO $ try $ getNominees' dt lower upper params
>     liftIO $ print ("getNominees' : ", result)
>     case result of
>         Left e -> jsonError e
>         Right x -> jsonSuccess x
>   where
>     wittyMsg = "Unexpected error encounted in service nominees: " 
>     jsonError e = jsonHandler $ makeObj [("error", showJSON (wittyMsg ++ (show e)))]
>     toNominees xs = map toJNominee . take 30 . sortBy jNomOrder . filter (\(_, v, _) -> v > 1e-6) $ xs
>     jsonSuccess x = jsonHandler $ makeObj $ objs x
>     objs x = [("nominees", JSArray . map showJSON $ toNominees x)]
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

> getNominees' :: DateTime -> Maybe Minutes -> Maybe Minutes -> [(String, Maybe String)] -> IO ([Nominee])
> getNominees' dt lower upper params = do
>     projs <- liftIO getProjects
>     runNominees dt lower upper params projs False
> 

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
