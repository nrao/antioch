> module Server.Score (
>     scoreHandler
>   ) where
 
> --import Control.Monad      (liftM)
> import Control.Monad.Trans                   (liftIO)
> import Control.Monad.State.Lazy              (StateT)
> --import Control.Monad.RWS.Strict
> --import Data.Record.Label
> import Data.List                             (find)
> import Data.Maybe                            (catMaybes)
> --import Database.HDBC
> import Database.HDBC.PostgreSQL              (Connection)
> import Network.Protocol.Http
> --import Network.Protocol.Uri
> import Network.Salvia.Handlers.PathRouter    (hParameters)
> import Network.Salvia.Handlers.PathRouter  (hPrefixRouter) -- trying
> import Network.Salvia.Handlers.Error         (hError)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter)
> import Network.Salvia.Httpd
> --import qualified Data.ByteString.Lazy.Char8 as L
> import Server.Json
> --import Server.List
> --import Antioch.Reports
> --import Network.Protocol.Uri                  (parseQueryParams)
> import Text.Printf
> import Maybe                                 (fromJust)
> import Antioch.DateTime
> import Antioch.DSSData                       (getProjects)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Filters
> import Antioch.Types
> import Antioch.Weather                       (getWeather, Weather)
> import Antioch.ReceiverTemperatures

> getPScore :: Connection -> StateT Context IO ()
> getPScore cnn = do
>     params <- hParameters
>     -- liftIO $ print params
>
>     -- Interpret options: pids
>     let spids = (catMaybes . map snd $ params)::[String]
>     let pids = urlToPids spids
>
>     -- get target period, start time, and scoring sessions
>     projs <- liftIO getProjects
>     let ss = concatMap sessions projs
>     let ps = concatMap periods ss
>
>     -- target periods
>     let tps = filter (\p -> (peId p) `elem` pids) ps
>     -- associated target sessions
>     let tss = map (\p -> (find (\s -> (sId . session $ p) == (sId s)) ss)) tps
>     -- target (period, session) sets
>     let tsps = catMaybes . map raise . zip tps $ tss
>
>     -- set up invariant part of the scoring environment
>     w <- liftIO $ getWeather Nothing
>     rt <- liftIO $ getReceiverTemperatures
>
>     -- compute scores
>     scores <- sequence $ map (liftIO . scorePeriod' ss w rt) tsps
>     let retvals = zip (map (peId . fst) tsps) scores
>     liftIO $ print retvals
>     jsonHandler $ makeObj [("scores", scoresListToJSValue retvals)]

http://trent.gb.nrao.edu:8002/score/session?duration=195&start=2010-03-16+11%3A45%3A00&sid=666

> getSScore :: Connection -> StateT Context IO ()
> getSScore cnn = do
>     params <- hParameters
>     liftIO $ print params
>     -- Interpret options: id, start, duration
>     let id       = read . fromJust . fromJust . lookup "sid" $ params
>     -- start at ...
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     let utc      = fromJust . parseUTCTime httpFormat $ startStr
>     let dt = toSeconds utc
>     -- duration
>     let dur = read . fromJust . fromJust . lookup "duration" $ params
>
>     -- get target session, and scoring sessions
>     projs <- liftIO getProjects
>     let ss = concatMap sessions projs
>     let sss = scoringSessions dt undefined ss
>     let s = head $ filter (\s -> (sId s) == id) ss
>     w <- liftIO $ getWeather Nothing
>     rt <- liftIO $ getReceiverTemperatures
>     rs <- liftIO $ getReceiverSchedule $ Just dt
>
>     score <- liftIO $ scoreSession dt dur s sss w rs rt
>     jsonHandler $ makeObj [("score", showJSON score)]

> scoresListToJSValue :: [(Int, Score)] -> JSValue
> scoresListToJSValue = JSArray . map scorePairToJson

> scorePairToJson :: (Int, Score) -> JSValue
> scorePairToJson pid_score = makeObj $
>       [
>           ("pid",   showJSON  . fst $ pid_score)
>         , ("score", showJSON  . snd $ pid_score)
>       ]

> scorePeriod' :: [Session] -> Weather -> ReceiverTemperatures -> (Period, Session) -> IO Score
> scorePeriod' ss w rt psp = do
>     let p = fst psp
>     let s = snd psp
>     let dt = startTime p
>     let sss = scoringSessions dt undefined ss
>     rs <- liftIO $ getReceiverSchedule . Just $ dt
>     scorePeriod p s sss w rs rt

> scoreHandler cnn = do
>     hPrefixRouter [
>           ("/periods",  getPScore cnn) 
>         , ("/session",  getSScore cnn) 
>       ] $ hError NotFound

> urlToPids :: [String] -> [Int]
> urlToPids ss = map read ss

> raise :: (Period, Maybe Session) -> Maybe (Period, Session)
> raise (p, ms)
>     | ms == Nothing   = Nothing
>     | otherwise       = Just (p, fromJust ms)
