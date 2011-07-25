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

> module Server.Score (
>     scoreHandler
>   ) where
 
> import Control.Monad.Trans                   (liftIO)
> import Control.Monad.State.Lazy              (StateT)
> import Data.List                             (find)
> import Data.Maybe                            (catMaybes)
> import Database.HDBC.PostgreSQL              (Connection)
> import Network.Protocol.Http
> import Network.Salvia.Handlers.PathRouter    (hParameters)
> import Network.Salvia.Handlers.PathRouter  (hPrefixRouter) -- trying
> import Network.Salvia.Handlers.Error         (hError)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter)
> import Network.Salvia.Httpd
> import Server.Json
> import Text.Printf
> import Maybe                                 (fromJust)
> import Database.HDBC
> import Antioch.DateTime
> import Antioch.DSSData                       (getProjects)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Filters
> import Antioch.Types
> import Antioch.Weather as W
> import Antioch.ReceiverTemperatures
> import Antioch.RunScores

Example URL:
http://trent.gb.nrao.edu:9051/score/periods?pids=6957&pids=6931&pids=6939

> getPScore :: Connection -> StateT Context IO ()
> getPScore cnn = do
>     params <- hParameters
>     liftIO $ print params
>
>     -- Interpret options: pids
>     let spids = (catMaybes . map snd $ params)::[String]
>     let pids = urlToPids spids
>
>     -- compute the scores 
>     projs <- liftIO getProjects
>     retvals <- liftIO $ runScorePeriods pids projs False
>
>     -- send them back
>     jsonHandler $ makeObj [("scores",   scoresListToJSValue retvals)]

> getRecentForecastTime :: IO String
> getRecentForecastTime = do
>   cnn <- W.connect
>   rst <- liftIO $ quickQuery' cnn query []
>   return . fromSql . head . head $ rst
>     where
>       query = "SELECT date FROM import_times ORDER BY date DESC LIMIT 1"

Example URL:
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
>     -- compute the score
>     projs <- liftIO getProjects
>     score <- liftIO $ runScoreSession id dt dur projs False
>     
>      -- send it back
>     jsonHandler $ makeObj [("score", showJSON score)]

> scoresListToJSValue :: [(Int, Score)] -> JSValue
> scoresListToJSValue = JSArray . map scorePairToJson

> scorePairToJson :: (Int, Score) -> JSValue
> scorePairToJson pid_score = makeObj $
>       [
>           ("pid",   showJSON  . fst $ pid_score)
>         , ("score", showJSON  . snd $ pid_score)
>       ]

> scoreHandler cnn = do
>     hPrefixRouter [
>           ("/periods",  getPScore cnn) 
>         , ("/session",  getSScore cnn) 
>       ] $ hError NotFound

> urlToPids :: [String] -> [Int]
> urlToPids ss = map read ss
