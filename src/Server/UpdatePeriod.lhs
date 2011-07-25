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

> module Server.UpdatePeriod (
>     updatePeriodHandler
>   ) where
 
> --import Control.Monad      (liftM)
> import Control.Monad.Trans                   (liftIO)
> import Control.Monad.State.Lazy              (StateT)
> --import Control.Monad.RWS.Strict
> --import Data.Record.Label
> import Data.List                             (find)
> import Data.Maybe                            (maybeToList, catMaybes, isJust)
> import Database.HDBC
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
> import Antioch.DSSData                       (getProjects, updatePeriodScore, updatePeriodMOC)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Filters
> import Antioch.Types
> import Antioch.Weather as W
> import Antioch.ReceiverTemperatures
> import Antioch.RunScores

> updatePeriodHandler :: Connection -> Handler ()
> updatePeriodHandler cnn = hMethodRouter [
>         (GET, updatePeriods cnn)
>     ] $ hError NotFound

Example URL:
http://trent.gb.nrao.edu:9051/update_periods?pids=6957&pids=6931&pids=6939

> updatePeriods :: Connection -> StateT Context IO ()
> updatePeriods cnn = do
>     liftIO $ print "updatePeriods"
> 
>     params <- hParameters
>     liftIO $ print params
>
>     -- Interpret options: pids
>     let spids = (catMaybes . map snd $ params)::[String]
>     let pids = urlToPids spids
>
>     -- get data from DB
>     projs <- liftIO getProjects
>
>     -- what time is it?
>     now <- liftIO $ getCurrentTime
>
>     -- compute the scores 
>     retvals <- liftIO $ runUpdatePeriods pids projs False
>     liftIO $ print ("runUpdatePeriods: ", retvals)
>
>     -- find the datetime for the forecast which feeds these scores
>     lit <- liftIO $ getRecentForecastTime
>     --liftIO . print . showJSON $ lit
>
>     -- determine if the forecast is current
>     let ft = maybe 0 id $ fromSqlString lit
>     let fresh = (diffMinutes ft now) < (8*60)
>     --liftIO . print $ fresh
>
>     -- write some results back to DB
>
>     -- get the (pids, scores) to update in the DB
>     let hscores = filter filterHistoricalScore retvals
>     liftIO $ print ("hscores to write to DB: ", hscores)
>
>     -- update them
>     liftIO $ mapM (updatePeriodScore' now) hscores
>
>     -- get the (pids, mocs) to update in the DB
>     let mocs = filter filterMOC retvals
>     liftIO $ print ("mocs to write to DB: ", mocs)
>
>     -- update them
>     liftIO $ mapM updatePeriodMOC' mocs
>
>     -- send them back
>     jsonHandler $ makeObj [("forecast", showJSON lit)
>                          , ("fresh",    showJSON fresh)
>                          , ("scores",   scoresListToJSValue retvals)]
>  where
>    filterHistoricalScore (_, _, hscore, _) = isJust hscore
>    filterMOC (_, _, _, moc) = isJust moc
>    updatePeriodScore' dt (pId, score, hscore, moc) = updatePeriodScore cnn pId dt (fromJust hscore)
>    updatePeriodMOC' (pId, score, hscore, moc) = updatePeriodMOC cnn pId moc

> getRecentForecastTime :: IO String
> getRecentForecastTime = do
>   cnn <- W.connect
>   rst <- liftIO $ quickQuery' cnn query []
>   return . fromSql . head . head $ rst
>     where
>       query = "SELECT date FROM import_times ORDER BY date DESC LIMIT 1"

> scoresListToJSValue :: [(Int, Score, Maybe Score, Maybe Bool)] -> JSValue
> scoresListToJSValue = JSArray . map scoresToJson

> scoresToJson :: (Int, Score, Maybe Score, Maybe Bool) -> JSValue
> scoresToJson (pid, cscore, hscore, moc) = makeObj $
>       [
>           ("pid",    showJSON pid)
>         , ("score",  showJSON cscore)
>         , ("hscore", showJSON hscore)
>         , ("moc",    showJSON moc)
>       ]

> urlToPids :: [String] -> [Int]
> urlToPids ss = map read ss
