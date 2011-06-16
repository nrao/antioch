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

> module Server.MinObsConditions (
>     getMOCHandler
>   , getMOC
>   ) where

> import Antioch.DateTime
> import Antioch.DSSData                      (getSession)
> import Antioch.HardwareSchedule             (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Weather                      (getWeather)
> import Antioch.ReceiverTemperatures
> import Antioch.RunScores
> import Control.Monad.State.Lazy             (StateT)
> import Control.Monad.Trans                  (liftIO)
> import Data.Maybe                           (maybeToList)
> import Database.HDBC
> import Database.HDBC.PostgreSQL              (Connection)
> import Maybe
> import Network.Protocol.Http
> import Network.Salvia.Handlers.Error        (hError)
> import Network.Salvia.Handlers.MethodRouter (hMethodRouter)
> import Network.Salvia.Handlers.PathRouter   (hParameters)
> import Network.Salvia.Httpd
> import Server.Json

> getMOCHandler :: Connection -> Handler()
> getMOCHandler cnn = hMethodRouter [
>       (GET, getMOC cnn)
>     ] $ hError NotFound

> getMOC :: Connection -> StateT Context IO ()
> getMOC cnn = do
>     -- parse params
>     params <- hParameters
>     liftIO $ print ("getMOC: ", params)
>     let start     = fromJust . fromHttpString $ getParam "start" params
>     let dur = read $ getParam "duration" params
>     let sessionId = read $ getParam "session_id" params
>     -- compute MOC
>     session <- liftIO $ getSession sessionId cnn
>     moc <- liftIO $ runMOC start dur session False
>     -- send result back
>     jsonHandler $ makeObj [("moc", showJSON . fromMaybe True $ moc)]
>   where
>     getParam p ps = fromJust . fromJust . lookup p $ ps

> data JMinObsCondition = JMinObsCondition {
>       moc :: Maybe Bool
> } deriving Show

> instance JSON JMinObsCondition where
>     readJSON = jsonToJMinObsCondition
>     showJSON = jMinObsConditionToJson

> jsonToJMinObsCondition _ = undefined

> jMinObsConditionToJson :: JMinObsCondition -> JSValue
> jMinObsConditionToJson conditions = makeObj $
>        [("moc", showJSON . moc $ conditions)]

> showJSON' :: JSON a => Maybe a -> Maybe JSValue
> showJSON' = fmap showJSON
