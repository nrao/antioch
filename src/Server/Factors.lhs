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

> module Server.Factors (
>     getFactorsHandler
>   , getFactors
>   ) where
 
> import Control.Monad.Trans                   (liftIO)
> import Control.Monad.State.Lazy              (StateT)
> import Database.HDBC.PostgreSQL              (Connection)
> import Network.Protocol.Http
> import Network.Salvia.Handlers.PathRouter    (hParameters)
> import Network.Salvia.Handlers.Error         (hError)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter)
> import Network.Salvia.Httpd
> import qualified Data.ByteString.Lazy.Char8 as L
> import Server.Json
> import Network.Salvia.Handlers.Redirect      (hRedirect)
> import Text.Printf
> import Maybe
> import Antioch.DateTime
> import Antioch.DSSData                       (getProjects, getSession)
> import Antioch.HardwareSchedule              (getReceiverSchedule)
> import Antioch.Score
> import Antioch.Settings                      (proxyListenerPort)
> import Antioch.Filters
> import Antioch.Types
> import Antioch.Utilities                     (rad2deg, rad2hrs)
> import Antioch.RunScores                     (runFactors)

> getFactorsHandler :: Connection -> Handler()
> getFactorsHandler cnn = hMethodRouter [
>       (GET,  getFactors cnn)
>     -- , (POST, getFactors)
>     ] $ hError NotFound

> getFactors :: Connection -> StateT Context IO ()
> getFactors cnn = do
>     params <- hParameters
>     -- Interpret options: id, start, tz, duration
>     let id       = read . fromJust . fromJust . lookup "id" $ params
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     -- timezone
>     let timezone = fromJust . fromJust . lookup "tz" $ params
>     -- start time
>     let startStr = fromJust . fromJust . lookup "start" $ params
>     -- start at ...
>     dt <- liftIO $ httpTzToDt startStr timezone
>     --              for duration.
>     let dur = read . fromJust . fromJust . lookup "duration" $ params
>     -- compute all factors with the given inputs 
>     projs <- liftIO getProjects
>     (s, scoresNfactors) <- liftIO $ runFactors id dt dur projs False
>     -- send the information back
>     jsonHandler $ makeObj [("ra", showJSON . floatStr . rad2hrs . ra $ s)
>                          , ("dec", showJSON . floatStr . rad2deg . dec $ s)
>                          , ("freq", showJSON . floatStr . frequency $ s)
>                          , ("xi", showJSON . floatStr . xi $ s)
>                          , ("type", showJSON . isSchedulableType dt dur $ s)
>                          , ("time", showJSON . hasTimeSchedulable dt undefined $ s)
>                          , ("not_complete", showJSON . isNotComplete dt undefined $ s)
>                          , ("enabled", showJSON . enabled $ s)
>                          , ("authorized", showJSON . authorized $ s)
>                          , ("observers", showJSON . hasObservers dt undefined $ s)
>                          , ("factors", factorsListToJSValue scoresNfactors)]

> floatStr :: Float -> String
> floatStr f = printf "%.2f" f

> floatFactor :: Maybe Score -> String
> floatFactor f = if isJust f then printf "%.7f" $ fromJust f else "0.0000"

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
>           ("name",  showJSON . fName $ jfactor)
>         , ("score", showJSON . floatFactor $ fScore jfactor)
>       ]

