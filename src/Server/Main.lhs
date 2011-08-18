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

#! /usr/bin/env runhaskell

> module Main where

> import Control.Concurrent.STM              (atomically, newTVar)
> import Control.Monad.Trans                 (liftIO)
> -- import Database.HDBC                       (disconnect, handleSqlError)
> import Database.HDBC                       
> import Database.HDBC.PostgreSQL            (Connection, connectPostgreSQL)
> import Network.Salvia.Handlers.Default     (hDefault)
> import Network.Salvia.Handlers.Error       (hError)
> import Network.Salvia.Handlers.PathRouter  (hPrefixRouter)
> import Network.Salvia.Handlers.Session     (SessionHandler, mkSessions)
> import Network.Salvia.Handlers.Redirect    (hRedirect)
> import Network.Protocol.Http               (Status(..))
> import Network.Protocol.Uri              
> import Network.Salvia.Httpd
> import Network.Socket                      (inet_addr)
> import Server.JPeriods
> import Server.RunScheduler
> import Server.Nominees
> import Server.Factors
> import Server.UpdatePeriod
> import Maybe
> import Antioch.Settings                    (salviaListenerPort, dssDataDB, dssHost)

> connect :: IO Connection
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "host=" ++ dssHost ++ " dbname=" ++ dssDataDB ++ " user=dss"

> main = do
>     print "starting server"
>     addr <- inet_addr "0.0.0.0" --this is any client, "127.0.0.1" for a client running on local host
>     cfg  <- defaultConfig
>     let cfg' = cfg {
>         hostname   = "localhost"
>       , email      = "helpdesk-dss@gb.nrao.edu" -- How is this used?
>       , listenAddr = addr
>       , listenPort = salviaListenerPort 
>       }
>     mkHandler >>= start cfg'

> discardSession           :: Handler a -> SessionHandler () a
> discardSession handler _ = handler

> mkHandler = do
>     counter  <- atomically $ newTVar 0
>     sessions <- mkSessions
>     return $ hDefault counter sessions handler

> handler = discardSession $ do
>     cnn <- liftIO connect
>     hPrefixRouter [
>           ("/runscheduler",   runSchedulerHandler)  
>         , ("/nominees",       getNomineesHandler)  
>         , ("/factors",        getFactorsHandler cnn)  
>         , ("/update_periods", updatePeriodHandler cnn)
>       ] $ hError NotFound
>     liftIO $ disconnect cnn   
