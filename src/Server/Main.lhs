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
> import Server.MinObsConditions
> import Server.RunScheduler
> import Server.Nominees
> import Server.Score
> import Server.Factors
> import Maybe
> import Antioch.Settings                    (salviaListenerPort, dssDataDB)

> connect :: IO Connection
> connect = handleSqlError $ connectPostgreSQL cnnStr 
>   where
>     cnnStr = "dbname=" ++ dssDataDB ++ " user=dss"

> main = do
>     print "starting server"
>     addr <- inet_addr "0.0.0.0" --this is any client, "127.0.0.1" for a client running on local host
>     cfg  <- defaultConfig
>     let cfg' = cfg {
>         hostname   = "localhost"
>       , email      = "nubgames@gmail.com" -- TBF: should we change this?
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
>           ("/runscheduler",  runSchedulerHandler)  
>         , ("/nominees",      getNomineesHandler)  
>         , ("/score",         scoreHandler cnn)  
>         , ("/factors",       getFactorsHandler cnn)  
>         , ("/periods",       periodsHandler cnn) -- Example, not used
>         , ("/moc",           getMOCHandler cnn)  
>       ] $ hError NotFound
>     liftIO $ disconnect cnn   -- important? some of the above use pre-connected DSSData methods!?
