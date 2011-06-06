> module Server.Json (
>     module Text.JSON
>   , jsonHandler
>   , simpleToJson
>   , simpleToJson'
>   ) where

> import Data.Record.Label
> import Database.HDBC
> import Network.Protocol.Http  (contentType)
> import Network.Salvia.Httpd
> import Text.JSON

> jsonHandler      :: JSON t => t -> Handler ()
> jsonHandler json = do
>     -- Note: setting the content type here seems to cause the output to 
>     -- not be returned. but I suppose this isn't important since we're
>     -- getting along fine with out it.
>     --enterM response $ setM contentType ("text/plain", Just "utf-8")
>     sendStr $ encode json

> simpleToJson             :: String -> [String] -> [[SqlValue]] -> JSValue
> simpleToJson name fields =
>     simpleToJson' name fields . map (map fromSql)

> simpleToJson'                   :: String -> [String] -> [[String]] -> JSValue
> simpleToJson' name fields items = makeObj [(name, showJSON items')]
>   where
>     items' = map (makeObj . zip fields . map showJSON) items
