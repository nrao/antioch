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
