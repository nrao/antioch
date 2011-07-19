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

> module Antioch.DBUtilities where

> import Antioch.Types
> import Database.HDBC
> import Database.HDBC.PostgreSQL

You've got a receiver, like Rcvr1_2, but what's it's Primary Key in the DB?

> getRcvrId :: Connection -> Receiver -> IO Int
> getRcvrId cnn rcvr = do
>     result <- quickQuery' cnn query xs
>     return $ fromSql . head . head $ result 
>   where
>     query = "SELECT id FROM receivers WHERE name = ?;"
>     xs = [toSql . show $ rcvr]

> getStrId :: Connection -> Int -> Int -> Int -> Int -> IO Int
> getStrId cnn rcvrId obsTypeId freq elev = do
>     result <- quickQuery' cnn query xs
>     return $ fromSql . head . head $ result
>   where
>     query = "SELECT id FROM stringency where receiver_id = ? and observing_type_id = ? and frequency = ? and elevation = ? order by id desc limit 1"
>     xs = [toSql rcvrId, toSql obsTypeId, toSql freq, toSql elev]
