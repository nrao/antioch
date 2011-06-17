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

> module Antioch.DebugTests where

> import Antioch.Debug
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score

> tests = TestList [test_getCanceledPeriods]

> test_getCanceledPeriods = TestCase $ do
>   assertEqual "test_getCanceledPeriods" ps canceled
>     where
>   start = fromGregorian 2006 2 1 0 0 0
>   dts = [(i*60) `addMinutes` start | i <- [0..10]]
>   ps = map mkPeriod dts
>   mkPeriod dt = Period 0 defaultSession dt 60 0.0 Pending undefined False 60
>   trace' = map (Cancellation) ps
>   trace'' = map (Timestamp) dts
>   trace = trace' ++ trace''
>   canceled = getCanceledPeriods trace     

