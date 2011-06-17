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

> module Antioch.HardwareScheduleTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.HardwareSchedule
> import Maybe
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)

> tests = TestList [
>     test_getReceiverSchedule
>   , test_getReceiverSchedule2
>      ]

> test_getReceiverSchedule = TestCase $ do
>     schd <- getReceiverSchedule $ Just dt
>     assertEqual "test_getReceiverSchedule" exp (take 2 schd)  
>       where
>         dt  = fromGregorian 2006 6 1 0 0 0
>         dt1 = fromGregorian 2006 6 1 0 0 0
>         dt2 = fromGregorian 2006 6 7 0 0 0
>         exp = [(dt1,[Rcvr1_2,Rcvr2_3]),(dt2,[Rcvr1_2,Rcvr4_6])]

> test_getReceiverSchedule2 = TestCase $ do
>     schd <- getReceiverSchedule $ Just dt
>     assertEqual "test_getReceiverSchedule2_1" exp (take 2 schd)  
>     schd <- getReceiverSchedule $ Just dt3
>     assertEqual "test_getReceiverSchedule2_2" ([exp!!1]) (take 1 schd)  
>     schd <- getReceiverSchedule Nothing 
>     assertEqual "test_getReceiverSchedule2_3" exp (take 2 schd)  
>     assertEqual "test_getReceiverSchedule2_4" 7 (length schd)  
>       where
>         dt  = fromGregorian 2006 6 2 0 0 0
>         dt1 = fromGregorian 2006 6 1 0 0 0
>         dt2 = fromGregorian 2006 6 7 0 0 0
>         dt3 = fromGregorian 2006 6 8 0 0 0
>         exp = [(dt1,[Rcvr1_2,Rcvr2_3]),(dt2,[Rcvr1_2,Rcvr4_6])]


