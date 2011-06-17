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

> module Antioch.RunHistWeatherOptTests where

> --import Antioch.Reports
> import Antioch.RunHistWeatherOpt
> import Antioch.Reports
> import Antioch.Schedule
> import Test.HUnit
> import Antioch.Types
> import Antioch.DateTime
> import Antioch.Score
> import Antioch.PProjects
> import Antioch.GenerateSchedule
> import Data.List
> import Data.Maybe


> tests = TestList [ test_reduceResult' ]

mode: 6 hrs, 3 true stability limits:
1: F
2: T
3: T
4: F
5: T
6: F

The stringency for this is simply 6/3 = 2.0;
but if we compute the stringency for the two halfs we get:
s1 = 3/2 = 1.5
s2 = 3/1 = 3.0
Those should recombine into a stringency of 2.0.

> test_reduceResult' = TestCase $ do
>   let strs = [[(0,0,0,0,1.5)], [(0,0,0,0,3.0)]]
>   let newStrs = reduceResult' 6 2 strs
>   let exp = [(0,0,0,0,2.0)]
>   assertEqual "test_reduceResult'_0" exp newStrs
>   -- now act like on hour 5 it's really False: 3/0 = Infinitiy
>   let strs = [[(0,0,0,0,1.5)], [(0,0,0,0,(3.0/0.0))]]
>   let newStrs = reduceResult' 6 2 strs
>   -- now it's 6/2 = 3.0
>   let exp = [(0,0,0,0,3.0)]
>   assertEqual "test_reduceResult'_1" exp newStrs
>   -- make sure it works with 3 cores
>   let strs = [[(0,0,0,0,2.0)], [(0,0,0,0,2.0)], [(0,0,0,0,2.0)]]
>   let newStrs = reduceResult' 6 3 strs
>   let exp = [(0,0,0,0,2.0)]
>   assertEqual "test_reduceResult'_2" exp newStrs


