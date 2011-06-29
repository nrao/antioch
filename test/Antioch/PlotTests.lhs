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

> module Antioch.PlotTests where

> import Antioch.Plots
> import Test.HUnit

> tests = TestList [test_adjustHistData
>                  ]

> test_adjustHistData = TestCase $ do
>     assertEqual "test_adjustHistData 1" xy1  (adjustHistData xy1) 
>     assertEqual "test_adjustHistData 2" xy2' (adjustHistData xy2) 
>     assertEqual "test_adjustHistData 3" xy3' (adjustHistData xy3) 
>     assertEqual "test_adjustHistData 4" xy4' (adjustHistData xy4) 
>   where
>     xy1  = [(0.0,1.0)]
>     xy2  = [(0.0,1.0),  (1.0,3.0)]
>     xy2' = [(-0.5,1.0), (0.5,3.0)]
>     xy3  = [(0.0,1.0),  (1.0,3.0), (2.0, 2.0)]
>     xy3' = [(-0.5,1.0), (0.5,3.0), (1.5, 2.0)]
>     xy4  = [(0.0,1.0),  (1.0,3.0), (2.0, 2.0), (3.0, 0.0)]
>     xy4' = [(-0.5,1.0), (0.5,3.0), (1.5, 2.0), (2.5, 0.0)]
