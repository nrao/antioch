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

> module Antioch.ReceiverTemperaturesTests where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Antioch.ReceiverTemperatures
> import Maybe
> import Test.HUnit
> import System.IO.Unsafe (unsafePerformIO)

> tests = TestList [
>     test_receiverTemperatures
>   , test_nearestNeighbor
>                  ]

> test_receiverTemperatures = TestCase $ do
>   rt <- getReceiverTemperatures
>   ts <- temperatures rt Rcvr1_2
>   assertEqual "test_receiverTemperatures_1" (1.1,22.125) (head ts) 
>   t <- temperature rt Rcvr1_2 1.12 ts
>   assertEqual "test_receiverTemperatures_2" 22.125 t 
>   t <- temperature rt Rcvr1_2 1.119 ts
>   assertEqual "test_receiverTemperatures_3" 22.125 t 
>   t <- temperature rt Rcvr1_2 1.121 ts
>   assertEqual "test_receiverTemperatures_4" 22.125 t 
>   ts <- temperatures rt Rcvr4_6
>   assertEqual "test_receiverTemperatures_5" (3.9,21.07) (head ts) 
>   t <- temperature rt Rcvr4_6 6.0 ts
>   assertEqual "test_receiverTemperatures_6" 9.4 t

> test_nearestNeighbor = TestCase $ do
>   assertEqual "test_nearestNeighbor_1" 2.0 (nearestNeighbor 2.1 xs)
>   assertEqual "test_nearestNeighbor_2" 2.0 (nearestNeighbor 0.1 xs)
>   assertEqual "test_nearestNeighbor_3" 4.0 (nearestNeighbor 3.1 xs)
>   assertEqual "test_nearestNeighbor_4" 8.0 (nearestNeighbor 7.1 xs)
>   assertEqual "test_nearestNeighbor_5" 8.0 (nearestNeighbor 9.1 xs)
>     where
>   xs = [2.0, 4.0, 6.0, 8.0]
