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

> module Antioch.SunRiseSetTests where

> import Antioch.SunRiseSet
> import Test.HUnit

> tests = TestList [test_ptcsSunRise
>                 , test_ptcsSunSet
>                 , test_sunRise
>                 , test_sunSet
>                  ]

As a check:
------------
PTCS PTCS
DOY Rise Set
-------------
001 14.6 1.1
031 14.4 1.6
061 13.8 2.1
091 13.1 2.6
121 12.4 3.1
151 12.0 3.6
181 12.0 3.8
211 12.3 3.5
241 12.8 2.8
271 13.2 2.1
301 13.7 1.3
331 14.2 0.9
361 14.6 1.0
------------

Common Sense test: Around April 1, 2010, the sun is coming up around 7:00 EST, 
or 11:00 UT.  So sunRise 91 should be at about this value, then PTCS adds 
about 2 hours to it.

> test_ptcsSunRise = TestCase $ do
>     assertEqual "test_sunRise_1" 14.564111 (ptcsSunRise 1)
>     assertEqual "test_sunRise_2" 14.439577 (ptcsSunRise 31)
>     assertEqual "test_sunRise_3" 13.874744 (ptcsSunRise 61)
>     assertEqual "test_sunRise_4" 13.0658455 (ptcsSunRise 91)
>     assertEqual "test_sunRise_5" 12.343692 (ptcsSunRise 121)
>     assertEqual "test_sunRise_6" 11.970525 (ptcsSunRise 151)
>     assertEqual "test_sunRise_7" 11.999554 (ptcsSunRise 181)
>     assertEqual "test_sunRise_8" 12.304111 (ptcsSunRise 211)
>     assertEqual "test_sunRise_9" 12.726834 (ptcsSunRise 241)
>     assertEqual "test_sunRise_10" 13.196716 (ptcsSunRise 271)
>     assertEqual "test_sunRise_11" 13.709338 (ptcsSunRise 301)
>     assertEqual "test_sunRise_12" 14.212818 (ptcsSunRise 331)
>     assertEqual "test_sunRise_13" 14.541542 (ptcsSunRise 361)

> test_ptcsSunSet = TestCase $ do
>     assertEqual "test_sunSet_1" 1.0886631 (ptcsSunSet 1)
>     assertEqual "test_sunSet_2" 1.5415993 (ptcsSunSet 31)
>     assertEqual "test_sunSet_3" 2.0935516 (ptcsSunSet 61)
>     assertEqual "test_sunSet_4" 2.6389828 (ptcsSunSet 91)
>     assertEqual "test_sunSet_5" 3.1469955 (ptcsSunSet 121)
>     assertEqual "test_sunSet_6" 3.5607853 (ptcsSunSet 151)
>     assertEqual "test_sunSet_7" 3.7386322 (ptcsSunSet 181)
>     assertEqual "test_sunSet_8" 3.5238285 (ptcsSunSet 211)
>     assertEqual "test_sunSet_9" 2.8931332 (ptcsSunSet 241)
>     assertEqual "test_sunSet_10" 2.040144 (ptcsSunSet 271)
>     assertEqual "test_sunSet_11" 1.2964287 (ptcsSunSet 301)
>     assertEqual "test_sunSet_12" 0.9386463 (ptcsSunSet 331)
>     assertEqual "test_sunSet_13" 1.0357571 (ptcsSunSet 361)

You can also check this code against various resources, including:
http://aa.usno.navy.mil/data/docs/RS_OneYear.php
Here the times are given in EST, which is = UTC - 5 hours.

> test_sunRise = TestCase $ do
>     assertEqual "test_sunRise_1" 12.564111 (sunRise 1)
>     assertEqual "test_sunRise_2" 9.999554  (sunRise 181)

> test_sunSet = TestCase $ do
>     assertEqual "test_sunSet_1" 22.088663 (sunSet 1)
>     assertEqual "test_sunSet_2" 0.7386322 (sunSet 181)
