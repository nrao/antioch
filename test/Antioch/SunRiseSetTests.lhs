> module Antioch.SunRiseSetTests where

> import Antioch.SunRiseSet
> import Test.HUnit

> tests = TestList [test_ptcsSunRise, test_ptcsSunSet ]

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
>     assertEqual "test_sunSet_1" 1.088661 (ptcsSunSet 1)
>     assertEqual "test_sunSet_2" 1.5415978 (ptcsSunSet 31)
>     assertEqual "test_sunSet_3" 2.0935512 (ptcsSunSet 61)
>     assertEqual "test_sunSet_4" 2.6389816 (ptcsSunSet 91)
>     assertEqual "test_sunSet_5" 3.1469939 (ptcsSunSet 121)
>     assertEqual "test_sunSet_6" 3.5607843 (ptcsSunSet 151)
>     assertEqual "test_sunSet_7" 3.7386339 (ptcsSunSet 181)
>     assertEqual "test_sunSet_8" 3.5238287 (ptcsSunSet 211)
>     assertEqual "test_sunSet_9" 2.8931303 (ptcsSunSet 241)
>     assertEqual "test_sunSet_10" 2.0401423 (ptcsSunSet 271)
>     assertEqual "test_sunSet_11" 1.2964275 (ptcsSunSet 301)
>     assertEqual "test_sunSet_12" 0.9386457 (ptcsSunSet 331)
>     assertEqual "test_sunSet_13" 1.0357552 (ptcsSunSet 361)

