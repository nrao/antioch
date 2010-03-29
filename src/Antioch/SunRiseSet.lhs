> module Antioch.SunRiseSet where

A mechanism suggested by Ron:
G = 2*pi*Day_of_year/365 in radians

A = 13.2178
B = 1.2754
C = -0.0915
D = 0.0673
E = 0.1573

L = 2.3553
M = -1.3304
N = 0.3406
O = 0.0525
P = 0.1520

PTCSSunRise in UT = A + B*cos(G) + C*sin(G) + D*cos(2*G) + E*sin(2*G)

PTCSSunSet in UT = L + M*cos(G) + N*sin(G) + O*cos(2*G) + P*sin(2*G)

SunRise = PTCSSunRise - 2
SunSet = (PTCSSunSet - 3 + 24) mudulo 24

Accurate to about 12 min. Only works for Green Bank.

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


This is a little bit tricky, since you'd think that PTCS would calculate
the true sun rise/set from a polynomyl, then apply these simple offsets.
But instead, it appears they have their own fancy polynomyl!

G = 2*pi*Day_of_year/365 in radians

> day_frac :: Int -> Float
> day_frac day = 2.0 * pi * ((fromIntegral day) / 365.0) 

PTCSSunRise in UT = A + B*cos(G) + C*sin(G) + D*cos(2*G) + E*sin(2*G)

> ptcsSunRise :: Int -> Float
> ptcsSunRise day = a + (b * cos g) + (c * sin g) + (d * cos  (2 * g)) + (e * sin (2 * g))
>   where 
>     g = day_frac day 
>     a = 13.2178
>     b = 1.2754
>     c = (-0.0915)
>     d = 0.0673::Float
>     e = 0.1573::Float

PTCSSunSet in UT = L + M*cos(G) + N*sin(G) + O*cos(2*G) + P*sin(2*G)

> ptcsSunSet :: Int -> Float
> ptcsSunSet day = l + (m * (cos g)) + (n * (sin g)) + (o * (cos (2 * g))) + (p * (sin (2 * g)))
>   where 
>     g = day_frac day 
>     l = 2.3553
>     m = (-1.3304)
>     n = 0.3406
>     o = 0.0525::Float
>     p = 0.1520::Float

> ptcsSunRiseOffset :: Float
> ptcsSunRiseOffset = 2.0 

> ptcsSunSetOffset :: Float
> ptcsSunSetOffset = 3.0


TBF: we probably shouldn't be calculating the actual sun rise/set this
way - we should get a more direct calculation.

PTCS considers the sun to have actually risen an offest *after* the sun 
has actually risen; this is to take into account the time that it takes for
the sun to actually warm up the GBT.

> sunRise day = unWrapHours $ (ptcsSunRise day) - ptcsSunRiseOffset

PTCS considers the sun to have actually set an offest *after* the sun 
has actually gone down; this is to take into account the time that it takes for
the GBT to cool down.

> sunSet day = unWrapHours $ (ptcsSunSet day) - ptcsSunSetOffset  

TBF: this is dumb, just handles (-48) to 24 hrs -> 0 - 24 hrs;

> unWrapHours :: Float -> Float
> unWrapHours hrs | hrs < 0.0 = hrs + 24.0 
>                 | otherwise = hrs



