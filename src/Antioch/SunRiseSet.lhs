> module Antioch.SunRiseSet where

A mechanism suggested by Ron:
G = 2*pi*Day_of_year/365 in radians

A = 11.2178 (13.2178 for PTCS)
B = 1.2754
C = -0.0915
D = 0.0673
E = 0.1573

L = 23.3553 (2.3553 for PTCS)
M = -1.3304
N = 0.3406
O = 0.0525
P = 0.1520

SunRise in UT = A + B*cos(G) + C*sin(G) + D*cos(2*G) + E*sin(2*G)

SunSet in UT = [L + M*cos(G) + N*sin(G) + O*cos(2*G) + P*sin(2*G)] modulo 24

PTCSSunRise = SunRise + 2
PTCSSunSet = (SunSet + 3 + 24) mudulo 24

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

You can also check this code against various resources, including:
http://aa.usno.navy.mil/data/docs/RS_OneYear.php


G = 2*pi*Day_of_year/365 in radians

> day_frac :: Int -> Float
> day_frac day = 2.0 * pi * ((fromIntegral day) / 365.0) 

SunRise in UT = A + B*cos(G) + C*sin(G) + D*cos(2*G) + E*sin(2*G)

> sunRise :: Int -> Float
> sunRise day = a + (b * cos g) + (c * sin g) + (d * cos  (2 * g)) + (e * sin (2 * g))
>   where 
>     g = day_frac day 
>     a = 11.2178
>     b = 1.2754
>     c = (-0.0915)
>     d = 0.0673::Float
>     e = 0.1573::Float

SunSet in UT = L + M*cos(G) + N*sin(G) + O*cos(2*G) + P*sin(2*G) modulo 24

> sunSet :: Int -> Float
> sunSet day = unWrapHours $ l + (m * (cos g)) + (n * (sin g)) + (o * (cos (2 * g))) + (p * (sin (2 * g)))
>   where 
>     g = day_frac day 
>     l = 23.3553
>     m = (-1.3304)
>     n = 0.3406
>     o = 0.0525::Float
>     p = 0.1520::Float

PTCS sun rise/set times VERSION 1.0

> ptcsSunRiseOffset :: Float
> ptcsSunRiseOffset = 2.0 

> ptcsSunSetOffset :: Float
> ptcsSunSetOffset = 3.0


PTCS considers the sun to have actually risen an offest *after* the sun 
has actually risen; this is to take into account the time that it takes for
the sun to actually warm up the GBT.

> ptcsSunRise day = (sunRise day) + ptcsSunRiseOffset

PTCS considers the sun to have actually set an offest *after* the sun 
has actually gone down; this is to take into account the time that it takes for
the GBT to cool down.

> ptcsSunSet day = unWrapHours $ sunSet day + ptcsSunSetOffset  

TBF: this is dumb

> unWrapHours :: Float -> Float
> unWrapHours hrs | hrs > 24.0 = hrs - 24.0
>                 | otherwise  = hrs

PTCS sun rise/set times VERSION 2.0

> ptcsSunRiseOffset_V2 = 0.0
> ptcsSunSetOffset_V2  = 3.0
> ptcsSunRise_V2 day   = (sunRise day) + ptcsSunRiseOffset_V2
> ptcsSunSet_V2 day    = unWrapHours $ sunSet day + ptcsSunSetOffset_V2


